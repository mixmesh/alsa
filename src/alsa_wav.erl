%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Generate / Read / Write wav/riff headers
%%% @end
%%% Created :  2 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_wav).

-compile(export_all).

-include("../include/alsa.hrl").
-include("alsa_wav.hrl").


to_snd(AudioFormat, BitsPerChannel) ->	
    case AudioFormat of
	?WAVE_FORMAT_PCM ->
	    case BitsPerChannel of
		8 -> u8;
		16 -> s16_le;
		32 -> s32_le;
		_ -> unknown
	    end;
	?WAVE_FORMAT_IEEE_FLOAT ->
	    case BitsPerChannel of
		32 -> float_le;
		64 -> float64_le;
		_ -> unkown
	    end;
	?WAVE_FORMAT_ALAW ->
	    a_law;
	?WAVE_FORMAT_ULAW ->
	    mu_law;
	_ -> unknown
    end.

from_snd(Format) ->
    case Format of
	u8     -> {?WAVE_FORMAT_PCM, 8};
	s16_le -> {?WAVE_FORMAT_PCM, 16};
	s32_le -> {?WAVE_FORMAT_PCM, 32};
	float_le -> {?WAVE_FORMAT_IEEE_FLOAT, 32};
	float64_le -> {?WAVE_FORMAT_IEEE_FLOAT, 64};
	a_law -> {?WAVE_FORMAT_ALAW, 8};
	mu_law -> {?WAVE_FORMAT_ULAW, 8}
    end.

read_tag(Fd) ->
    case file:read(Fd, 4) of
	{ok,<<Tag:4/binary>>} -> {ok,Tag};
	{ok,_} -> {error, bad_tag};
	eof -> {error, file_too_short};
	Err = {error,_} -> Err
    end.

read_taglen(Fd) ->
    case file:read(Fd, 8) of
	{ok,<<Tag:4/binary, Len:32/little>>} -> {ok,Tag,Len};
	{ok,_} -> {error, bad_tag};
	eof -> {error, file_too_short};
	Err = {error,_} ->
	    Err
    end.

read_file_header(Filename) ->
    case file:open(Filename, [read, binary]) of
	{ok, Fd} ->
	    try read_header(Fd) of
		Result -> Result
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

read_header(Fd) ->	
    case read_taglen(Fd) of
	{ok,?WAV_ID_RIFF,_FileLength} ->
	    case read_tag(Fd) of
		{ok, ?WAV_ID_WAVE} ->
		    case read_taglen(Fd) of
			{ok,?WAV_ID_FMT, HdrLen} ->
			    read_header_(Fd, (HdrLen+1) band -2);
			{ok,_,_} -> {error, not_wav};
			Err = {error,_} -> Err
		    end;
		{ok, _} -> {error, not_wav};
		Err = {error,_} -> Err
	    end;
	{ok,_,_} -> {error, not_wav};
	Err = {error,_} -> Err
    end.

read_header_(Fd, HdrLen) ->
    case file:read(Fd, HdrLen) of
	{ok, Bin} -> decode_header(Bin);
	eof -> {error, file_too_short};
	Error -> Error
    end.

write_header(Fd, Header) ->
    write_header(Fd, 0, Header).


write_tag(Fd, Tag) when byte_size(Tag) =:= 4 ->
    case file:write(Fd, Tag) of
	ok -> ok;
	Err = {error,_} -> Err
    end.

write_taglen(Fd, Tag, Len) when byte_size(Tag) =:= 4, is_integer(Len), Len>=0 ->
    case file:write(Fd, <<Tag/binary, Len:32/little>>) of
	ok -> ok;
	Err = {error,_} -> Err
    end.

write_header(Fd, FileLength, Header) ->
    case write_taglen(Fd, ?WAV_ID_RIFF, FileLength) of
	ok ->
	    case write_tag(Fd, ?WAV_ID_WAVE) of
		ok ->
		    case write_taglen(Fd, ?WAV_ID_FMT, 2+2+4+4+2+2) of
			ok ->
			    write_header_(Fd, Header);
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.
		
write_header_(Fd, Header) ->
    case file:write(Fd, encode_header(Header)) of
	ok -> ok;
	Err = {error,_} -> Err
    end.
	    
    
encode_header(#{ format := Format,
		 channels := NumChannels,
		 rate := SampleRate
	       }) ->
    {AudioFormat, BitsPerChannel} = from_snd(Format),
    ByteRate = SampleRate*(((NumChannels*BitsPerChannel)+7) div 8),
    <<?WAV_HEADER_FIELDS(AudioFormat,
			 NumChannels,
			 SamplesRate,
			 ByteRate,
			 0,
			 BitsPerChannel)>>.

decode_header(Bin) ->
    case Bin of
	<<?WAV_HEADER_FIELDS(AudioFormat,
			     NumChannels,
			     SamplesRate,
			     ByteRate,
			     _BlockAlign,
			     BitsPerChannel),
	  XBin/binary>> ->
	    if AudioFormat =:= ?WAVE_FORMAT_EXTENSIBLE ->
		    case XBin of
			<<?XWAV_HEADER_FIELDS(_CbSize,
					      ValidBitsPerChannel,
					      ChannelMask,
					      AudioFormat1),
			  _/binary>> ->
			    SndFormat = to_snd(AudioFormat1, BitsPerChannel),
			    {ok,
			     #{ format => SndFormat,
				audio_format => AudioFormat1,
				channels => NumChannels,
				bits_per_channel => BitsPerChannel,
				valid_bit_per_channel => ValidBitsPerChannel,
				channel_mask => ChannelMask,
				rate => SampleRate,
				byte_rate => ByteRate
			      }};
			_ ->
			    {error, too_short}
		    end;
	       true ->
		    SndFormat = to_snd(AudioFormat, BitsPerChannel),
		    {ok, #{ format => SndFormat,
			    audio_format => AudioFormat,
			    channels => NumChannels,
			    bits_per_channel => BitsPerChannel,
			    rate => SampleRate,
			    byte_rate => ByteRate
			  }}
	    end;
	<<_/binary>> ->
	    {error, header_too_short}
    end.

