%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Generate / Read / Write wav/riff headers
%%% @end
%%% Created :  2 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_wav).

-include("../include/alsa.hrl").
-include("alsa_wav.hrl").

-export([read_header/1, write_header/2]).
-export([read_data_tag/1]).
-export([read_file_info/1, read_info/1]).
-export([encode_header/1]).
-export([read_file_header/1]).
-export([to_snd/4, from_snd/1]).
-export([poke_file_length/1]).  %% fixme: extended header?

-define(dbg(F,A), ok).
%% -define(dbg(F,A), io:format((F), (A))).

-define(ALIGN(Len), (((Len)+1) band -2)).
-define(PAD(Len),  (?ALIGN(Len)-(Len))).

to_snd(AudioFormat, BitsPerChannel, NumChannels, FrameSize) ->	
    case AudioFormat of
	?WAVE_FORMAT_PCM ->
	    case BitsPerChannel of
		8 -> u8;
		16 -> s16_le;
		24 ->
		    SampleSize = FrameSize div NumChannels,
		    if SampleSize =:= 3 ->
			    s24_3le;
		       SampleSize =:= 4 ->
			    s24_le
		    end;
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
	s24_le -> {?WAVE_FORMAT_PCM, 24};
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



-spec read_header(Fd::file:io_device()) ->
	  {ok, map()} | {error, term()}.

read_header(Fd) ->
    case read_taglen(Fd) of
	{ok,?WAV_ID_RIFF,_FileLength} ->
	    ?dbg("FileLength = ~w\n", [_FileLength + 8]),
	    case read_tag(Fd) of
		{ok, ?WAV_ID_WAVE} ->
		    ?dbg("Tag ~p found\n", [?WAV_ID_WAVE]),
		    read_header_fmt(Fd);
		{ok, _Tag} ->
		    ?dbg("error: Tag ~p found\n", [_Tag]),
		    {error, not_wav};
		Err = {error,_} -> Err
	    end;
	{ok,_,_} -> {error, not_wav};
	Err = {error,_} -> Err
    end.

%% skip until data tag, return length
read_data_tag(Fd) ->
    case find_tag_(Fd, ?WAV_ID_DATA) of
	{ok, _Tag, Len} -> {ok, Len};
	false -> {error, no_data_found};
	Error -> Error
    end.

read_file_info(Filename) ->
    case file:open(Filename, [read, binary]) of
	{ok, Fd} ->
	    try read_info(Fd) of
		Info -> Info
	    after
		file:close(Fd)
	    end;
	Error -> Error
    end.

-spec read_info(Fd::file:io_device()) ->
	  {ok, map()} | {error, term()}.

read_info(Fd) ->
    case read_taglen(Fd) of
	{ok,?WAV_ID_RIFF,_FileLength} ->
	    ?dbg("FileLength = ~w\n", [_FileLength + 8]),
	    case read_tag(Fd) of
		{ok, ?WAV_ID_WAVE} ->
		    ?dbg("Tag ~p found\n", [?WAV_ID_WAVE]),
		    read_info_(Fd);
		{ok, _Tag} ->
		    ?dbg("error: Tag ~p found\n", [_Tag]),
		    {error, not_wav};
		Err = {error,_} -> Err
	    end;
	{ok,_,_} -> {error, not_wav};
	Err = {error,_} -> Err
    end.

read_info_(Fd) ->
    case find_tag_(Fd, ?WAV_ID_LIST) of
	{ok, _Tag, Len} ->
	    case file:read(Fd, ?ALIGN(Len)) of
		{ok, Bin} ->
		    decode_info(Bin);
		eof -> {error, file_too_short};
		Err = {error,_} -> Err
	    end;
	false ->
	    #{};
	Error -> Error
    end.

decode_info(<<Tag:4/binary, Binary/binary>>) when Tag =:= ?WAV_LIST_INFO ->
    decode_info(Binary, #{});
decode_info(<<>>) ->
    #{}.

decode_info(<<Tag:4/binary, Len:32/little, _:?PAD(Len)/binary, 
	      InfoData:Len/binary, Data/binary>>, Info) ->
    decode_info(Data, Info#{ Tag => ascii(InfoData) });
decode_info(<<>>, Info) ->
    Info.

ascii(<<>>) -> [];
ascii(<<0,_/binary>>) -> [];
ascii(<<C,Cs/binary>>) -> [C|ascii(Cs)].

skip_tags_in_binary(<<Tag1:4/binary, Len1:32/little, _:?PAD(Len1)/binary, 
		    InfoData:Len1/binary, Data/binary>>, Tag) ->
    if Tag =:= Tag1 ->
	    InfoData;
       true ->
	    skip_tags_in_binary(Data, Tag)
    end;
skip_tags_in_binary(<<>>, _Tag) ->
    <<>>.


read_header_fmt(Fd) ->
    case read_taglen(Fd) of
	{ok,?WAV_ID_FMT, HdrLen} ->
	    ?dbg("Tag ~p len=~p\n", [?WAV_ID_FMT, HdrLen]),
	    case read_header_(Fd, ?ALIGN(HdrLen)) of
		{ok,Header, _Tail} ->
		    ?dbg("Header tail=~p\n", [_Tail]),
		    {ok, Pos} = file:position(Fd, cur),
		    case find_tag_(Fd, ?WAV_ID_DATA) of
			{ok, ?WAV_ID_DATA, _DataLength} ->
			    ?dbg("DataLength = ~w\n", [_DataLength]),
			    {ok,Header#{ data_length => _DataLength}};
			_ ->
			    file:position(Fd, Pos),
			    {ok,Header}
			    %%{error, missing_data}
		    end;
		Err = {error,_} -> Err
	    end;
	{ok,_Tag,HdrLen} -> %% skip 
	    ?dbg("skip: Tag ~p len=~p\n", [_Tag, HdrLen]),
	    {ok,_Pos} = file:position(Fd, {cur, ?ALIGN(HdrLen)}),
	    ?dbg("skipped: pos = ~p\n", [_Pos]),
	    read_header_fmt(Fd);
	{ok, _} -> {error, not_wav};
	Err = {error,_} -> Err
    end.

read_header_(Fd, HdrLen) ->
    case file:read(Fd, HdrLen) of
	{ok, Bin} -> decode_header(Bin);
	eof -> {error, file_too_short};
	Error -> Error
    end.

find_tag_(Fd, Tag) ->
    case read_taglen(Fd) of
	{ok,Tag,Len} ->
	    {ok,Tag,Len};
	{ok,_Tag,HdrLen} ->
	    ?dbg("skip: Tag ~p len=~p\n", [_Tag, HdrLen]),
	    {ok,_Pos} = file:position(Fd, {cur, ?ALIGN(HdrLen)}),
	    ?dbg("skipped: pos = ~p\n", [_Pos]),
	    find_tag_(Fd, Tag);
	{error, file_too_short} ->
	    false;
	Err = {error, _} -> Err
    end.

write_header(Fd, Params) when is_map(Params) ->
    write_header(Fd, 28, Params).


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

write_header(Fd, FileLength, Params) when is_map(Params) ->
    case write_taglen(Fd, ?WAV_ID_RIFF, FileLength) of
	ok ->
	    case write_tag(Fd, ?WAV_ID_WAVE) of
		ok ->
		    case write_taglen(Fd, ?WAV_ID_FMT, 2+2+4+4+2+2) of
			ok ->
			    case write_header_(Fd, Params) of
				ok ->
				    case write_taglen(Fd, ?WAV_ID_DATA,
						      FileLength-24) of
					ok -> ok;
					Error -> Error
				    end;
				Error -> Error
			    end;
			Error -> Error
		    end;
		Error -> Error
	    end;
	Error -> Error
    end.
		
write_header_(Fd, Params) when is_map(Params)  ->
    case file:write(Fd, encode_header(Params)) of
	ok -> ok;
	Err = {error,_} -> Err
    end.

-define(FILE_LENGTH_OFFSET, 4).
-define(DATA_LENGTH_OFFSET, 40).  %% of 50 for extended header
-define(WAV_HEADER_LEN, 16).
-define(XWAV_HEADER_LEN, (?WAV_HEADER_LEN+10)).

%% asssume Fd is at end of file, poke the length
poke_file_length(Fd) ->
    {ok, EOF} = file:position(Fd, cur),
    %% assume simple header is written 
    Len  = EOF - 8,
    file:position(Fd, {bof,?FILE_LENGTH_OFFSET}),
    file:write(Fd, <<Len:32/little>>),
    file:position(Fd, {bof,?DATA_LENGTH_OFFSET}),
    %% remove wav header length(16) "WAVE"(4) "fmt "(4)+len(4) "data"(4) +len(4)
    file:write(Fd, <<(Len-(?WAV_HEADER_LEN+20)):32/little>>).
    

    
encode_header(Params) when is_map(Params) ->
    Format = maps:get(format, Params),
    NumChannels = maps:get(channels, Params),
    Rate = maps:get(rate, Params),
    {AudioFormat, BitsPerChannel} = from_snd(Format),
    ByteRate = Rate*(((NumChannels*BitsPerChannel)+7) div 8),
    FrameSize = (BitsPerChannel*NumChannels+7) div 8,
    <<?WAV_HEADER_FIELDS(AudioFormat,
			 NumChannels,
			 Rate,
			 ByteRate,
			 FrameSize,
			 BitsPerChannel)>>.

decode_header(Bin) ->
    case Bin of
	<<?WAV_HEADER_FIELDS(AudioFormat,
			     NumChannels,
			     Rate,
			     ByteRate,
			     FrameSize,
			     BitsPerChannel),
	  XBin/binary>> ->
	    if AudioFormat =:= ?WAVE_FORMAT_EXTENSIBLE ->
		    case XBin of
			<<?XWAV_HEADER_FIELDS(_CbSize,
					      ValidBitsPerChannel,
					      ChannelMask,
					      AudioFormat1),
			  XBin1/binary>> ->
			    SndFormat = to_snd(AudioFormat1, BitsPerChannel,
					       NumChannels, FrameSize),
			    {ok,
			     #{format=>SndFormat,
			       audio_format=>AudioFormat1,
			       channels=>NumChannels,
			       bits_per_channel=>BitsPerChannel,
			       valid_bit_per_channel=>ValidBitsPerChannel,
			       channel_mask=>ChannelMask,
			       rate=>Rate,
			       frame_size=>FrameSize,
			       byte_rate=>ByteRate}, XBin1};
			_ ->
			    {error, too_short}
		    end;
	       true ->
		    SndFormat = to_snd(AudioFormat, BitsPerChannel,
				       NumChannels, FrameSize),
		    {ok, #{format=>SndFormat,
			   audio_format=>AudioFormat,
			   channels=>NumChannels,
			   bits_per_channel=>BitsPerChannel,
			   rate=>Rate,
			   frame_size=>FrameSize,
			   byte_rate=>ByteRate }, XBin}
	    end;
	<<_/binary>> ->
	    {error, header_too_short}
    end.

