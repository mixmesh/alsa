%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    snd audio format (.au files)
%%% @end
%%% Created : 11 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_au).

-include("../include/alsa.hrl").
-include("alsa_au.hrl").

-export([read_header/1, write_header/2]).
-export([encode_header/1]).
-export([read_file_header/1]).
-export([to_snd/1, to_snd/2, from_snd/1]).
-export([poke_file_length/1]).  %% fixme: extended header?

to_snd(Format, _BitsPerChannel) ->	
    to_snd(Format).    
to_snd(Format) ->	
    case Format of
	?AU_ENCODING_MULAW_8 -> mu_law;
	?AU_ENCODING_LINEAR_8 -> s8;
	?AU_ENCODING_LINEAR_16 -> s16_be;
	?AU_ENCODING_LINEAR_24 -> s24_be;
	?AU_ENCODING_LINEAR_32 -> s32_be;
	?AU_ENCODING_FLOAT     -> float_be;
	?AU_ENCODING_DOUBLE    -> float64_be;
%% ?AU_ENCODING_ADPCM_G721  23).  %% 4-bit CCITT g.721 ADPCM 
%% ?AU_ENCODING_ADPCM_G722  24).  %% CCITT g.722 ADPCM 
%% ?AU_ENCODING_ADPCM_G723_3 25). %% CCITT g.723 3-bit ADPCM 
%% ?AU_ENCODING_ADPCM_G723_5 26). %% CCITT g.723 5-bit ADPCM 
	?AU_ENCODING_ALAW_8    -> a_law
    end.

from_snd(Format) ->
    case Format of
	mu_law -> ?AU_ENCODING_MULAW_8;
	s8 -> ?AU_ENCODING_LINEAR_8;
	s16_be ->?AU_ENCODING_LINEAR_16;
	s24_be -> ?AU_ENCODING_LINEAR_24;
	s32_be -> ?AU_ENCODING_LINEAR_32;
	float_be -> ?AU_ENCODING_FLOAT;
	float64_be -> ?AU_ENCODING_DOUBLE;
%% ?AU_ENCODING_ADPCM_G721  23).  %% 4-bit CCITT g.721 ADPCM 
%% ?AU_ENCODING_ADPCM_G722  24).  %% CCITT g.722 ADPCM 
%% ?AU_ENCODING_ADPCM_G723_3 25). %% CCITT g.723 3-bit ADPCM 
%% ?AU_ENCODING_ADPCM_G723_5 26). %% CCITT g.723 5-bit ADPCM 
	a_law -> ?AU_ENCODING_ALAW_8
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
    case file:read(Fd, 8) of
	{ok, <<?AU_MAGIC, DataOffset:32>>} ->
	    case file:read(Fd, 16) of
		{ok,
		 <<?AU_HEADER_FIELDS(DataSize,Encoding,Rate,Channels)>>} ->
		    ExtraLen = DataOffset - 24,
		    {ok,Annot} = file:read(Fd, ExtraLen),
		    SndFormat = to_snd(Encoding),
		    {ok, #{format=>SndFormat,
			   encoding=>Encoding,
			   channels=>Channels,
			   rate=>Rate,
			   annotation=>cstring(Annot),
			   data_size => DataSize }};
		{ok, _} -> {error, not_au};
		Err = {error,_} -> Err
	    end;
	{ok,_} -> {error, not_au};
	Err = {error,_} -> Err
    end.

write_header(Fd, Params) ->
    write_header(Fd, 0, Params).

write_header(Fd, DataSize, Params) ->
    Header = encode_header(DataSize, Params),
    file:write(Fd,
	       <<?AU_MAGIC, 16:32, Header/binary>>).
		
%% asssume Fd is at end of file, poke the length
poke_file_length(Fd) ->
    {ok, EOF} = file:position(Fd, cur),
    %% assume simple header is written 
    DataSize  = EOF - 24,
    file:position(Fd, {bof,8}),
    file:write(Fd, <<DataSize:32/little>>).

encode_header(Params) ->
    encode_header(0, Params).
encode_header(DataSize, Params) ->
    Encoding = from_snd(maps:get(format, Params)),
    Rate = from_snd(maps:get(rate, Params)),
    Channels = from_snd(maps:get(channels, Params)),
    <<?AU_HEADER_FIELDS(DataSize,Encoding,Rate,Channels)>>.

cstring(<<0,_/binary>>) -> [];
cstring(<<>>) -> [];
cstring(<<C,Cs/binary>>) -> [C | cstring(Cs)].

    
     
