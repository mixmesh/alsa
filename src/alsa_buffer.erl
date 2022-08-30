%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Sample buffer
%%% @end
%%% Created : 29 Aug 2022 by Tony Rogvall <tony@rogvall.se>

-module(alsa_buffer).

-export([new/0, new/1]).
-export([read/2, read_samples/2]).
-export([skip/2, skip_samples/2]).
-export([insert/2, insert_file/2]).
-export([append/2, append_file/2]).
-export([setopts/2, getopts/2]).
-export([reset/1, clear/1]).
-export([silence/2]).

-define(MAX_DATA_SIZE, (10*1024*1024)).
-define(DEFAULT_FORMAT,   s16_le).
-define(DEFAULT_CHANNELS, 1).
-define(DEFAULT_RATE,     16000).
-define(DEFAULT_LOOP,     false).
-define(DEFAULT_MUTED,    false).

-type unsigned() :: non_neg_integer().

-record(sample_buffer,
	{
	 format = ?DEFAULT_FORMAT  :: alsa:format(),  %%
	 channels = ?DEFAULT_CHANNELS :: unsigned(),     %%
	 rate = ?DEFAULT_RATE         :: unsigned(),     %% Hz
	 loop = ?DEFAULT_LOOP :: boolean(),      %% start from beginning at end
	 muted = ?DEFAULT_MUTED :: boolean(),      %% step but generate silence
	 bytes_per_sample=2 :: unsigned(),     %% frame size in bytes
	 pos = 0 :: integer(),                 %% current pos in current buffer
	 buf = <<>> :: binary()                %% sample buffer
	}).

-type sample_buffer() :: #sample_buffer{}.

new() -> new([]).

new(Opts) when is_list(Opts) ->
    new(maps:from_list(Opts));
new(Opts) when is_map(Opts) ->
    Format   = maps:get(format, Opts, s16_le),
    Channels = maps:get(channels, Opts, 1),
    Rate     = maps:get(rate, Opts, 16000),
    Loop     = maps:get(loop, Opts, false),
    Muted    = maps:get(muted, Opts, false),
    Size = alsa:format_size(Format, 1)*Channels,
    #sample_buffer{format=Format,
		   channels=Channels,
		   rate=Rate,
		   loop = Loop,
		   muted = Muted,
		   bytes_per_sample=Size}.

setopts(Cb, [{Key,Value}|Opts]) ->
    case Key of
	format when is_atom(Value) ->
	    Size = alsa:format_size(Value, 1)*Cb#sample_buffer.channels,    
	    setopts(Cb#sample_buffer{format=Value, 
				     bytes_per_sample=Size}, Opts);
	channels when is_integer(Value), Value>0 ->
	    Size = alsa:format_size(Cb#sample_buffer.format, 1)*Value,
	    setopts(Cb#sample_buffer{channels=Value, 
				     bytes_per_sample=Size}, Opts);
	rate when is_integer(Value), Value>0 ->
	    setopts(Cb#sample_buffer{rate=Value}, Opts);
	muted when is_boolean(Value) ->
	    setopts(Cb#sample_buffer{muted=Value}, Opts);
        loop when is_boolean(Value) ->
	    setopts(Cb#sample_buffer{loop=Value}, Opts)
    end;
setopts(Cb, []) ->
    Cb.

getopts(Cb, [Key|Opts]) ->
    Value = case Key of
		format -> Cb#sample_buffer.format;
		channels -> Cb#sample_buffer.channels;
		rate -> Cb#sample_buffer.rate;
		muted -> Cb#sample_buffer.muted;
		loop -> Cb#sample_buffer.loop;
		pos -> Cb#sample_buffer.pos;
		bytes_per_sample -> Cb#sample_buffer.bytes_per_sample
	    end,
    [{Key,Value}|getopts(Cb,Opts)];
getopts(_Cb, []) ->
    [].

reset(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{pos=0 }.

clear(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{pos=0, buf = <<>> }.

read_samples(Cb=#sample_buffer{bytes_per_sample=Z}, N) ->
    read(Cb, N*Z).

read(Cb=#sample_buffer{pos=Pos,buf=Buf,muted=true}, N) when
      is_integer(N), N >= 0 ->
    BufSize = byte_size(Buf),
    Pos1 = Pos + N,
    if Cb#sample_buffer.loop ->
	    {silence(Cb,N), Cb#sample_buffer{pos=Pos1 rem BufSize}};
       true ->
	    {silence(Cb,N),Cb#sample_buffer{pos=min(BufSize, Pos1)}}
    end;
read(Cb=#sample_buffer{pos=Pos, buf=Buf}, N) when
      is_integer(N), N >= 0 ->
    case Buf of
	<<_:Pos/binary, Data:N/binary, _/binary>> ->
	    Pos1 = Pos+N,
	    {Data, Cb#sample_buffer{pos=Pos1}};
	<<_:Pos/binary, Data/binary>> when byte_size(Data) > 0 ->
	    Pos1 = Pos + byte_size(Data),
	    {Data1, Cb1} = read(Cb#sample_buffer{pos=Pos1},N-byte_size(Data)),
	    {<<Data/binary, Data1/binary>>, Cb1};
	<<_:Pos/binary>> ->
	    if Cb#sample_buffer.loop ->
		    read(Cb#sample_buffer{pos=0}, N);
	       true ->
		    {silence(Cb,N), Cb}
	    end
    end.

skip_samples(Cb=#sample_buffer{bytes_per_sample=Z}, N) ->
    skip(Cb, N*Z).

-spec skip(Cb::sample_buffer(), N::integer()) -> Cb1::sample_buffer().

skip(Cb=#sample_buffer{pos=Pos, buf=Buf}, N) when is_integer(N), N >= 0 ->
    case Buf of
	<<_:Pos/binary, _:N/binary, _/binary>> ->
	    Cb#sample_buffer{pos=Pos+N};
	<<_:Pos/binary, Data/binary>> when byte_size(Data) > 0 ->
	    Pos1 = Pos + byte_size(Data),
	    skip(Cb#sample_buffer{pos=Pos1},N-byte_size(Data));
	<<_:Pos/binary>> ->
	    if Cb#sample_buffer.loop ->
		    skip(Cb#sample_buffer{pos=0}, N);
	       true ->
		    Cb
	    end
    end.

%% write/append samples to sample buffer
insert(Cb=#sample_buffer{pos=Pos,buf=Buf}, Data) when is_binary(Data) ->
    <<Buf0:Pos/binary,Buf1/binary>> = Buf,
    Cb#sample_buffer{buf = <<Buf0/binary,Data/binary,Buf1/binary>>}.

%% write samples from a file into the sample_buffer
insert_file(Cb, Filename) ->
    case read_file(Filename, ?MAX_DATA_SIZE) of    
	{ok,{Header,Data}} ->
	    Data1 = transform_data(Cb, Header, Data),
	    insert(Cb, Data1);
	Error ->
	    io:format("inster_file: error ~p\n", [Error]),
	    Cb
    end.

%% write/append samples to sample buffer
append(Cb=#sample_buffer{buf=Buf}, Data) when is_binary(Data) ->
    Cb#sample_buffer{buf = <<Buf/binary,Data/binary>>}.

%% write samples from a file into the sample_buffer
append_file(Cb, Filename) ->
    case read_file(Filename, ?MAX_DATA_SIZE) of
	{ok,{Header,Data}} ->
	    Data1 = transform_data(Cb, Header, Data),
	    append(Cb, Data1);
	Error ->
	    io:format("append_file: error ~p\n", [Error]),
	    Cb
    end.

%% FIXME: match format with existing?
%% reformat new data?
read_file(Filename, MaxDataSize) ->
    case file:open(Filename, [read, binary]) of
	{ok, Fd} ->
	    try alsa_playback:read_header(Fd) of
		Header ->
		    Format = maps:get(format, Header),
		    Channels = maps:get(channels, Header),
		    Size = alsa:format_size(Format, 1)*Channels,
		    case read_file_data(Fd, MaxDataSize, Size) of
			{ok,Data} ->
			    {ok,{Header, Data}};
			Error ->
			    Error
		    end
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

read_file_data(Fd, MaxDataSize, Align) ->
    {ok,Cur} = file:position(Fd, cur),
    {ok,End} = file:position(Fd, eof),
    Size = End - Cur,
    AlignedDataSize = align_down(Size, Align),
    AlignedMaxDataSize = align_up(MaxDataSize, Align),
    DataSize = min(AlignedMaxDataSize, AlignedDataSize),
    file:position(Fd, Cur),
    file:read(Fd, DataSize).

%% align X to nearest lower multiple of align
align_down(X, Align) ->
    X - (X rem Align).

%% align X to nearest greater multiple of Align
align_up(X, Align) ->
    X + ((Align - (X rem Align)) rem Align).
    
silence(#sample_buffer{format=Format, channels=Channels}, N) ->
    alsa:make_silence(Format, Channels, N).

transform_data(Cb, Header, Data) ->
    Rate = maps:get(rate, Header),
    Format = maps:get(format, Header),
    Channels = maps:get(channels, Header),
    Data1 = alsa_util:reformat(Format, Cb#sample_buffer.format,
			       Channels, Cb#sample_buffer.channels,
			       Data),
    Data2 = if Rate =/= Cb#sample_buffer.rate ->
		    alsa_util:resample(Rate, Cb#sample_buffer.rate,
				       Cb#sample_buffer.format,
				       Cb#sample_buffer.channels, Data1);
	       true ->
		    Data1
	    end,
    Data2.

    
    
