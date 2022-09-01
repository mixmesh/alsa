%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Sample buffer
%%% @end
%%% Created : 29 Aug 2022 by Tony Rogvall <tony@rogvall.se>

-module(alsa_buffer).

-export([new/0, new/1, new/2]).
-export([read/2, read_samples/2, read_samples_and_marks/2]).
-export([skip/2, skip_samples/2]).
-export([delete_samples/2, copy_samples/2]).
-export([insert/2, insert/3, insert/4]).
-export([insert_file/2, insert_file/3]).
-export([append/2, append/3]).
-export([append_file/2]).
-export([setopts/2, getopts/2]).
-export([get_position/1, get_position/2]).
-export([set_position/2]).
-export([buffer/1]).
-export([reset/1, clear/1]).
-export([stop/1, run/1, mute/1, unmute/1]).
-export([silence/2]).
-export([mark/5, mark/6]).
-export([unmark/2]).
-export([find_marks/3]).

%% test
-export([normalize_range/2]).
-export([test/0]).

-define(DEFAULT_FORMAT,   s16_le).
-define(DEFAULT_CHANNELS, 1).
-define(DEFAULT_RATE,     16000).
-define(DEFAULT_LOOP,     false).
-define(DEFAULT_MUTED,    false).
-define(DEFAULT_STATE,    stopped).

-type unsigned() :: non_neg_integer().
-type range_start() :: bof | unsigned().
-type range_size() :: eof | unsigned().
-type sample_range() :: [{range_start(), range_size()}].
-type sample_event_flag() :: stop|once.
-type sample_event() :: {pid(),Pos::unsigned(),
			 [sample_event_flag()],UserData::term()}.
-type sample_length() :: integer() | {time,number()}.
-type sample_position() :: 
	unsigned() | cur | bof | eof | {time,number()} |
	{cur, sample_length()} |
	{bof, sample_length()} |
	{eof, sample_length()}.
-define(GB_EMPTY, {0,nil}).


-record(sample_buffer,
	{
	 state = ?DEFAULT_STATE :: running | stopped,
	 format = ?DEFAULT_FORMAT  :: alsa:format(),  %%
	 channels = ?DEFAULT_CHANNELS :: unsigned(),  %%
	 rate = ?DEFAULT_RATE         :: number(),  %% Hz
	 muted = ?DEFAULT_MUTED :: boolean(),
	 loop = ?DEFAULT_LOOP :: boolean(),    %% start from beginning at end
	 bpf = 2 :: unsigned(),                %% bytes per frame, in bytes
	 cur = 0 :: integer(),                 %% current pos in current buffer
	 buf = <<>> :: binary(),               %% sample buffer
	 marks = #{} :: #{ reference() => sample_event() },
	 mark_tree = gb_trees:empty() :: 
		       gb_trees:tree(unsigned(),[reference()])
	}).

-type sample_buffer() :: #sample_buffer{}.

-spec new() ->
	  {ok,sample_buffer()} | {error,Reason::term()}.
new() -> new([]).

-spec new(Opts::list()|map()) ->
	  {ok,sample_buffer()} | {error,Reason::term()}.
new(Opts) -> new(Opts, []).

-spec new(Opts::list()|map(), Data::iodata()) ->
	  {ok,sample_buffer()}.
new(Opts,Data) when is_list(Opts) ->
    new(maps:from_list(Opts),Data);
new(Opts,Data) when is_map(Opts) ->
    Buf = iolist_to_binary(Data),
    Cb0 = #sample_buffer{buf=Buf},
    setopts(Cb0, Opts).

setopts(Cb, Opts) when is_list(Opts) ->
    setopts(Cb, maps:from_list(Opts));
setopts(Cb, Opts) when is_map(Opts) ->
    setopts_(Cb, maps:keys(Opts), Opts).

setopts_(Cx, [Key|Ks], Opts) ->
    Value = maps:get(Key, Opts),
    case Key of
	format when is_atom(Value) ->
	    case lists:member(Value, alsa:formats()) of
		true ->
		    Bpf = alsa:format_size(Value, 1)*
			Cx#sample_buffer.channels,
		    Cx1 = Cx#sample_buffer{format=Value,bpf=Bpf},
		    setopts_(Cx1,Ks,Opts);
		false ->
		    {error,{bad_format, Value}}
	    end;
	channels when is_integer(Value), Value > 0 ->
	    Bpf = alsa:format_size(Cx#sample_buffer.format, 1)*Value,
	    Cx1 = Cx#sample_buffer{channels=Value,bpf=Bpf},
	    setopts_(Cx1,Ks,Opts);
	rate when is_number(Value), Value>0 ->
	    Cx1 = Cx#sample_buffer{rate=Value},
	    setopts_(Cx1,Ks,Opts);
	loop when is_boolean(Value) ->
	    Cx1 = Cx#sample_buffer{loop=Value},
	    setopts_(Cx1,Ks,Opts);
	muted when is_boolean(Value) ->
	    Cx1 = Cx#sample_buffer{muted=Value},
	    setopts_(Cx1,Ks,Opts);
	state when Value =:= stopped;
		   Value =:= running ->
	    Cx1 = Cx#sample_buffer{state=Value},
	    setopts_(Cx1,Ks,Opts);
	_ ->
	    %% ignore other alsa options
	    setopts_(Cx,Ks,Opts)
    end;
setopts_(Cx, [], _Opts) ->
    {ok, Cx}.

getopts(Cb, [Key|Opts]) ->
    Value = case Key of
		format -> Cb#sample_buffer.format;
		channels -> Cb#sample_buffer.channels;
		rate -> Cb#sample_buffer.rate;
		loop -> Cb#sample_buffer.loop;
		muted -> Cb#sample_buffer.muted;
		bytes_per_sample -> Cb#sample_buffer.bpf;
		size -> byte_size(Cb#sample_buffer.buf);
		state -> Cb#sample_buffer.state
	    end,
    [{Key,Value}|getopts(Cb,Opts)];
getopts(_Cb, []) ->
    [].

%% add a mark to current position
-spec mark(Cb::sample_buffer(), Pid::pid(), Pos::sample_position(),
	   Flags:: [sample_event_flag()], UserData::term()) ->
	  Cb1::sample_buffer().
mark(Cb, Pid, Pos, Flags, UserData) when is_pid(Pid), is_list(Flags) ->
    mark(Cb, Pid, make_ref(), Pos, Flags, UserData).

mark(Cb=#sample_buffer{bpf=Bpf,mark_tree=Gb,marks=Marks},
     Pid, Ref, Pos, Flags, UserData)
  when is_pid(Pid), is_reference(Ref), is_list(Flags) ->
    Pos1 = pos(Cb,Pos)*Bpf,
    Gb1 = case gb_trees:lookup(Pos1, Gb) of
	      none -> 
		  gb_trees:insert(Pos1, [Ref], Gb);
	      {value,List0} -> 
		  gb_trees:update(Pos1, [Ref|List0], Gb)
	  end,
    Event = {Pid,Pos1,Flags,UserData},
    Marks1 = Marks#{ Ref => Event },
    io:format("add event ~p to gb=~p\n", [Event, Gb1]),
    {Ref, Cb#sample_buffer{mark_tree=Gb1, marks=Marks1}}.

%% remove a mark 
unmark(Cb=#sample_buffer{mark_tree=Gb,marks=Marks}, Ref) 
  when is_reference(Ref) ->
    case maps:get(Ref, Marks, undefined) of
	{_Pid,Pos,_Flags,_EventData} ->
	    Marks1 = maps:remove(Ref, Marks),
	    case gb_trees:lookup(Pos, Gb) of
		none ->
		    Cb#sample_buffer{marks=Marks1};
		{value,List0} ->
		    Gb1 = case lists:delete(Ref,List0) of
			      [] ->
				  gb_trees:delete(Pos, Gb);
			      List1 ->
				  gb_trees:update(Pos, List1, Gb)
			  end,
		    Cb#sample_buffer{mark_tree=Gb1, marks=Marks1}
	    end;
	undefined ->
	    Cb
    end.

-spec find_marks(Cb::sample_buffer(), From::unsigned(), To::unsigned()) ->
	  [{reference(),sample_event()}].
find_marks(#sample_buffer{mark_tree=Gb, marks=Marks,bpf=Bpf},
	   From, To) when is_integer(From), is_integer(To), From >= 0,
			  To >= From ->
    Iter = gb_trees:iterator_from(From*Bpf, Gb),
    case find_marks_(Iter, Marks, To*Bpf, []) of
	[] -> [];
	RefList -> [{Ref,maps:get(Ref,Marks)} || Ref <- RefList]
    end.

find_marks_(Iter, Marks, To, Acc) ->
    case gb_trees:next(Iter) of
	{Pos, RefList, Iter1} ->
	    if Pos < To ->
		    find_marks_(Iter1, Marks, To, Acc++RefList);
	       true -> Acc
	    end;
	none -> Acc
    end.

-spec get_position(Cb::sample_buffer()) -> integer().
get_position(Cb) ->
    get_position(Cb, cur).

-spec get_position(Cb::sample_buffer(),Pos::sample_position()) ->
	  integer().
get_position(Cb=#sample_buffer{bpf=Bpf}, Pos) ->
    pos(Cb, Pos) div Bpf.

-spec set_position(Cb::sample_buffer(),Pos::sample_position()) ->
	  Cb1::sample_buffer().
set_position(Cb, Pos) ->
    set_position_(Cb, pos(Cb, Pos)).

set_position_(Cb=#sample_buffer{bpf=Bpf,buf=Buf}, Pos) ->
    Pos1 = Pos*Bpf,  %% new byte position
    Pos2 = if Cb#sample_buffer.loop, Pos1 >= 0 ->
		   Pos1 rem byte_size(Buf);
	      Cb#sample_buffer.loop, Pos1 < 0 ->
		   BufSize = byte_size(Buf),
		   ((Pos1 rem BufSize) + BufSize) rem BufSize;
	      Pos >= 0 -> min(byte_size(Buf), Pos1);
	      Pos < 0 -> 0
	   end,
    Cb#sample_buffer{cur=Pos2}.

buffer(#sample_buffer{buf=Buf}) ->
    Buf.
    
reset(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{cur=0 }.

clear(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{cur=0, buf = <<>> }.

stop(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{state=stopped}.

run(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{state=running}.

mute(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{muted=true}.

unmute(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{muted=false}.

-spec read_samples_and_marks(Cb::sample_buffer(), Len::sample_length()) -> 
	  {Data::binary(), Cb1::sample_buffer(), [sample_event()]}.
read_samples_and_marks(Cb=#sample_buffer{cur=Cur,buf=Buf,bpf=Bpf,loop=Loop},
		       Len) ->
    BufSize = byte_size(Buf),      %% Number of bytes in buffer
    NumSamples = BufSize div Bpf,  %% Number of samples in buffer
    From = Cur div Bpf,            %% start Sample position
    Len1 = len(Cb, Len),           %% length in number of samples
    LenBytes = Len1*Bpf,           %% length in number of bytes
    {Data, Cb1} = read_(Cb, LenBytes),
    if Loop, Cur + LenBytes >= BufSize -> 
	    Cur2 = (Cur + LenBytes) rem BufSize,
	    Marks1 = find_marks(Cb, From, NumSamples),
	    Marks2 = find_marks(Cb, 0, Cur2),
	    {Data, Cb1, lists:usort(Marks1 ++ Marks2)};
       true ->
	    Marks = find_marks(Cb, From, From+Len1),
	    {Data, Cb1, Marks}
    end.

-spec read_samples(Cb::sample_buffer(), Len::sample_length()) -> 
	  {Data::binary(), Cb1::sample_buffer()}.
read_samples(Cb=#sample_buffer{bpf=Bpf}, Len) ->
    Len1 = len(Cb, Len),
    read_(Cb, Len1*Bpf).

-spec read(Cb::sample_buffer(), N::unsigned()) -> 
	  {Data::binary(), Cb1::sample_buffer()}.
read(Cb=#sample_buffer{}, N) when is_integer(N), N >= 0 ->
    read_(Cb, N).

read_(Cb=#sample_buffer{state=stopped}, _N) ->
    {<<>>, Cb};  %% nothing to read
read_(Cb=#sample_buffer{cur=Cur,buf=Buf,muted=true},N) ->
    BufSize = byte_size(Buf),
    Pos1 = Cur + N,
    if Cb#sample_buffer.loop ->
	    {silence(Cb,N), Cb#sample_buffer{cur=Pos1 rem BufSize}};
       true ->
	    Pos2 = min(BufSize, Pos1),
	    {silence(Cb,N),Cb#sample_buffer{cur=Pos2}}
    end;
read_(Cb=#sample_buffer{muted=false,state=running},N) ->
    read_data_(Cb, N).

read_data_(Cb=#sample_buffer{cur=Cur,buf=Buf}, N) ->
    case Buf of
	<<_:Cur/binary, Data:N/binary, _/binary>> ->
	    Pos1 = Cur+N,
	    {Data, Cb#sample_buffer{cur=Pos1}};
	<<_:Cur/binary, Data/binary>> when byte_size(Data) > 0 ->
	    Pos1 = Cur + byte_size(Data),
	    N1 = N - byte_size(Data),
	    {Data1, Cb1} = read_data_(Cb#sample_buffer{cur=Pos1},N1),
	    {<<Data/binary, Data1/binary>>, Cb1};
	<<_:Cur/binary>> ->
	    if Cb#sample_buffer.loop ->
		    read(Cb#sample_buffer{cur=0}, N);
	       true ->
		    {silence(Cb,N), Cb}
	    end
    end.

-spec skip_samples(Cb::sample_buffer(), Len::sample_length()) -> 
	  Cb1::sample_buffer().
skip_samples(Cb=#sample_buffer{bpf=Bpf}, Len) ->
    Len1 = len(Cb, Len),
    skip(Cb, Len1*Bpf).

-spec skip(Cb::sample_buffer(), N::unsigned()) -> Cb1::sample_buffer().
skip(Cb=#sample_buffer{cur=Pos, buf=Buf}, N) when is_integer(N), N >= 0 ->
    case Buf of
	<<_:Pos/binary, _:N/binary, _/binary>> ->
	    Cb#sample_buffer{cur=Pos+N};
	<<_:Pos/binary, Data/binary>> when byte_size(Data) > 0 ->
	    Pos1 = Pos + byte_size(Data),
	    skip(Cb#sample_buffer{cur=Pos1},N-byte_size(Data));
	<<_:Pos/binary>> ->
	    if Cb#sample_buffer.loop ->
		    skip(Cb#sample_buffer{cur=0}, N);
	       true ->
		    Cb
	    end
    end.

%% copy samples from sample buffer
-spec copy_samples(Cb::sample_buffer(), Range::sample_range()) -> binary().

copy_samples(Cb=#sample_buffer{bpf=Bpf,buf=Buf}, Range) ->
    IRange = normalize_range(Cb, Range),
    list_to_binary(copy_range_(IRange, Bpf, Buf)).

copy_range_([{Pos,Len}|Range],Bpf,Buf) ->
    Pos1 = Pos*Bpf, Len1=Len*Bpf,
    case Buf of
	<<_:Pos1/binary, Bin:Len1/binary, _/binary>> ->
	    [Bin | copy_range_(Range, Bpf, Buf)];
	<<_:Pos1/binary, Bin/binary>> ->
	    [Bin]
    end;
copy_range_([],_Bpf,_Buf) ->
    [].

-spec delete_samples(Cb::sample_buffer(), Range::sample_range()) ->
	  Cb1::sample_buffer.

%% FIXME: move Position if deleted!
delete_samples(Cb=#sample_buffer{bpf=Bpf,buf=Buf}, Range) ->
    IRange = normalize_range(Cb, Range),
    Buf1 = iolist_to_binary(delete_range_(IRange, 0, Bpf, Buf)),
    Cb#sample_buffer{buf=Buf1}.

delete_range_([{Pos,Len}|Range],Pos0,Bpf,Buf) ->
    Pos1 = Pos-Pos0,
    Pos2 = Pos1*Bpf, Len2 = Len*Bpf,
    <<Keep:Pos2/binary, _:Len2/binary, Buf1/binary>> = Buf,
    [Keep|delete_range_(Range,Pos+Len,Bpf,Buf1)];
delete_range_([],_Pos,_Bpf,Buf) ->
    [Buf].

normalize_range(Cb,Range) ->
    IRange = range(Cb,Range),            %% convert into integer range
    IRange2 = lists:keysort(1,IRange),         %% sorted on start positions
    overlap_range(IRange2).

%% remove overlapping areas
overlap_range([{Pos1,Len1},{Pos2,Len2}|Range]) when Pos2+Len2 =< Pos1+Len1 ->
    overlap_range([{Pos1,Len1}|Range]);
overlap_range([{Pos1,Len1},{Pos2,Len2}|Range]) when Pos2 =< Pos1+Len1 ->
    Len3 = ((Pos2+Len2)-(Pos1+Len1)),
    overlap_range([{Pos1,Len1+Len3}|Range]);
overlap_range([{Pos1,Len1}|Range]) ->
    [{Pos1,Len1}|overlap_range(Range)];
overlap_range([]) ->
    [].

%% translate range [{Pos,Len}] into byte position byte length
range(Cb, [Interval|Range]) ->
    [ival(Cb,Interval) | range(Cb, Range)];
range(_Cb, []) ->
    [].

ival(Cb=#sample_buffer{cur=Cur,buf=Buf,bpf=Bpf},{Pos,eof}) ->
    {pos(Cb,Pos), (byte_size(Buf)-Cur) div Bpf};
ival(Cb,{Pos,Len}) ->
    {pos(Cb,Pos), len(Cb,Len)}.

%% translate sample_position into absolute sample position
-spec pos(Cb::sample_buffer(), Pos::sample_position()) -> integer().
pos(_Cb,Pos) when is_integer(Pos), Pos >= 0 -> Pos;
pos(#sample_buffer{rate=R},{time,T}) -> trunc((R * T) / 1000);
pos(_Cb,bof) -> 0;
pos(#sample_buffer{bpf=Bpf,buf=Buf},eof) -> byte_size(Buf) div Bpf;
pos(#sample_buffer{bpf=Bpf,cur=Cur},cur) -> Cur div Bpf;
pos(Cb=#sample_buffer{bpf=Bpf,cur=Cur},{cur,Offs}) ->
    (Cur div Bpf) + len(Cb, Offs);
pos(Cb=#sample_buffer{},{bof,Offs}) -> len(Cb, Offs);
pos(Cb=#sample_buffer{bpf=Bpf,buf=Buf},{eof,Offs}) ->
    (byte_size(Buf) div Bpf) + len(Cb, Offs).

%% translate Len into number of samples
-spec len(Cb::sample_buffer(), Len::sample_length()) -> integer().
len(_Cb, Len) when is_integer(Len) -> Len;
len(#sample_buffer{rate=R}, {time,T}) -> trunc((R * T) / 1000).

%% Generate Len samples for silence 
-spec silence(Cb::sample_buffer(), Len::sample_length()) ->
	  binary().
silence(Cb=#sample_buffer{format=Format,channels=Channels}, Len) ->
    Len1 = len(Cb, Len) div Channels,
    alsa:make_silence(Format, Channels, Len1).

%% insert raw data (assumed in current format) at current position
-spec insert(Cb::sample_buffer(), Data::iodata()|{silence,sample_length()}) ->
	  Cb1::sample_buffer().
insert(Cb, Data) ->
    insert(Cb, cur, Data).

%% insert raw data (assumed in current format) at position Pos
-spec insert(Cb::sample_buffer(), Pos::sample_position(), 
	     Data::iodata()|{silence,sample_length()}) ->
	  Cb1::sample_buffer().
insert(Cb=#sample_buffer{bpf=Bpf,buf=Buf},Pos,Data) ->
    Pos1 = pos(Cb,Pos)*Bpf,
    Data1 = samples_to_binary(Cb,Data),
    <<Buf0:Pos1/binary,Buf1/binary>> = Buf,
    Cb#sample_buffer{buf = <<Buf0/binary,Data1/binary,Buf1/binary>>}.

-spec insert(Cb::sample_buffer(), Pos::sample_position(), 
	     Header::map(), Data::iodata()) ->
	  Cb1::sample_buffer().
insert(Cb=#sample_buffer{bpf=Bpf,buf=Buf},Pos,Header,Data) when 
      is_map(Header) ->
    Pos1 = pos(Cb,Pos)*Bpf,
    Data1 = samples_to_binary(Cb,Header,Data),
    <<Buf0:Pos1/binary,Buf1/binary>> = Buf,
    Cb#sample_buffer{buf = <<Buf0/binary,Data1/binary,Buf1/binary>>}.

samples_to_binary(_Cb,Data) when is_binary(Data) ->
    Data;
samples_to_binary(_Cb,Data) when is_list(Data) ->
    iolist_to_binary(Data);
samples_to_binary(Cb, {silence, Len}) ->
    silence(Cb, Len).

samples_to_binary(Cb,Header,Data) when is_binary(Data) ->
    transform_data(Cb,Header,Data);
samples_to_binary(Cb,Header,Data) when is_list(Data) ->
    transform_data(Cb,Header,iolist_to_binary(Data));
samples_to_binary(Cb, _Header, {silence, Len}) ->
    silence(Cb, Len).

%% write samples from a file into the sample_buffer
insert_file(Cb, Filename) ->
    insert_file(Cb, cur, Filename).

insert_file(Cb, Pos, Filename) ->
    case read_file(Filename) of    
	{ok,{Header,Data}} ->
	    insert(Cb, Pos, Header, Data);
	Error ->
	    io:format("inster_file: error ~p\n", [Error]),
	    Cb
    end.

append(Cb, Data) ->
    insert(Cb, eof, Data).

append(Cb, Header, Data) ->
    insert(Cb, eof, Header, Data).

append_file(Cb, Filename) ->
    insert_file(Cb, eof, Filename).

read_file(Filename) ->
    case file:open(Filename, [read, binary]) of
	{ok, Fd} ->
	    try alsa_playback:read_header(Fd) of
		Header ->
		    Format = maps:get(format, Header),
		    Channels = maps:get(channels, Header),
		    Size = alsa:format_size(Format, 1)*Channels,
		    case read_file_data(Fd, Size) of
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

read_file_data(Fd, Align) ->
    {ok,Cur} = file:position(Fd, cur),
    {ok,End} = file:position(Fd, eof),
    Size = End - Cur,
    AlignedDataSize = align_down(Size, Align),
    %% AlignedMaxDataSize = align_up(MaxDataSize, Align),
    DataSize = AlignedDataSize, %% min(AlignedMaxDataSize, AlignedDataSize),
    file:position(Fd, Cur),
    file:read(Fd, DataSize).

%% align X to nearest lower multiple of align
align_down(X, Align) ->
    X - (X rem Align).

%% align X to nearest greater multiple of Align
%%align_up(X, Align) ->
%%    X + ((Align - (X rem Align)) rem Align).


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

%% Tests

test() ->
    {ok,Cb1} = new(#{ format => u8, channels => 1}, "Hello world"),
    <<"low">> = buffer(delete_samples(Cb1, [{0,3},{5,1},{7,4}])),
    <<"low">> = copy_samples(Cb1, [{3,2},{6,1}]),

    {ok,Cb2} = new(#{ rate => 1000, format => u8, channels => 1}, "0123456789"),
    0 = get_position(Cb2, bof),
    0 = get_position(Cb2, cur),
    10 = get_position(Cb2, eof),
    2 = get_position(Cb2, {bof,2}),
    5 = get_position(Cb2, {cur,5}),
    8 = get_position(Cb2, {eof,-2}),
    2 = get_position(Cb2, {cur,{time,2}}),
    ok.
    
