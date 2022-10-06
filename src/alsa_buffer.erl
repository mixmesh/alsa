%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Sample buffer
%%% @end
%%% Created : 29 Aug 2022 by Tony Rogvall <tony@rogvall.se>

-module(alsa_buffer).

-export([new/0, new/1, new/2]).
-export([read/2, read_with_marks/2]).
-export([skip/2]).
-export([delete/2, copy/2]).
-export([insert/2, insert/3, insert/4]).
-export([insert_file/2, insert_file/3]).
-export([append/2, append/3]).
-export([append_file/2]).
-export([setopts/2, getopts/2]).
-export([get_position/1, get_position/2]).
-export([set_position/2]).
-export([buffer/1]).
-export([restart/1, clear/1]).
-export([stop/1, run/1, mute/2]).
-export([silence/2]).
-export([mark/5, mark/6]).
-export([unmark/2]).
-export([move_mark/3, set_mark_flags/3, set_mark_user_data/3]).
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
-type sample_length() :: integer() | {time,number()}.
-type sample_position() :: 
	unsigned() | cur | bof | eof | {time,number()} |
	{cur, sample_length()} |
	{bof, sample_length()} |
	{eof, sample_length()}.

-record(sample_buffer,
	{
	 state = ?DEFAULT_STATE :: running | stopped,
	 format = ?DEFAULT_FORMAT  :: alsa:format(),  %%
	 channels = ?DEFAULT_CHANNELS :: unsigned(),  %%
	 rate = ?DEFAULT_RATE         :: number(),  %% Hz
	 muted = ?DEFAULT_MUTED :: boolean(),
	 loop = ?DEFAULT_LOOP :: boolean(), %% start from beginning at end
	 bpf = 2 :: unsigned(),             %% bytes per frame, in bytes
	 cur = 0 :: integer(),              %% current frame in current buffer
	 buf = <<>> :: binary(),            %% sample buffer
	 marks = alsa_marks:marks()         %% sample marks
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
    Cb0 = #sample_buffer{buf=Buf,marks=alsa_marks:new()},
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
-spec mark(Cb::sample_buffer(), Pid::pid(), 
	   Pos::alsa_marks:sample_position(),
	   Flags:: [alsa_marks:sample_event_flag()], UserData::term()) ->
	  Cb1::sample_buffer().
mark(Cb, Pid, Pos, Flags, UserData) when is_pid(Pid), is_list(Flags) ->
    mark(Cb, Pid, make_ref(), Pos, Flags, UserData).

mark(Cb=#sample_buffer{marks=Marks},
     Pid, Ref, Pos, Flags, UserData)
  when is_pid(Pid), is_reference(Ref), is_list(Flags) ->
    Pos1 = pos(Cb,Pos),
    {Ref,Marks1} = alsa_marks:mark(Marks,Pid, Ref, Pos1, Flags, UserData),
    {Ref,Cb#sample_buffer{marks=Marks1}}.

%% remove a mark 
unmark(Cb=#sample_buffer{marks=Marks}, Ref) when is_reference(Ref) ->
    Marks1 = alsa_marks:unmark(Marks, Ref),
    Cb#sample_buffer{marks=Marks1}.

%% move mark to a new position
move_mark(Cb=#sample_buffer{marks=Marks}, Ref, NewPos) when
      is_reference(Ref) ->
    NewPos1 = pos(Cb,NewPos),
    Marks1 = alsa_marks:set_poistion(Marks, Ref, NewPos1),
    Cb#sample_buffer{marks=Marks1}.

%% set mark flags
set_mark_flags(Cb=#sample_buffer{marks=Marks}, Ref, NewFlags) when
      is_reference(Ref), is_list(NewFlags) ->
    Marks1 = alsa_marks:set_flags(Marks, Ref, NewFlags),
    Cb#sample_buffer{marks=Marks1}.    

%% set mark user data
set_mark_user_data(Cb=#sample_buffer{marks=Marks}, Ref, NewUserData) when
      is_reference(Ref) ->
    Marks1 = alsa_marks:set_user_data(Marks, Ref, NewUserData),
    Cb#sample_buffer{marks=Marks1}.    

%% return list of marks from and including position From
%% until but NOT including To (To is the location after the region scanned)
-spec find_marks(Cb::sample_buffer(), From::unsigned(), To::unsigned()) ->
	  [{reference(),alsa_marks:sample_event()}].
find_marks(#sample_buffer{marks=Marks},
	   From, To) when is_integer(From), is_integer(To), From >= 0,
			  To >= From ->
    alsa_marks:range(Marks, From, To).

-spec get_position(Cb::sample_buffer()) -> integer().
get_position(Cb=#sample_buffer{}) ->
    get_position(Cb, cur).

-spec get_position(Cb::sample_buffer(),Pos::alsa_marks:sample_position()) ->
	  integer().
get_position(Cb=#sample_buffer{}, Pos) ->
    pos(Cb, Pos).

-spec set_position(Cb::sample_buffer(),Pos::alsa_marks:sample_position()) ->
	  Cb1::sample_buffer().
set_position(Cb, Pos) ->
    set_position_(Cb, pos(Cb, Pos)).

set_position_(Cb=#sample_buffer{}, Pos) ->
    NFrames = pos_eof(Cb),
    Pos1 = if Cb#sample_buffer.loop, Pos >= 0 ->
		   Pos rem NFrames;
	      Cb#sample_buffer.loop, Pos < 0 ->
		   ((Pos rem NFrames) + NFrames) rem NFrames;
	      Pos >= 0 -> min(NFrames, Pos);
	      Pos < 0 -> 0
	   end,
    Cb#sample_buffer{cur=Pos1}.

buffer(#sample_buffer{buf=Buf}) ->
    Buf.
    
restart(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{ cur=0 }.

clear(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{ cur=0, buf = <<>> }.

stop(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{ state=stopped}.

run(Cb=#sample_buffer{}) ->
    Cb#sample_buffer{ state=running}.

mute(Cb=#sample_buffer{}, On) when is_boolean(On) ->
    Cb#sample_buffer{ muted=On}.

-spec read_with_marks(Cb::sample_buffer(), Len::sample_length()) -> 
	  {Data::binary(), Cb1::sample_buffer(), [alsa_marks:sample_event()]}.
read_with_marks(Cb=#sample_buffer{cur=Cur,loop=Loop}, Len) ->
    Len1 = len(Cb, Len),   %% length in number of samples
    NFrames = pos_eof(Cb), %% Number of frames in buffer
    {Data, Cb1} = read_(Cb, Len1),
    %% io:format("read: range from=~w,len=~w,n=~w\n", [Cur,Len1,NFrames]),
    Cur1 = Cur + Len1,
    if Loop, Cur1 >= NFrames -> 
	    Cur2 = Cur1 rem NFrames,
	    Marks1 = find_marks(Cb, Cur, NFrames), %% scan rest of buffer
	    Marks2 = find_marks(Cb, 0, Cur2),      %% scan wrapped buffer
	    {Data, Cb1, lists:usort(Marks1 ++ Marks2)};
       true ->
	    Marks = find_marks(Cb, Cur, Cur1),
	    {Data, Cb1, Marks}
    end.

-spec read(Cb::sample_buffer(), Len::sample_length()) ->
	  {Data::binary(), Cb1::sample_buffer()}.
read(Cb=#sample_buffer{}, Len) ->
    read_(Cb, len(Cb, Len)).

read_(Cb=#sample_buffer{state=stopped}, _Len) ->
    {<<>>, Cb};  %% nothing to read
read_(Cb=#sample_buffer{cur=Cur,muted=true},Len) ->
    NFrames = pos_eof(Cb), %% total number of frames
    Cur1 = Cur + Len,      %% position after read
    if Cb#sample_buffer.loop ->
	    {silence(Cb,Len), Cb#sample_buffer{cur=Cur1 rem NFrames}};
       true ->
	    Cur2 = min(NFrames, Cur1),
	    {silence(Cb,Len),Cb#sample_buffer{cur=Cur2}}
    end;
read_(Cb=#sample_buffer{muted=false,state=running},Len) ->
    read_data_(Cb, Len).

read_data_(Cb=#sample_buffer{cur=Cur,bpf=Bpf,buf=Buf}, Len) ->
    Pos  = Cur*Bpf,  %% current byte position
    BLen = Len*Bpf,  %% number of bytes to read
    case Buf of
	<<_:Pos/binary, Data:BLen/binary, _/binary>> ->
	    Cur1 = Cur+Len,
	    {Data, Cb#sample_buffer{cur=Cur1}};
	<<_:Pos/binary>> ->
	    if Cb#sample_buffer.loop, byte_size(Buf)>0 ->
		    read_data_(Cb#sample_buffer{cur=0}, Len);
	       true ->
		    {silence(Cb,Len), Cb}
	    end;
	<<_:Pos/binary, Data/binary>> ->
	    Len1 = byte_size(Data) div Bpf, %% number of frames in data
	    Cur1 = Cur + Len1,
	    Len2 = Len - Len1,
	    {Data1, Cb1} = read_data_(Cb#sample_buffer{cur=Cur1},Len2),
	    {<<Data/binary, Data1/binary>>, Cb1}
    end.

-spec skip(Cb::sample_buffer(), Len::sample_length()) -> 
	  Cb1::sample_buffer().
skip(Cb, Len) ->
    skip_data_(Cb, len(Cb, Len)).

skip_data_(Cb=#sample_buffer{cur=Cur, bpf=Bpf, buf=Buf}, Len)  ->
    Pos  = Cur*Bpf,  %% current byte position
    BLen = Len*Bpf,  %% number of bytes to read
    case Buf of
	<<_:Pos/binary, _:BLen/binary, _/binary>> ->
	    Cb#sample_buffer{cur=Cur+Len};
	<<_:Pos/binary>> -> %% check for partial frame?
	    if Cb#sample_buffer.loop, byte_size(Buf) > 0 ->
		    skip_data_(Cb#sample_buffer{cur=0}, Len);
	       true ->
		    Cb
	    end;
	<<_:Pos/binary, Data/binary>> ->
	    Len1 = byte_size(Data) div Bpf, %% number of frames in data
	    Cur1 = Cur + Len1,
	    skip_data_(Cb#sample_buffer{cur=Cur1},Len - Len1)
    end.

%% copy samples from sample buffer
-spec copy(Cb::sample_buffer(), Range::sample_range()) -> binary().

copy(Cb=#sample_buffer{bpf=Bpf,buf=Buf}, Range) ->
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

-spec delete(Cb::sample_buffer(), Range::sample_range()) ->
	  Cb1::sample_buffer.

%% FIXME: move Position if deleted!
delete(Cb=#sample_buffer{bpf=Bpf,buf=Buf}, Range) ->
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

ival(Cb=#sample_buffer{},{Pos,eof}) ->
    NFrames = pos_eof(Cb),
    Pos1 = pos(Cb,Pos),
    {Pos1, NFrames-Pos1};
ival(Cb,{Pos,Len}) ->
    {pos(Cb,Pos), len(Cb,Len)}.

%% position of at end of buffer (in number of frames)
pos_eof(#sample_buffer{bpf=Bpf,buf=Buf}) ->
    byte_size(Buf) div Bpf.

%% translate sample_position into absolute sample position
-spec pos(Cb::sample_buffer(), Pos::sample_position()) -> integer().
pos(_Cb,Pos) when is_integer(Pos), Pos >= 0 -> Pos;
pos(#sample_buffer{rate=R},{time,T}) -> trunc((R * T) / 1000);
pos(_Cb,bof) -> 0;
pos(Cb,eof) -> pos_eof(Cb);
pos(#sample_buffer{cur=Cur},cur) -> Cur;
pos(Cb=#sample_buffer{cur=Cur},{cur,Offs}) -> Cur + len(Cb, Offs);
pos(Cb=#sample_buffer{},{bof,Offs}) -> len(Cb, Offs);
pos(Cb=#sample_buffer{},{eof,Offs}) -> pos_eof(Cb) + len(Cb, Offs).

%% translate Len into number of samples
-spec len(Cb::sample_buffer(), Len::sample_length()) -> integer().
len(_Cb, Len) when is_integer(Len) -> Len;
len(#sample_buffer{rate=R}, {time,T}) -> trunc((R * T) / 1000).

%% Generate Len samples for silence 
-spec silence(Cb::sample_buffer(), Len::sample_length()) ->
	  binary().
silence(Cb=#sample_buffer{format=Format,channels=Channels}, Len) ->
    alsa:make_silence(Format, Channels, len(Cb,Len)).

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
	    io:format("insert_file: error ~p\n", [Error]),
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
%%     Data1 = alsa_util:reformat(Format, Cb#sample_buffer.format,
%%			       Channels, Cb#sample_buffer.channels,
%%			       Data),
    Data1 = alsa_samples:reformat(Format, Cb#sample_buffer.format,
				  Channels, Cb#sample_buffer.channels,
				  Data),
    Data2 = if Rate =/= Cb#sample_buffer.rate ->
%%		    alsa_util:resample(Rate, Cb#sample_buffer.rate,
%%				       Cb#sample_buffer.format,
%%				       Cb#sample_buffer.channels, Data1);
		    alsa_samples:resample(Rate, Cb#sample_buffer.rate,
					  Cb#sample_buffer.format,
					  Cb#sample_buffer.channels, Data1);
	       true ->
		    Data1
	    end,
    Data2.

%% Tests

test() ->
    {ok,Cb1} = new(#{ format => u8, channels => 1}, "Hello world"),
    <<"low">> = buffer(delete(Cb1, [{0,3},{5,1},{7,4}])),
    <<"low">> = copy(Cb1, [{3,2},{6,1}]),

    {ok,Cb2} = new(#{ rate => 1000, format => u8, channels => 1}, "0123456789"),
    0 = get_position(Cb2, bof),
    0 = get_position(Cb2, cur),
    10 = get_position(Cb2, eof),
    2 = get_position(Cb2, {bof,2}),
    5 = get_position(Cb2, {cur,5}),
    8 = get_position(Cb2, {eof,-2}),
    2 = get_position(Cb2, {cur,{time,2}}),

    {ok,Cb3_0} = new(#{ rate=>1000, format=>s16_le, channels => 1},
		     <<<<X:16/signed-little>> ||  X <- [10,-10,0,1,-1,100]>>),
    Cb3_1 = run(Cb3_0),
    {<<10:16/signed-little, -10:16/signed-little>>, _} = read(Cb3_1, 2),
    Cb3_2 = skip(Cb3_1, 2),
    {<<0:16/signed-little, 1:16/signed-little>>, _} = read(Cb3_2, 2),
    ok.
    
