%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    ALSA multi channel sample player
%%% @end
%%% Created : 30 Aug 2022 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(alsa_play).

-behaviour(gen_server).

%% API
-export([start/1]).
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-define(SERVER, ?MODULE).
-define(MAX_CHANNELS, 16).
-define(is_channel(C), (is_integer((C)) andalso ((C)>=0) 
			andalso ((C)<?MAX_CHANNELS))).
-define(DEFAULT_WAVE_NUM, 0).

-export([new/1]).
-export([set_wave/2]).
-export([mute/2]).
-export([insert/4]).
-export([insert_file/2, insert_file/3]).
-export([append/2, append/3]).
-export([append_file/2]).
-export([mark/2, mark/3, mark/4]).
-export([unmark/2]).
-export([delete/2]).
-export([run/1, run/0]).
-export([stop/1, stop/0]).
-export([restart/1, clear/1, remove/1]).
-export([restart/0, clear/0, remove/0]).
-export([pause/0, resume/0]).

-type channel() :: non_neg_integer().

-define(verbose(F), ok).
%% -define(verbose(F), io:format((F),[])).
-define(verbose(F,A), ok).
%% -define(verbose(F,A), io:format((F),(A))).
%%-define(info(F,A), ok).
-define(info(F,A), io:format((F),(A))).

-record(state, 
	{
	 handle :: alsa:handle(),
	 params :: #{},                 %% alsa open params
	 pause = true :: boolean(),     %% user call pause
	 playing = false :: boolean(),  %% alsa is playing
	 channels :: #{ integer() => alsa_samples:wavedef() },
	 output = undefined :: undefine | binary(),
	 marks = []      %% current marks
	}).


new(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {new, Channel}).

set_wave(Channel, WaveDef) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {set_wave, Channel, WaveDef}).

insert(Channel, Pos, Header, Samples) ->
    case is_channel(Channel) andalso
	is_pos(Pos) andalso
	is_header(Header) andalso
	is_data(Samples) of
	true ->
	    gen_server:call(?SERVER, {insert,Channel,Pos,Header,Samples});
	false ->
	    error(badarg)
    end.

insert(Channel, Pos, Samples) ->
    case is_channel(Channel) andalso
	is_pos(Pos) andalso
	is_data(Samples) of
	true ->
	    gen_server:call(?SERVER, {insert,Channel,Pos,Samples});
	false ->
	    error(badarg)
    end.

insert_file(Channel, Filename) ->
    insert_file(Channel, cur, Filename).

insert_file(Channel, Pos, Filename) ->
    case is_channel(Channel) andalso 
	is_pos(Pos) andalso 
	is_filename(Filename) of
	true ->
	    case alsa_util:read_file(Filename) of
		{ok,{Header,Samples}} ->
		    gen_server:call(?SERVER, {insert,Channel,Pos,Header,Samples});
		Error ->
		    Error
	    end;
	false ->
	    error(badarg)
    end.

append(Channel, Samples) ->
    insert(Channel, eof, Samples).

append(Channel, Header, Samples) ->
    insert(Channel, eof, Header, Samples).

append_file(Channel, Filename) ->
    insert_file(Channel, eof, Filename).

-spec mark(Channel::channel(), UserData::term()) ->
    {ok, Ref::reference()} | {error, Reason::term()}.
mark(Channel, UserData) -> mark(Channel, cur, [], UserData).

-spec mark(Channel::channel(), Flags::[alsa_marks:event_flag()],
      UserData::term()) ->
    {ok, Ref::reference()} | {error, Reason::term()}.
mark(Channel, Flags, UserData) -> mark(Channel, cur, Flags, UserData).

-spec mark(Channel::channel(), Pos::alsa_marks:position(),
	   Flags::[alsa_samples:event_flag()], UserData::term()) ->
	  {ok, Ref::reference()} | {error, Reason::term()}.
mark(Channel, Pos, Flags, UserData) ->
    case is_channel(Channel) andalso
	is_pos(Pos) andalso
	is_mark_flags(Flags) of
	true ->
	    gen_server:call(?SERVER, {mark,self(),Channel,Pos,Flags,UserData});
	false ->
	    error(badarg)
    end.

-spec unmark(Channel::channel(), Ref::reference()) ->
	  ok | {error, Reason::term()}.
unmark(Channel, Ref) ->
    case is_channel(Channel) andalso
	is_reference(Ref) of
	true -> gen_server:call(?SERVER, {unmark, Channel, Ref});
	false -> error(badarg)
    end.

%% delete samples in channel
delete(Channel, Range) when ?is_channel(Channel), is_list(Range) ->
    gen_server:call(?SERVER, {delete, Channel, Range}).

restart(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {restart, Channel}).

clear(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {clear, Channel}).

remove(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {remove, Channel}).

remove() ->
    gen_server:call(?SERVER, remove).

mute(Channel, On) when ?is_channel(Channel), is_boolean(On) ->
    gen_server:call(?SERVER, {mute, Channel, On}).

run(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {run, Channel}).

stop(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {stop, Channel}).

restart() ->
    gen_server:call(?SERVER, restart).

clear() ->
    gen_server:call(?SERVER, clear).

run() ->
    gen_server:call(?SERVER, run).

stop() ->
    gen_server:call(?SERVER, stop).

resume() ->
    gen_server:call(?SERVER, resume).

pause() ->
    gen_server:call(?SERVER, pause).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [#{}], []).

-spec start_link(Options::#{ atom() => term()}) -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start_link(Options) when is_map(Options) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Options, []).

-spec start(Options::#{ atom() => term()}) -> {ok, Pid :: pid()} |
	  {error, Error :: {already_started, pid()}} |
	  {error, Error :: term()} |
	  ignore.
start(Options) when is_map(Options) ->
    gen_server:start({local, ?SERVER}, ?MODULE, Options, []).
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%% @end
%%--------------------------------------------------------------------
-spec init(Args :: term()) -> {ok, State :: term()} |
	  {ok, State :: term(), Timeout :: timeout()} |
	  {ok, State :: term(), hibernate} |
	  {stop, Reason :: term()} |
	  ignore.
init(Options) ->
    process_flag(trap_exit, true),
    case alsa_playback:open(Options) of
	{ok,H,Params} ->
	    ?info("Alsa open: params = ~p\n", [Params]),
	    {ok, #state{ handle = H,
			 params = Params,
			 channels = #{}
		       }};
	{error, Reason} ->
	    {stop, {error, Reason}}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%% @end
%%--------------------------------------------------------------------
-spec handle_call(Request :: term(), From :: {pid(), term()}, State :: term()) ->
	  {reply, Reply :: term(), NewState :: term()} |
	  {reply, Reply :: term(), NewState :: term(), Timeout :: timeout()} |
	  {reply, Reply :: term(), NewState :: term(), hibernate} |
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), Reply :: term(), NewState :: term()} |
	  {stop, Reason :: term(), NewState :: term()}.

handle_call({new, Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    Rate = maps:get(rate, State#state.params, 16000),
	    W = alsa_samples:wave_new(),
	    alsa_samples:wave_set_rate(W, Rate),
	    ChanMap1 = ChanMap#{ Channel => W },
	    {reply, ok, State#state { channels = ChanMap1 }};
	_Buf ->
	    {reply, {error, ealready}, State}
    end;

handle_call({set_wave,Channel,WaveDef}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    alsa_samples:set_wave(W, WaveDef),
	    {reply, ok, State}
    end;

handle_call({insert,Channel,Pos,Samples}, _From, State) ->
    handle_insert(Channel, Pos, State#state.params, Samples, State);

handle_call({insert,Channel,Pos,Header,Samples}, _From, State) ->
    handle_insert(Channel, Pos, Header, Samples, State);

handle_call({mark,Pid,Channel,Pos,Flags,UserData}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    Ref = erlang:monitor(process, Pid),
	    Pos1 = pos(W, Pos),
	    Flags1 = flags(W, Flags),
	    alsa_samples:mark(W, Pid, Ref, Pos1, Flags1, UserData),
	    {reply, {ok,Ref}, State}
    end;

handle_call({unmark,Channel,Ref}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error,enoent}, State};
	W ->
	    alsa_samples:unmark(W, Ref),
	    erlang:demonitor(Ref, [flush]),
	    {reply, ok, State}
    end;

handle_call({restart,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    alsa_samples:wave_set_time(W, 0),
	    {reply, ok, State}
    end;

%% clear one channel, but keep structure
handle_call({clear,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    alsa_samples:wave_clear(W),
	    {reply, ok, State}
    end;
%% clear all channels
handle_call(clear, _From, State) ->
    maps:foreach(
      fun(_Channel, W) ->
	      %% force flush of all potential samples
	      alsa_samples:wave_clear(W)
      end, State#state.channels),
    {reply, ok, State};

handle_call({mute,Channel,On}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    alsa_samples:wave_set_mute(W, On),
	    {reply, ok, State}
    end;

handle_call(run, _From, State) ->
    maps:foreach(
      fun (_Channel, W) ->
	      alsa_samples:wave_set_state(W, running)
      end, State#state.channels),
    State1 = continue_playing(State),
    {reply, ok, State1};

handle_call({run,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    alsa_samples:wave_set_state(W, running),
	    State1 = continue_playing(State),
	    {reply, ok, State1}
    end;

handle_call({stop,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    alsa_samples:wave_set_state(W, stopped),
	    {reply, ok, State}
    end;
handle_call(stop, _From, State) ->
    maps:foreach(
      fun(_Channel, W) ->
	      alsa_samples:wave_set_state(W, stopped)
      end, State#state.channels),
    {reply, ok, State};

handle_call({remove,Channel}, _From, State) ->
    case maps:take(Channel, State#state.channels) of
	error ->
	    {reply, ok, State};
	{W, #{}} ->
	    alsa_samples:wave_clear(W),
	    {reply, ok, State#state { channels = #{}, playing = false }};
	{W, ChanMap1} ->
	    alsa_samples:wave_clear(W),
	    {reply, ok, State#state { channels = ChanMap1 }}
    end;

handle_call(remove, _From, State) ->
    maps:foreach(
      fun(_Channel, W) ->
	      alsa_samples:wave_clear(W)
      end, State#state.channels),
    {reply, ok, State#state { channels = #{}, playing = false }};

handle_call({delete,Channel,Range}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    Reply = alsa_samples:wave_delete_samples(W, Range),
	    {reply, Reply, State}
    end;

handle_call(restart, _From, State) ->
    maps:foreach(
      fun(_Channel, W) ->
	      alsa_samples:wave_set_time(W, 0)
      end, State#state.channels),
    {reply, ok, State};

handle_call(pause, _From, State) ->
    case State#state.pause of
	true ->
	    {reply, ok, State};
	false ->
	    {reply, ok, State#state { pause = true }}
    end;
handle_call(resume, _From, State) ->
    case State#state.pause of
	true ->
	    State1 = continue_playing(State#state{pause=false}),
	    %% io:format("resume: playing=~p\n", [State1#state.playing]),
	    {reply, ok, State1};
	false ->
	    {reply, ok, State}
    end;

handle_call(_Request, _From, State) ->
    Reply = {error,{bad_call,_Request}},
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_cast(Request :: term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: term(), NewState :: term()}.
handle_cast(_Request, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%% @end
%%--------------------------------------------------------------------
-spec handle_info(Info :: timeout() | term(), State :: term()) ->
	  {noreply, NewState :: term()} |
	  {noreply, NewState :: term(), Timeout :: timeout()} |
	  {noreply, NewState :: term(), hibernate} |
	  {stop, Reason :: normal | term(), NewState :: term()}.

handle_info({select,Handle,undefined,_Ready}, State) when 
      Handle =:= State#state.handle, State#state.pause =:= false ->
    case alsa:write_(Handle, State#state.output) of
	{error, eagain} ->
	    ?verbose("select: eagain\n"),
	    ok = alsa:select_(Handle),
	    {norereply, State};
	{error, epipe} -> 
	    ?verbose("select: epipe\n"),
	    alsa:recover_(Handle, epipe),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{error, estrpipe} ->
	    ?verbose("play: estrpipe\n"),
	    alsa:recover_(Handle, estrpipe),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{error, AlsaError} when is_integer(AlsaError) ->
	    _Error =  alsa:strerror(AlsaError),
	    ?verbose("play: error ~s\n", [_Error]),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{ok, Written} ->
	    Size = byte_size(State#state.output),
	    if Written =:= Size ->
		    State1 = notify(State#state.marks, State),
		    State2 = play(State1#state{output=undefined,marks=[]}),
		    {noreply, State2};
	       true ->
		    <<_:Written/binary, Bin1/binary>> = State#state.output,
		    State1 = play(State#state{output=Bin1 }),
		    {noreply, State1}
	    end
    end;
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%% @end
%%--------------------------------------------------------------------
-spec terminate(Reason :: normal | shutdown | {shutdown, term()} | term(),
		State :: term()) -> any().
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%% @end
%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term() | {down, term()},
		  State :: term(),
		  Extra :: term()) -> {ok, NewState :: term()} |
	  {error, Reason :: term()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called for changing the form and appearance
%% of gen_server status when it is returned from sys:get_status/1,2
%% or when it appears in termination error logs.
%% @end
%%--------------------------------------------------------------------
-spec format_status(Opt :: normal | terminate,
		    Status :: list()) -> Status :: term().
format_status(_Opt, Status) ->
    Status.

%%%===================================================================
%%% Internal functions
%%%===================================================================

continue_playing(State) ->
    if State#state.pause -> State;
       State#state.playing -> State;
       true -> play(State#state{playing=true})
    end.

handle_insert(Channel, Pos, Header, Samples, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    SrcRate = maps:get(rate, Header, 16000),
	    SrcFormat = maps:get(format, Header, s16_le),
	    SrcChannels = maps:get(channels, Header, 1),
	    Pos1 = pos(W, Pos),
	    Samples1 = samples_to_binary(W, Header, Samples),
	    %% FIXME: handle multi channel! use wave index!
	    ?verbose("voice=~w,src_rate=~w, src_format=~w, scr_channels=~w, pos=~w\n",
		      [Channel, SrcRate, SrcFormat, SrcChannels, Pos1]),
	    ?verbose("#samples = ~w\n", [byte_size(Samples1) div
					     alsa:format_size(SrcFormat,
							      SrcChannels)]),
	    ok = alsa_samples:wave_set_samples(W, ?DEFAULT_WAVE_NUM,
					       Pos1, 0,
					       SrcRate, SrcFormat, SrcChannels, 
					       Samples1),
	    {reply, ok, State}
    end.

samples_to_binary(_W,_Header,Data) when is_binary(Data) ->
    Data;
samples_to_binary(_W,_Header,Data) when is_list(Data) ->
    iolist_to_binary(Data);
samples_to_binary(W, #{ format := Format, channels := Channels},
		  {silence, Len}) ->
    alsa:make_silence(Format, Channels, len(W,Len)).


-type unsigned() :: non_neg_integer().
%%-type range_start() :: bof | unsigned().
%%-type range_size() :: eof | unsigned().
%%-type sample_range() :: [{range_start(), range_size()}].
-type sample_length() :: integer() | {time,number()}.
-type sample_position() :: 
	unsigned() | cur | bof | eof | last | {time,number()} |
	{cur, sample_length()} |
	{bof, sample_length()} |
	{eof, sample_length()}.

%% translate sample_position into absolute sample position
-spec pos(W::alsa_samples:wavedef(), Pos::sample_position()) -> integer().
pos(_W,Pos) when is_integer(Pos), Pos >= 0 -> Pos;
pos(W,{time,T}) -> time_pos(W, T);
pos(_W,bof) -> 0;
pos(W,eof) -> pos_eof(W);
pos(W,last) -> pos_last(W);
pos(W,cur) -> pos_cur(W);
pos(W,{cur,Offs}) -> pos_cur(W) + len(W, Offs);
pos(W,{bof,Offs}) -> len(W, Offs);
pos(W,{eof,Offs}) -> pos_eof(W) + len(W, Offs).

pos_cur(W) ->
    round(alsa_samples:wave_get_time(W) * alsa_samples:wave_get_rate(W)).

pos_eof(W) ->
    alsa_samples:wave_get_num_samples(W, ?DEFAULT_WAVE_NUM).

pos_last(W) ->
    case pos_eof(W) of
	0 -> 0;
	N -> N-1
    end.

%% translate Len into number of samples
-spec len(W::alas_samples:wavedef(), Len::sample_length()) -> integer().
len(_W, Len) when is_integer(Len) -> Len;
len(W, {time,T}) -> time_pos(W, T).

time_pos(W, T) ->
    trunc((alsa_samples:wave_get_rate(W) * T) / 1000).

%% translate set/repeat positions
flags(W, Fs) -> [flag(W, F) || F <- Fs].

flag(_W, restart) -> {set,0};
flag(W, {set,Pos}) -> {set,pos(W,Pos)};
flag(W, {repeat,Pos,Count}) -> {repeat,pos(W,Pos),Count};
flag(_W, F) -> F.

notify([], State) -> 
    State;
notify([{Channel,Ms}|Marks], State) ->
    %% Note that marks is reversed in RevMs
    %% W = maps:get(Channel, State#state.channels),
    lists:foreach(
      fun({Ref,Pid,Pos,UserData}) ->
	      Event = {Ref,Channel,Pos,UserData},
	      ?verbose("notify ~p\n", [Event]),
	      Pid ! Event
      end, Ms),
    notify(Marks, State).

play(State = #state {output=undefined,pause=true}) ->
    %% io:format("playing=false\n"),
    State#state{ playing=false };
play(State = #state {handle=Handle,output=undefined,
		     params=Params,channels=ChanMap }) ->
    PeriodSize = maps:get(period_size, Params),
    Channels = maps:get(channels, Params),
    Format = maps:get(format, Params),
    case read_buffer_list(ChanMap, Format, Channels, PeriodSize) of
	{[], Marks} ->
	    State1 = notify(Marks, State),
	    %% io:format("playing=false\n"),
	    State1#state{ playing=false, output=undefined, marks=[]};
	{BufferList,Marks} ->
	    Bin = alsa_samples:mix(Format, Channels, BufferList, []),
	    Playing = if State#state.playing =:= false, 
			 State#state.pause =:= true ->
			      true;
			 true ->
			      State#state.playing
		      end,
	    %% io:format("playing=~w, n=~w\n", [Playing,length(BufferList)]),
	    case alsa:write_(Handle, Bin) of
		{error, eagain} ->
		    ?verbose("play: again\n"),
		    ok = alsa:select_(Handle),
		    State#state{ playing=Playing, output=Bin, marks=Marks };
		{error, epipe} -> 
		    ?verbose("play: epipe\n"),
		    alsa:recover_(Handle, epipe),
		    play(State#state{playing=Playing,output=Bin,marks=Marks});
		{error, estrpipe} ->
		    ?verbose("play: estrpipe\n"),
		    alsa:recover_(Handle, estrpipe),
		    play(State#state{playing=Playing,output=Bin,marks=Marks});
		{error, AlsaError} when is_integer(AlsaError) ->
		    _Error =  alsa:strerror(AlsaError),
		    ?verbose("play: error ~s\n", [_Error]),
		    play(State#state{playing=Playing,output=Bin,marks=Marks});
		{ok, Written} ->
		    ?verbose("play: written ~w, remain=~w\n", 
			     [Written, byte_size(Bin)]),
		    if Written =:= byte_size(Bin) ->
			    State1 = notify(Marks, State),
			    play(State1#state{playing=Playing,
					      output=undefined, marks=[]});
		       true ->
			    <<_:Written/binary, Bin1/binary>> = Bin,
			    play(State#state{playing=Playing,
					     output=Bin1,marks=Marks})
		    end
	    end
    end;
%% send more!
play(State = #state {handle=Handle,marks=Marks,output=Bin}) ->
    case alsa:write_(Handle, Bin) of
	{error, eagain} ->
	    ?verbose("play1: again\n"),
	    ok = alsa:select_(Handle),
	    State;
	{error, epipe} ->
	    ?verbose("play1: epipe\n"),
	    alsa:recover_(Handle, epipe),
	    play(State);
	{error, estrpipe} ->
	    ?verbose("play1: estrpipe\n"),
	    alsa:recover_(Handle, estrpipe),
	    play(State);
	{error, AlsaError} when is_integer(AlsaError) ->
	    _Error =  alsa:strerror(AlsaError),
	    ?verbose("play1: error ~s\n", [_Error]),	    
	    play(State);
	{ok, Written} ->
	    ?verbose("play1: written ~w, remain=~w\n", 
		     [Written, byte_size(Bin)]),
	    if Written =:= byte_size(Bin) ->
		    State1 = notify(Marks, State),
		    play(State1#state{output=undefined,marks=[]});
	       true ->
		    <<_:Written/binary, Bin1/binary>> = Bin,
		    play(State#state{output=Bin1})
	    end
    end.

read_buffer_list(ChanMap, Format, Channels, PeriodSize) ->
    read_buffer_list_(maps:keys(ChanMap), ChanMap, 
		      Format, Channels, PeriodSize, [], []).

read_buffer_list_([Channel|Cs], ChanMap, Format, Channels, PeriodSize,
		  Acc, Marks) ->
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    read_buffer_list_(Cs, ChanMap, Format, Channels, 
			      PeriodSize, Acc, Marks);
	W ->
	    case alsa_samples:wave(W, Format, Channels, PeriodSize) of
		{Ms,<<>>} ->
		    read_buffer_list_(Cs, ChanMap, Format, Channels, 
				      PeriodSize, Acc, 
				      add_marks(Channel,Ms,Marks));
		{Ms,Samples} ->
		    %% Ms = alsa_samples:get_marks(W, PeriodSize),
		    read_buffer_list_(Cs, ChanMap,
				      Format, Channels, PeriodSize,
				      [Samples|Acc], 
				      add_marks(Channel,Ms,Marks))
	    end
    end;
read_buffer_list_([], _ChanMap, _Format, _Channels, _PeriodSize, Acc, Marks) ->
    {Acc, Marks}.

add_marks(_Channel,[],Marks) -> Marks;
add_marks(Channel,Ms,Marks) -> [{Channel,Ms}|Marks].
    
%%
%% check argument types
%%
%% Len = integer() | {time, number()}
%% Pos = bof|eof|cur|{bof,Len}|{eof,Len}|{cur,Len}|unsigned()|{time,number()}
is_pos(bof) -> true;
is_pos(eof) -> true;
is_pos(last) -> true;
is_pos(cur) -> true;
is_pos(Pos) when is_integer(Pos), Pos >= 0 -> true;
is_pos({bof,Len}) -> is_len(Len);
is_pos({eof,Len}) -> is_len(Len);
is_pos({cur,Len}) -> is_len(Len);
is_pos(Len) -> is_len(Len).

is_len({time,T}) when is_number(T) -> true;
is_len(Len) -> is_integer(Len).

is_channel(Chan) -> is_integer(Chan).
    
is_data({silence,Len}) -> is_len(Len);
is_data(Data) when is_binary(Data) -> true;
is_data(List) when is_list(List) -> true;
is_data(_) -> false.

is_header(Header) -> is_map(Header).
    
is_filename(Name) -> is_string(Name).

is_string(Name) -> 
    try erlang:iolist_size(Name) of
	_ -> true
    catch
	error:_ -> 
	    false
    end.

is_mark_flags([notify|Fs]) -> is_mark_flags(Fs);
is_mark_flags([once|Fs]) -> is_mark_flags(Fs);
is_mark_flags([stop|Fs]) -> is_mark_flags(Fs);
is_mark_flags([restart|Fs]) -> is_mark_flags(Fs); %% = {set,{pos,bof}}
is_mark_flags([{set,Pos}|Fs]) -> is_pos(Pos) andalso is_mark_flags(Fs);
is_mark_flags([{repeat,Pos,Num}|Fs]) -> is_pos(Pos) andalso 
					is_integer(Num) andalso
					Num >= 0 andalso
					is_mark_flags(Fs);
is_mark_flags([]) -> true;
is_mark_flags(_) -> false.
