%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    ALSA multi voice sample player
%%% @end
%%% Created : 30 Aug 2022 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(alsa_play).

-behaviour(gen_server).

%% API
-export([start/0, start/1]).
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-export([alloc/0, alloc/1]).
-export([new/1, new/2]). %% avoid, but ok to use
-export([set_wave/2]).
-export([set_volume/2]).
-export([mute/2]).
-export([set_samples/3, set_samples/4]).
-export([set_file/3]).
-export([insert/4, insert/5]).
-export([insert_file/3, insert_file/4]).
-export([append/3, append/4]).
-export([append_file/3]).
-export([mark/2, mark/3, mark/4]).
-export([unmark/2]).
-export([delete/2]).
-export([run/1, run/0]).
-export([stop/1, stop/0]).
-export([restart/1, clear/1, remove/1, update/1]).
-export([restart/0, clear/0, remove/0]).
-export([pause/0, resume/0]).
-export([set_callback/1, set_callback/2,set_callback_args/1]).
-export([get_params/0]).
-export([get_allocated_list/0]).
-export([get_free_list/0]).

-include("../include/alsa_log.hrl").

-define(dbg(F,A), ok).
%% -define(dbg(F,A), io:format((F),(A))).

-define(SERVER, ?MODULE).

-define(MAX_VOICES, 128).   %% just a reasonably high number
-define(is_voiceid(ID), (is_integer((ID)) andalso ((ID)>=1)
			 andalso ((ID)<?MAX_VOICES))).
-define(DEFAULT_WAVE_NUM, 0).

-type unsigned() :: non_neg_integer().

-type voiceid() :: integer().
-type voiceopt() :: {queue,boolean()} | {clear,boolean()} |
		    {buffer_size, unsigned()}.

%%-type range_start() :: bof | unsigned().
%%-type range_size() :: eof | unsigned().
%%-type sample_range() :: [{range_start(), range_size()}].
-type sample_length() :: integer() | {time,number()}.
-type sample_position() :: 
	unsigned() | cur | bof | eof | last | {time,number()} |
	{cur, sample_length()} |
	{bof, sample_length()} |
	{eof, sample_length()} |
	{label, integer()}.

-type callback3() ::
	fun((Samples::binary(),NumBytes::integer(),Params::map()) -> ok).
-type callback4() ::
	fun((Samples::binary(),NumBytes::integer(),
	     Params::map(), Args::[term()]) -> ok).

-record(state, 
	{
	 handle :: alsa:handle(),
	 params :: #{},                 %% alsa open params
	 pause = true :: boolean(),     %% user call pause
	 playing = false :: boolean(),  %% alsa is playing
	 voice_map :: #{ voiceid() => alsa_samples:wavedef() },
	 owner_map :: #{ voiceid() => reference(), reference() => voiceid() },
	 free_list :: [voiceid()],
	 output = undefined :: undefined | binary(),
	 marks = [],     %% current marks
	 callback = undefined :: undefined | callback3() | callback4(),
	 callback_args = undefined  %% list when callback4!
	}).

%%%===================================================================
%%% API
%%%===================================================================

-spec alloc() -> ID::voiceid().
alloc() -> alloc([]).
-spec alloc([voiceopt()]) -> ID::voiceid().
alloc(Options) when is_list(Options) ->
    gen_server:call(?SERVER, {alloc, self(), Options}).

-spec get_allocated_list() -> [voiceid()].
get_allocated_list() ->
    gen_server:call(?SERVER, get_allocated_list).

%% old
-spec new(ID::voiceid()) -> ok.
new(ID) ->
    new(ID, []).
-spec new(ID::voiceid(), [voiceopt()]) -> ok.
new(ID,Options) when ?is_voiceid(ID), is_list(Options) ->
    gen_server:call(?SERVER, {new, ID, Options}).


remove(ID) when ?is_voiceid(ID) ->
    gen_server:call(?SERVER, {remove, ID}).
remove() ->
    gen_server:call(?SERVER, remove).


-spec get_free_list() -> [voiceid()].
get_free_list() ->
    gen_server:call(?SERVER, get_free_list).

%% get alsa param map
-spec get_params() -> map().
get_params() ->
    gen_server:call(?SERVER, get_params).

set_wave(ID, WaveDef) when ?is_voiceid(ID) ->
    gen_server:call(?SERVER, {set_wave, ID, WaveDef}).

set_volume(ID, Volume) when ?is_voiceid(ID), is_number(Volume), 
			    Volume >= 0, Volume =< 1 ->
    gen_server:call(?SERVER, {set_volume, ID, Volume}).

set_samples(ID, Index, Samples) ->
    case is_voiceid(ID) andalso is_data(Samples) of
	true ->
	    gen_server:call(?SERVER, {set_samples,ID,Index,Samples});
	false ->
	    error(badarg)
    end.

set_samples(ID, Index, Header, Samples) ->
    case is_voiceid(ID) andalso is_header(Header) 
	andalso is_data(Samples) of
	true ->
	    gen_server:call(?SERVER, {set_samples,ID,Index,Header,
				      Samples});
	false ->
	    error(badarg)
    end.

set_file(ID, Index, Filename) ->
    case is_voiceid(ID) andalso is_filename(Filename) of
	true ->
	    case alsa_util:read_file(Filename) of
		{ok,{Header,Samples}} ->
		    io:format("load ~w samples format=~p\n",
			      [byte_size(Samples), Header]),
		    set_samples(ID, Index, Header, Samples);
		Error ->
		    io:format("file error: ~p\n", [Error]),
		    Error
	    end;
	false ->
	    error(badarg)
    end.

insert(ID, Index, Pos, Samples) ->
    insert_samples(ID, Index, Pos, undefined, Samples).
insert(ID, Index, Pos, Header, Samples) ->
    insert_samples(ID, Index, Pos, Header, Samples).

insert_samples(ID, Index, Pos, Header, Samples) ->
    case is_voiceid(ID) andalso is_pos(Pos) andalso
	is_header(Header) andalso is_data(Samples) of
	true ->
	    gen_server:call(?SERVER, {insert_samples,ID,Index,
				      Pos,Header,Samples});
	false ->
	    error(badarg)
    end.

insert_file(ID, Index, Filename) ->
    insert_file(ID, Index, cur, Filename).

insert_file(ID, Index, Pos, Filename) ->
    case is_voiceid(ID) andalso 
	is_pos(Pos) andalso 
	is_filename(Filename) of
	true ->
	    case alsa_util:read_file(Filename) of
		{ok,{Header,Samples}} ->
		    io:format("load ~w samples format=~p\n",
			      [byte_size(Samples), Header]),
		    gen_server:call(?SERVER, {insert_samples,ID,Index,
					      Pos,Header,Samples});
		Error ->
		    io:format("file error: ~p\n", [Error]),
		    Error
	    end;
	false ->
	    error(badarg)
    end.

append(ID, Index, Samples) ->
    insert(ID, Index, eof, Samples).

append(ID, Index, Header, Samples) ->
    insert(ID, Index, eof, Header, Samples).

append_file(ID, Index, Filename) ->
    insert_file(ID, Index, eof, Filename).

-spec mark(ID::voiceid(), UserData::term()) ->
    {ok, Ref::reference()} | {error, Reason::term()}.
mark(ID, UserData) -> mark(ID, cur, [], UserData).

-spec mark(ID::voiceid(), Flags::[alsa_samples:event_flag()],
      UserData::term()) ->
    {ok, Ref::reference()} | {error, Reason::term()}.
mark(ID, Flags, UserData) -> mark(ID, cur, Flags, UserData).

-spec mark(ID::voiceid(), Pos::sample_position(),
	   Flags::[alsa_samples:event_flag()], UserData::term()) ->
	  {ok, Ref::reference()} | {error, Reason::term()}.
mark(ID, Pos, Flags, UserData) ->
    case is_voiceid(ID) andalso
	is_pos(Pos) andalso
	is_mark_flags(Flags) of
	true ->
	    gen_server:call(?SERVER, {mark,self(),ID,Pos,Flags,UserData});
	false ->
	    error(badarg)
    end.

-spec unmark(ID::voiceid(), Ref::reference()) ->
	  ok | {error, Reason::term()}.
unmark(ID, Ref) ->
    case is_voiceid(ID) andalso
	is_reference(Ref) of
	true -> gen_server:call(?SERVER, {unmark, ID, Ref});
	false -> error(badarg)
    end.

%% delete samples in channel
delete(ID, Range) when ?is_voiceid(ID), is_list(Range) ->
    gen_server:call(?SERVER, {delete, ID, Range}).


update(U={ID,Update}) when ?is_voiceid(ID),is_list(Update) ->
    gen_server:call(?SERVER, {update,[U]});
update([]) ->
    ok;
update(IDList=[{ID,Update}|_]) when ?is_voiceid(ID), is_list(Update) ->
    gen_server:call(?SERVER, {update, IDList}).


restart(ID) when ?is_voiceid(ID) ->
    gen_server:call(?SERVER, {restart, ID});
restart([]) ->
    ok;
restart(IDList) when is_list(IDList), ?is_voiceid(hd(IDList)) ->
    gen_server:call(?SERVER, {restart, IDList}).
restart() ->
    gen_server:call(?SERVER, restart).

clear(ID) when ?is_voiceid(ID) ->
    gen_server:call(?SERVER, {clear, ID});
clear([]) ->
    ok;
clear(IDList) when is_list(IDList), ?is_voiceid(hd(IDList)) ->
    gen_server:call(?SERVER, {clear, IDList}).
clear() ->
    gen_server:call(?SERVER, clear).


mute(ID, On) when ?is_voiceid(ID), is_boolean(On) ->
    gen_server:call(?SERVER, {mute, ID, On});
mute([],_On) ->
    ok;
mute(IDList, On) when is_list(IDList), ?is_voiceid(hd(IDList)), 
		      is_boolean(On) ->
    gen_server:call(?SERVER, {mute, IDList, On}).

run(ID) when ?is_voiceid(ID) ->
    gen_server:call(?SERVER, {run, ID});
run([]) ->
    ok;
run(IDList) when is_list(IDList), ?is_voiceid(hd(IDList)) ->
    gen_server:call(?SERVER, {run, IDList}).
run() ->
    gen_server:call(?SERVER, run).

stop(ID) when ?is_voiceid(ID) ->
    gen_server:call(?SERVER, {stop, ID});
stop([]) ->
    ok;
stop(IDList) when is_list(IDList), ?is_voiceid(hd(IDList)) ->
    gen_server:call(?SERVER, {stop, IDList}).
stop() ->
    gen_server:call(?SERVER, stop).

resume() ->
    gen_server:call(?SERVER, resume).

pause() ->
    gen_server:call(?SERVER, pause).

set_callback(Fun) when is_function(Fun, 3) ->
    gen_server:call(?SERVER, {set_callback, Fun, undefined}).

set_callback(Fun,Args) when is_function(Fun, 4), is_list(Args) ->
    gen_server:call(?SERVER, {set_callback, Fun, Args}).
%% handy to update context info!!! 
set_callback_args(Args) when is_list(Args) ->
    gen_server:call(?SERVER, {set_callback_args, Args}).


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

start() -> start(#{}).

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
	    ?dbg("Alsa open: params = ~p\n", [Params]),
	    Fun = maps:get(callback, Options, undefined),
	    Args = maps:get(callback_args, Options, undefiined),
	    {ok, #state{ handle = H,
			 params = Params,
			 voice_map = #{},
			 owner_map = #{},
			 free_list = lists:seq(1,?MAX_VOICES),
			 callback = Fun,
			 callback_args = Args
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

handle_call({alloc, Pid, Options}, _From, State) ->
    case State#state.free_list of
	[ID|Free] ->
	    VoiceMap = State#state.voice_map,
	    OwnerMap = State#state.owner_map,
	    Ref = erlang:monitor(process, Pid),
	    W   = create_wave(State#state.params, Options),
	    {reply, ID, 
	     State#state { voice_map = VoiceMap#{ ID => W },
			   owner_map = OwnerMap#{ ID => Ref, Ref => ID },
			   free_list = Free }};
	[] ->
	    {reply, {error, enoent}, State}
    end;

handle_call({new, ID, Options}, _From, State) ->
    VoiceMap = State#state.voice_map,
    case maps:get(ID, VoiceMap, undefined) of
	undefined ->
	    Free = lists:delete(ID, State#state.free_list),
	    W    = create_wave(State#state.params, Options),
	    {reply, ok, State#state { voice_map = VoiceMap#{ ID => W },
				      free_list = Free }};
	_Buf ->
	    {reply, {error, ealready}, State}
    end;

handle_call(get_allocated_list, _From, State) ->
    L = maps:fold(fun(ID, _, Acc) -> [ID|Acc] end, [], State#state.voice_map),
    {reply, L, State};

handle_call(get_free_list, _From, State) ->
    {reply, State#state.free_list, State};

handle_call({set_wave,ID,WaveDef}, _From, State) ->
    case find_voice(ID, State) of
	Error = {error,_} ->
	    {reply, Error, State};
	W ->
	    alsa_samples:set_wave_def(W, WaveDef),
	    {reply, ok, State}
    end;

handle_call({set_volume,ID,Volume}, _From, State) ->
    case find_voice(ID, State) of
	Error = {error,_} ->
	    {reply, Error, State};
	W ->
	    %% fixme: multi channel > 2
	    alsa_samples:wave_set_volume(W, 0, Volume),
	    alsa_samples:wave_set_volume(W, 1, Volume),
	    {reply, ok, State}
    end;

handle_call({insert_samples,ID,Index,Pos,Header,Samples}, _From, State) ->
    handle_set_samples(ID, Index, Pos, Header, Samples, false, State);

handle_call({set_samples,ID,Index,Header,Samples}, _From, State) ->
    handle_set_samples(ID, Index, bof, Header, Samples, true, State);

handle_call({mark,Pid,ID,Pos,Flags,UserData}, _From, State) ->
    case find_voice(ID, State) of
	Error = {error,_} ->
	    {reply, Error, State};
	W ->
	    Ref = erlang:monitor(process, Pid),
	    Pos1 = pos(W, Pos),
	    Flags1 = flags(W, Flags),
	    alsa_samples:mark(W, Pid, Ref, Pos1, Flags1, UserData),
	    {reply, {ok,Ref}, State}
    end;

handle_call({unmark,ID,Ref}, _From, State) ->
    case find_voice(ID, State) of
	Error = {error,_} ->
	    {reply, Error, State};
	W ->
	    alsa_samples:unmark(W, Ref),
	    erlang:demonitor(Ref, [flush]),
	    {reply, ok, State}
    end;

handle_call({update,UList}, _From, State) ->
    %% UList is on form [ {ID,[{volume,V},restart,clear,mute,run]} ]
    R = each_wave(UList, 
		  fun({W,U}) ->
			  lists:foreach(
			    fun({volume,V}) ->
				    alsa_samples:wave_set_volume(W, 0, V),
				    alsa_samples:wave_set_volume(W, 1, V);
			       (restart) ->
				    alsa_samples:wave_set_time(W,0);
			       (clear) ->
				    alsa_samples:wave_clear(W);
			       ({mute,On}) ->
				    alsa_samples:wave_set_mute(W,On);
			       (run) ->
				    alsa_samples:wave_set_state(W, running);
			       (stop) ->
				    alsa_samples:wave_set_state(W, stopped)
			    end, U)
		  end, State),
    {reply, R, State};

handle_call({restart,ID}, _From, State) ->
    R = each_wave(ID, fun(W) -> alsa_samples:wave_set_time(W,0) end, State),    
    {reply, R, State};

%% clear one channel, but keep structure
handle_call({clear,ID}, _From, State) ->
    R = each_wave(ID, fun(W) -> alsa_samples:wave_clear(W) end, State),
    {reply, R, State};

%% clear all voice_maps
handle_call(clear, _From, State) ->
    R = each_wave(State#state.voice_map,
		  fun(W) -> alsa_samples:wave_clear(W) end, State),
    {reply, R, State};

handle_call({mute,ID,On}, _From, State) ->
    R = each_wave(ID, fun(W) -> alsa_samples:wave_set_mute(W,On) end, State),
    {reply, R, State};

handle_call(run, From, State) ->
    set_wave_state(State#state.voice_map, running, State),
    gen_server:reply(From, ok),
    State1 = continue_playing(State),
    {noreply, State1};

handle_call({run,IDList}, From, State) -> %% ID or List of ID
    set_wave_state(IDList, running, State),
    gen_server:reply(From, ok),
    State1 = continue_playing(State),
    {noreply, State1};

handle_call({stop,IDList}, _From, State) -> %% ID or List of ID
    set_wave_state(IDList, stopped, State),
    State1 = continue_playing(State),
    {reply, ok, State1};

handle_call(stop, _From, State) ->
    set_wave_state(State#state.voice_map, stopped, State),    
    {reply, ok, State};

handle_call({remove,ID}, _From, State) ->
    case maps:take(ID, State#state.voice_map) of
	error ->
	    {reply, ok, State};
	{W, VoiceMap} ->
	    Playing = VoiceMap =/= #{},
	    alsa_samples:wave_clear(W),
	    OwnerMap = remove_owner(ID, State#state.owner_map),
	    {reply, ok, State#state { voice_map = VoiceMap,
				      owner_map = OwnerMap,
				      playing = Playing
				    }}
    end;

handle_call(remove, _From, State) ->
    {reply, ok,
     maps:fold(
       fun(ID, W, Si) ->
	       %% check if ID is not in owner map then clear and free
	       case maps:take(ID, Si#state.owner_map) of
		   error ->
		       Free = [ID|Si#state.free_list],
		       alsa_samples:wave_clear(W),
		       VoiceMap = maps:remove(ID, Si#state.voice_map),
		       Playing = VoiceMap =/= #{},
		       Si#state { free_list=Free,
				  voice_map = VoiceMap,
				  playing = Playing };
		   _ ->
		       Si
	       end
       end, State, State#state.voice_map)};

handle_call({delete,ID,Range}, _From, State) ->
    case find_voice(ID, State) of
	Error = {error,_} ->
	    {reply, Error, State};
	W ->
	    Reply = alsa_samples:wave_delete_samples(W, Range),
	    {reply, Reply, State}
    end;

handle_call(restart, _From, State) ->
    maps_foreach(
      fun(_ID, W) ->
	      alsa_samples:wave_set_time(W, 0)
      end, State#state.voice_map),
    {reply, ok, State};

handle_call(pause, _From, State) ->
    case State#state.pause of
	true ->
	    {reply, ok, State};
	false ->
	    {reply, ok, State#state { pause = true }}
    end;
handle_call(resume, From, State) ->
    case State#state.pause of
	true ->
	    gen_server:reply(From, ok),
	    State1 = continue_playing(State#state{pause=false}),
	    %% io:format("resume: playing=~p\n", [State1#state.playing]),
	    {noreply, State1};
	false ->
	    {reply, ok, State}
    end;

handle_call({set_callback, Fun, Args}, _From, State) ->
    {reply, ok, State#state { callback = Fun, callback_args = Args }};
handle_call({set_callback_args, Args}, _From, State) ->
    {reply, ok, State#state { callback_args = Args }};
handle_call(get_params, _From, State) ->
    {reply, State#state.params, State};

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
	    %% ?dbg("select: eagain\n",[]),
	    ok = alsa:select_(Handle),
	    {norereply, State};
	{error, epipe} -> 
	    ?dbg("select: epipe\n",[]),
	    alsa:recover_(Handle, epipe),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{error, estrpipe} ->
	    ?dbg("play: estrpipe\n",[]),
	    alsa:recover_(Handle, estrpipe),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{error, AlsaError} when is_integer(AlsaError) ->
	    _Error =  alsa:strerror(AlsaError),
	    ?dbg("play: error ~s\n", [_Error]),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{ok, Written} ->
	    user_callback(State#state.output, Written, State),
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
%% remove allocated channels
handle_info({'DOWN',Ref,process,_Pid,_Reason}, State) ->
    case maps:take(Ref, State#state.owner_map) of
	error -> %% not allocated, ignore
	    {noreply, State};
	{ID, OwnerMap1} ->
	    Free = [ID|State#state.free_list],
	    case maps:take(ID, State#state.voice_map) of
		error -> %% crash?
		    {noreply, State};
		{W, VoiceMap} ->
		    Playing = VoiceMap =/= #{},
		    alsa_samples:wave_clear(W),
		    OwnerMap = maps:remove(ID, OwnerMap1),
		    {noreply, State#state { voice_map = VoiceMap,
					    owner_map = OwnerMap,
					    free_list = Free,
					    playing = Playing
					  }}
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

set_wave_state(IDList, RunState, State) ->
    each_wave(IDList, 
	      fun (W) -> alsa_samples:wave_set_state(W, RunState) end,
	      State).

each_wave([ID|IDList], Fun, State) ->
    do_wave_(ID, Fun, State),
    each_wave(IDList, Fun, State);
each_wave([], _Fun, _State) ->
    ok;
each_wave(ID, Fun, State) when ?is_voiceid(ID) ->
    do_wave_(ID, Fun, State);
each_wave(VoiceMap, Fun, _State) when is_map(VoiceMap) ->
    maps_foreach(fun (_ID, W) -> Fun(W) end, VoiceMap).

do_wave_({ID,UList}, Fun, State) ->
    case find_voice(ID, State) of
	Error = {error,_} -> Error;
	W -> Fun({W,UList})
    end;
do_wave_(ID, Fun, State) ->
    case find_voice(ID, State) of
	Error = {error,_} -> Error;
	W -> Fun(W)
    end.

find_voice(ID, State) ->
    case maps:get(ID, State#state.voice_map, undefined) of
	undefined ->
	    {error, enoent};
	W -> 
	    W
    end.

remove_owner(ID, OwnerMap) ->
    case maps:get(ID, OwnerMap,undefined) of
	undefined ->
	    OwnerMap;
	Ref -> 
	    erlang:demonitor(Ref, [flush]),
	    OwnerMap1 = maps:remove(ID, OwnerMap),
	    maps:remove(Ref, OwnerMap1)
    end.

create_wave(Params, Options) ->
    Rate = maps:get(rate, Params, 16000),
    W = alsa_samples:wave_new(),
    alsa_samples:wave_set_rate(W, Rate),
    alsa_samples:wave_set_buffer_mode(W, ?DEFAULT_WAVE_NUM, Options),
    W.

continue_playing(State) ->
    if State#state.pause -> State;
       State#state.playing -> State;
       true -> play(State#state{playing=true})
    end.

handle_set_samples(ID, Index, Pos, Header0, Samples, Clear, State) ->
    Header = if Header0 =:= undefined -> State#state.params; 
		true -> Header0 
	     end,
    VoiceMap = State#state.voice_map,
    case maps:get(ID, VoiceMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	W ->
	    Rate = maps:get(rate, Header, 16000),
	    Format = maps:get(format, Header, s16_le),
	    Channels = maps:get(channels, Header, 1),
	    Pos1 = pos(W, Pos),
	    Samples1 = samples_to_binary(W, Header, Samples),
	    ?dbg("voice=~w,src_rate=~w, src_format=~w, scr_channels=~w, pos=~w\n",
		      [ID, Rate, Format, Channels, Pos1]),
	    ?dbg("#samples = ~w\n", [byte_size(Samples1) div
					     alsa:format_size(Format,
							      Channels)]),
	    ChanMask = (1 bsl Channels)-1,  %% all channels
%%	    io:format("chanmask = ~w\n", [ChanMask]),
	    write_samples(ChanMask, Clear, W, Index, Pos1, 0, 
			  Rate, Format, Channels, Samples1),
%%	    io:format("Num samples[~w] = ~w\n",
%%		      [Index,alsa_samples:wave_get_num_samples(W, Index)]),
%%	    io:format("Num samples[~w] = ~w\n",
%%		      [Index+1,alsa_samples:wave_get_num_samples(W, Index+1)]),
	    {reply, ok, State}
    end.

%% write samples in selected channels on Index Index+1 ...
write_samples(ChanMask, Clear, W, Index, Pos,
	      Chan, Rate, Format, Channels, Samples) when 
      is_integer(Index), ChanMask =/= 0 ->
    if ChanMask band 1 =:= 1 ->
	    if Clear ->
		    io:format("clear\n"),
		    alsa_samples:wave_set_num_samples(W, Index, 0);
	       true ->
		    ok
	    end,
	    %%io:format("set samples index=~w, chan=~w\n", [Index,Chan]),
	    alsa_samples:wave_set_samples(W, Index, Pos, Chan, Rate, Format, 
					  Channels, Samples),
	    %%io:format("set channel index=~w, chan=~w\n", [Index,Chan]),
	    alsa_samples:wave_set_chan(W, Index, Chan);
       true ->
	    ignore
    end,
    write_samples(ChanMask bsr 1, Clear, W, Index+1, Pos,
		  Chan+1, Rate, Format, Channels, Samples);
write_samples(_ChanMask, Clear, W, Index, Pos,
	      Chan, Rate, Format, Channels, Samples) when 
      is_atom(Index) ->
    %% FIXME: at least stereo!?
    if Clear ->
	    %%io:format("clear\n"),
	    alsa_samples:wave_set_num_samples(W, Index, 0);
       true ->
	    ok
    end,
    %% io:format("set samples index=~w, chan=~w\n", [Index,Chan]),
    alsa_samples:wave_set_samples(W, Index, Pos, Chan, Rate, Format, 
				  Channels, Samples);
write_samples(0, _Clear, _W, _Index, _Pos, _Chan, _Rate, _Format, 
	      _Channels, _Samples) ->
    ok.


samples_to_binary(_W,_Header,Data) when is_binary(Data) ->
    Data;
samples_to_binary(_W,_Header,Data) when is_list(Data) ->
    iolist_to_binary(Data);
samples_to_binary(W, #{ format := Format, channels := Channels},
		  {silence, Len}) ->
    alsa:make_silence(Format, Channels, len(W,Len)).


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
pos(W,{eof,Offs}) -> pos_eof(W) + len(W, Offs);
pos(_W,Lbl={label,_}) -> Lbl.

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
notify([{ID,Ms}|Marks], State) ->
    lists:foreach(
      fun({Ref,Pid,Pos,UserData}) ->
	      Event = {Ref,ID,Pos,UserData},
	      ?dbg("notify ~p\n", [Event]),
	      Pid ! Event
      end, Ms),
    notify(Marks, State).

play(State = #state {output=undefined,pause=true}) ->
    %% io:format("playing=false\n"),
    State#state{ playing=false };
play(State = #state {handle=Handle,output=undefined,
		     params=Params,voice_map=VoiceMap }) ->
    PeriodSize = maps:get(period_size, Params),
    Channels = maps:get(channels, Params),
    Format = maps:get(format, Params),
    case read_buffer_list(VoiceMap, Format, Channels, PeriodSize) of
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
		    %% ?dbg("play: again\n", []),
		    ok = alsa:select_(Handle),
		    State#state{ playing=Playing, output=Bin, marks=Marks };
		{error, epipe} -> 
		    ?dbg("play: epipe\n", []),
		    alsa:recover_(Handle, epipe),
		    play(State#state{playing=Playing,output=Bin,marks=Marks});
		{error, estrpipe} ->
		    ?dbg("play: estrpipe\n",[]),
		    alsa:recover_(Handle, estrpipe),
		    play(State#state{playing=Playing,output=Bin,marks=Marks});
		{error, AlsaError} when is_integer(AlsaError) ->
		    _Error =  alsa:strerror(AlsaError),
		    ?dbg("play: error ~s\n", [_Error]),
		    play(State#state{playing=Playing,output=Bin,marks=Marks});
		{ok, Written} ->
		    ?dbg("play: written ~w, remain=~w\n", 
			     [Written, byte_size(Bin)]),
		    user_callback(Bin, Written, State),
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
	    %% ?dbg("play1: again\n",[]),
	    ok = alsa:select_(Handle),
	    State;
	{error, epipe} ->
	    ?dbg("play1: epipe\n", []),
	    alsa:recover_(Handle, epipe),
	    play(State);
	{error, estrpipe} ->
	    ?dbg("play1: estrpipe\n", []),
	    alsa:recover_(Handle, estrpipe),
	    play(State);
	{error, AlsaError} when is_integer(AlsaError) ->
	    _Error =  alsa:strerror(AlsaError),
	    ?dbg("play1: error ~s\n", [_Error]),	    
	    play(State);
	{ok, Written} ->
	    user_callback(Bin, Written, State),
	    ?dbg("play1: written ~w, remain=~w\n", 
		     [Written, byte_size(Bin)]),
	    if Written =:= byte_size(Bin) ->
		    State1 = notify(Marks, State),
		    play(State1#state{output=undefined,marks=[]});
	       true ->
		    <<_:Written/binary, Bin1/binary>> = Bin,
		    play(State#state{output=Bin1})
	    end
    end.

user_callback(_Data, _NumBytes, #state{ callback=undefined}) ->
    ok;
user_callback(Data, NumBytes, #state{ callback=Fun, callback_args=undefined,
				      params=Params}) ->
    Fun(Data, NumBytes, Params);
user_callback(Data, NumBytes, #state{ callback=Fun, callback_args=Args,
				      params=Params}) ->
    Fun(Data, NumBytes, Params, Args).


read_buffer_list(VoiceMap, Format, Channels, PeriodSize) ->
    read_buffer_list_(maps:keys(VoiceMap), VoiceMap, 
		      Format, Channels, PeriodSize, [], []).

read_buffer_list_([ID|IDs], VoiceMap, Format, Channels, PeriodSize,
		  Acc, Marks) ->
    case maps:get(ID, VoiceMap, undefined) of
	undefined ->
	    read_buffer_list_(IDs, VoiceMap, Format, Channels, 
			      PeriodSize, Acc, Marks);
	W ->
	    case alsa_samples:wave(W, Format, Channels, PeriodSize) of
		{Ms,_,<<>>} ->
		    read_buffer_list_(IDs, VoiceMap, Format, Channels, 
				      PeriodSize, Acc, 
				      add_marks(ID,Ms,Marks));
		{Ms,#{peak := _P, energy := _E},Samples} ->
		    %% io:format("peak=~p, energy=~p\n", [_P, _E]),
		    read_buffer_list_(IDs, VoiceMap, Format, Channels,
				      PeriodSize, [Samples|Acc], 
				      add_marks(ID,Ms,Marks))
	    end
    end;
read_buffer_list_([], _VoiceMap, _Format, _Channels, _PeriodSize, Acc, Marks) ->
    {Acc, Marks}.

add_marks(_ID,[],Marks) -> Marks;
add_marks(ID,Ms,Marks) -> [{ID,Ms}|Marks].

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
is_pos({label,Lbl}) -> is_integer(Lbl);
is_pos(Len) -> is_len(Len).

is_len({time,T}) when is_number(T) -> true;
is_len(Len) -> is_integer(Len).

is_voiceid(Chan) -> is_integer(Chan).
    
is_data({silence,Len}) -> is_len(Len);
is_data(Data) when is_binary(Data) -> true;
is_data(List) when is_list(List) -> true;
is_data(_) -> false.

is_header(undefined) -> true;
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
is_mark_flags([{label,Lbl}|Fs]) -> is_integer(Lbl) andalso is_mark_flags(Fs);
is_mark_flags([{set,Pos}|Fs]) -> is_pos(Pos) andalso is_mark_flags(Fs);
is_mark_flags([{repeat,Pos,Num}|Fs]) -> is_pos(Pos) andalso 
					is_integer(Num) andalso
					Num >= 0 andalso
					is_mark_flags(Fs);
is_mark_flags([]) -> true;
is_mark_flags(_) -> false.

%% compat 
maps_foreach(Fun, Map) ->
    maps:fold(fun(K, V, _) -> Fun(K, V) end, [], Map).
