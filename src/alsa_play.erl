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
-define(MAX_CHANNELS, 8).
-define(is_channel(C), (is_integer((C)) andalso ((C)>=0) 
			andalso ((C)<?MAX_CHANNELS))).

-export([new/1]).
-export([mute/2, loop/2]).
-export([insert/2, insert/3, insert/4]).
-export([insert_file/2, insert_file/3]).
-export([append/2, append/3]).
-export([append_file/2]).
-export([mark/2, mark/3, mark/4]).
-export([unmark/2]).
-export([delete/2]).
-export([run/1, run/0]).
-export([stop/1, stop/0]).
-export([reset/1, clear/1, remove/1]).
-export([reset/0, clear/0, remove/0]).
-export([resume/0, pause/0]).

-export([test/0, 
	 test/1, 
	 test_pong/0, 
	 test_notify/0,
	 test_notify_once/0,
	 test_notify_once_one/0,
	 test_notify_music/0
	]).

-define(verbose(F), ok).
%% -define(verbose(F), io:format((F),[])).
-define(verbose(F,A), ok).
%% -define(verbose(F,A), io:format((F),(A))).
%%-define(info(F,A), ok).
-define(info(F,A), io:format((F),(A))).

test() ->
    test("default").
test(Device) ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    start(#{device=>Device, rate=>16000 }),
    new(1),
    new(2),
    new(3),
    append_file(1, filename:join(Sounds, "Front_Center.wav")),
    append_file(2, filename:join(Sounds, "Front_Left.wav")),
    append_file(2, filename:join(Sounds, "Front_Right.wav")),
    append_file(3, filename:join(Sounds, "Rear_Right.wav")),
    loop(1, true),
    loop(2, true),
    loop(3, true),
    run(),
    resume().

test_pong() ->
    start(#{}),
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    new(1),
    mute(1, true),
    append_file(1, filename:join(Sounds, "plop.wav")),   
    new(2),
    mute(2, true),
    append_file(2, filename:join(Sounds, "beep.wav")),
    new(3),
    mute(3, true),
    append_file(3, filename:join(Sounds, "Side_Left.wav")),
    run(),
    resume(),
    timer:sleep(500),

    lists:foreach(
      fun(_) ->
	      mute(1, false),
	      reset(1),
	      timer:sleep(500), %% listen to plop
	      mute(1, true),

	      mute(2, false),
	      reset(2),
	      timer:sleep(500),  %% listen to beep
	      mute(2, true),

	      mute(3, false),
	      reset(3),
	      timer:sleep(2000),  %% listen to Side_Left
	      mute(3, true)
      end, lists:seq(1, 4)),

    loop(1, true),
    loop(2, true),
    mute(1, false),
    mute(2, false),
    mute(3, false),
    reset(3),
    
    timer:sleep(2000),  %% listen to all

    alsa_play:pause(),

    remove(1),
    remove(2),
    remove(3),
    ok.

test_notify() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    start(#{ rate => 16000 }),
    new(1),
    new(2),
    new(3),
    append_file(1, filename:join(Sounds, "Front_Center.wav")),
    append(1, {silence, {time,100}}),
    append_file(2, filename:join(Sounds, "Front_Left.wav")),
    append(2, {silence, {time,100}}),
    append_file(3, filename:join(Sounds, "Front_Right.wav")),
    append(3, {silence, {time,100}}),
    {ok,Ref1} = mark(1, {eof,-1}, [], center_done),
    {ok,Ref2} = mark(2, {eof,-1}, [], left_done),
    {ok,Ref3} = mark(3, {eof,-1}, [], right_done),
    run(1),
    resume(),
    receive
	{Ref1, 1, _Pos1, _Flags1, center_done} ->
	    run(2)
    end,
    receive
	{Ref2, 2, _Pos2, _Flags2, left_done} ->
	    run(3)
    end,
    receive
	{Ref3, 3, _Pos3, _Flags3, right_done} ->
	    ok
    end,
    alsa_play:pause(),
    unmark(1, Ref1),
    unmark(2, Ref2),
    unmark(3, Ref3),
    remove(1),
    remove(2),
    remove(3),
    ok.

%% play each wav file in sequence in different channels 1,2,3,
%% reset channel and remove mark after each completed play
test_notify_once() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    start(#{ rate => 16000 }),
    new(1),
    new(2),
    new(3),
    append_file(1, filename:join(Sounds, "Front_Center.wav")),
    append(1, {silence, {time,100}}),
    append_file(2, filename:join(Sounds, "Front_Left.wav")),
    append(2, {silence, {time,100}}),
    append_file(3, filename:join(Sounds, "Front_Right.wav")),
    append(3, {silence, {time,100}}),

    {ok,Ref1} = mark(1, {eof,-1}, [once], sample_played),
    run(1),
    resume(),
    receive
	{Ref1, 1, _Pos1, _Flags1, sample_played} ->
	    ok
    end,

    {ok,Ref2} = mark(2, {eof,-1}, [once], sample_played),
    run(2),
    receive
	{Ref2, 2, _Pos2, _Flags2, sample_played} ->
	    ok
    end,

    {ok,Ref3} = mark(3, {eof,-1}, [once], sample_played),
    run(3),
    receive
	{Ref3, 3, _Pos3, _Flags3, sample_played} ->
	    ok
    end,
    pause(),
    remove(1),
    remove(2),
    remove(3),
    ok.


%% play each wav file in sequence in the same channels 1
%% remove mark after each completed play
test_notify_once_one() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    start(#{ rate => 16000 }),
    new(1),
    append_file(1, filename:join(Sounds, "Front_Center.wav")),
    %% append(1, {silence, {time,100}}),
    {ok,Ref1} = mark(1, {eof,-1}, [once,stop], sample1_played),

    append_file(1, filename:join(Sounds, "Front_Left.wav")),
    %% append(2, {silence, {time,100}}),
    {ok,Ref2} = mark(1, {eof,-1}, [once,stop], sample2_played),
    append_file(1, filename:join(Sounds, "Front_Right.wav")),
    %% append(3, {silence, {time,100}}),
    {ok,Ref3} = mark(1, {eof,-1}, [once,stop], sample3_played),

    run(1),
    resume(),
    receive
	{Ref1, 1, _Pos1, _Flags1, sample1_played} ->
	    ok
    end,
    run(1),
    pause(),resume(),
    receive
	{Ref2, 1, _Pos2, _Flags2, sample2_played} ->
	    ok
    end,
    run(1),
    pause(),resume(),
    receive
	{Ref3, 1, _Pos3, _Flags3, sample3_played} ->
	    ok
    end,
    pause(),
    remove(1),
    ok.

test_notify_music() ->    
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    start(#{ rate => 16000 }),
    new(1),
    append_file(1, filename:join(Sounds, "POL-super-match-short.wav")),
    {ok,Ref1} = mark(1, {eof,-1}, [reset], "Music"),
    new(2),
    append(2, {silence, {time,500}}),
    append_file(2, filename:join(Sounds, "Front_Left.wav")),
    append(2, {silence, {time,1000}}),
    {ok,Ref2} = mark(2, {eof,-1}, [reset], "Left"),
    new(3),
    append(3, {silence, {time,1000}}),
    append_file(3, filename:join(Sounds, "Front_Right.wav")),
    append(3, {silence, {time,500}}),
    {ok,Ref3} = mark(3, {eof,-1}, [reset], "Right"),

    erlang:start_timer(20000, self(), stop),    
    run(),
    resume(),
    test_notify_loop(),
    unmark(1,Ref1),unmark(2,Ref2),unmark(3,Ref3),
    remove(1),remove(2),remove(3),
    ok.



test_notify_loop() ->
    receive
	{_Ref, _Chan, _Pos, _Flags, What} ->
	    io:format("~s\n", [What]),
	    test_notify_loop();
	{timeout, _, stop} ->
	    pause(),
	    ok
    end.
    
    
    

new(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {new, Channel}).

insert(Channel, Samples) ->
    insert(Channel, cur, Samples).

insert(Channel, Pos, Samples) ->
    case is_channel(Channel) andalso
	is_pos(Pos) andalso
	is_data(Samples) of
	true ->
	    gen_server:call(?SERVER, {insert,Channel,Pos,Samples});
	false ->
	    error(badarg)
    end.

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

insert_file(Channel, Filename) ->
    insert_file(Channel, cur, Filename).

insert_file(Channel, Pos, Filename) ->
    case is_channel(Channel) andalso 
	is_pos(Pos) andalso 
	is_filename(Filename) of
	true ->
	    gen_server:call(?SERVER, {insert_file,Channel,Pos,Filename});
	false ->
	    error(badarg)
    end.

append(Channel, Samples) ->
    insert(Channel, eof, Samples).

append(Channel, Header, Samples) ->
    insert(Channel, eof, Header, Samples).

append_file(Channel, Filename) ->
    insert_file(Channel, eof, Filename).

mark(Channel, UserData) -> mark(Channel, cur, [], UserData).
mark(Channel, Flags, UserData) -> mark(Channel, cur, Flags, UserData).
mark(Channel, Pos, Flags, UserData) ->
    case is_channel(Channel) andalso
	is_pos(Pos) andalso
	is_mark_flags(Flags) of
	true ->
	    gen_server:call(?SERVER, {mark,self(),Channel,Pos,Flags,UserData});
	false ->
	    error(badarg)
    end.

unmark(Channel, Ref) ->
    case is_channel(Channel) andalso
	is_reference(Ref) of
	true -> gen_server:call(?SERVER, {unmark, Channel, Ref});
	false -> error(badarg)
    end.

%% delete samples in channel
delete(Channel, Range) when ?is_channel(Channel), is_list(Range) ->
    gen_server:call(?SERVER, {delete, Channel, Range}).

reset(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {reset, Channel}).

clear(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {clear, Channel}).

remove(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {remove, Channel}).

remove() ->
    gen_server:call(?SERVER, remove).

mute(Channel, On) when ?is_channel(Channel), is_boolean(On) ->
    gen_server:call(?SERVER, {mute, Channel, On}).

loop(Channel, On) when ?is_channel(Channel), is_boolean(On) ->
    gen_server:call(?SERVER, {loop, Channel, On}).

run(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {run, Channel}).

stop(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {stop, Channel}).

reset() ->
    gen_server:call(?SERVER, reset).

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

-record(state, 
	{
	 handle :: alsa:handle(),
	 params :: #{},  %% alsa open params
	 pause = true :: boolean(),
	 channels :: #{ integer() => alsa_buffer:sample_buffer()  },
	 output = undefined :: undefine | binary(),
	 marks = []      %% current marks
	}).

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

handle_call({new,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    case alsa_buffer:new(State#state.params) of
		{ok,Buf} ->
		    ChanMap1 = ChanMap#{ Channel => Buf },
		    {reply, ok, State#state { channels = ChanMap1 }};
		Error={error,_} ->
		    {reply, Error, State}
	    end;
	_Buf ->
	    {reply, {error, ealready}, State}
    end;

handle_call({insert,Channel,Pos,Samples}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:insert(Buf, Pos, Samples),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;
handle_call({insert,Channel,Pos,Header,Samples}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:insert(Buf, Pos, Header, Samples),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({insert_file,Channel,Pos,Filename}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:insert_file(Buf, Pos, Filename),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({mark,Pid,Channel,Pos,Flags,UserData}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Ref = erlang:monitor(process, Pid),
	    {_,Buf1} = alsa_buffer:mark(Buf, Pid, Ref, Pos, Flags, UserData),
	    {reply, {ok,Ref},
	     State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({unmark,Channel,Ref}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error,enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:unmark(Buf, Ref),
	    erlang:demonitor(Ref, [flush]),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({reset,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:reset(Buf),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({clear,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    %% FIXME: clean marks?
	    Buf1 = alsa_buffer:clear(Buf),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;    

handle_call({mute,Channel,On}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = case On of
		       true  -> alsa_buffer:mute(Buf);
		       false -> alsa_buffer:unmute(Buf)
		   end,
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({run,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:run(Buf),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({stop,Channel}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:stop(Buf),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({loop,Channel,On}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    case alsa_buffer:setopts(Buf,[{loop,On}]) of
		{ok,Buf1} ->
		    {reply, ok, State#state { channels = 
						  ChanMap#{ Channel => Buf1 }}};
		Error ->
		    {reply, Error, State}
	    end
    end;    

handle_call({remove,Channel}, _From, State) ->
    case maps:remove(Channel, State#state.channels) of
	#{} ->
	    {reply, ok, State#state { channels = #{}, pause = true }};
	ChanMap1 ->
	    {reply, ok, State#state { channels = ChanMap1 }}
    end;

handle_call(remove, _From, State) ->
    ChanMap1 = #{},
    {reply, ok, State#state { channels = ChanMap1, pause = true }};

handle_call({delete,Channel,Range}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:delete(Buf,Range),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call(reset, _From, State) ->
    ChanMap1 = maps:map(
		 fun(_Channel, Buf) ->
			 alsa_buffer:reset(Buf)
		 end, State#state.channels),
    {reply, ok, State#state { channels = ChanMap1 }};

handle_call(stop, _From, State) ->
    ChanMap1 = maps:map(
		 fun(_Channel, Buf) ->
			 alsa_buffer:stop(Buf)
		 end, State#state.channels),
    {reply, ok, State#state { channels = ChanMap1 }};

handle_call(run, _From, State) ->
    ChanMap1 = maps:map(
		 fun(_Channel, Buf) ->
			 alsa_buffer:run(Buf)
		 end, State#state.channels),
    {reply, ok, State#state { channels = ChanMap1 }};

%% FIXME: clean marks?
handle_call(clear, _From, State) ->
    ChanMap1 = maps:map(
		 fun(_Channel, Buf) ->
			 alsa_buffer:clear(Buf)
		 end, State#state.channels),
    {reply, ok, State#state { channels = ChanMap1 }};

handle_call(pause, _From, State) ->
    case State#state.pause of
	true ->
	    {reply, ok, State};
	false ->
	    {reply, ok, State#state { pause = true }}
    end;
%% FIXME: when output=binary we are selecting and should not play!
handle_call(resume, _From, State) ->
    case State#state.pause of
	true ->
	    State1 = play(State#state{pause=false}),
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

notify([], State) -> 
    State;
notify([{Channel,Ms}|Marks], State) ->
    State1 = 
	lists:foldl(
	  fun({Ref,{Pid,Pos,Flags,UserData}}, Si) ->
		  Sj = 
		      with_buffer(
			fun(Buf) ->
				lists:foldl(
				  fun(stop, Bi) ->
					  alsa_buffer:stop(Bi);
				     (once, Bi) ->
					  alsa_buffer:unmark(Bi, Ref);
				     (reset,Bi) ->
					  alsa_buffer:reset(Bi)
				  end, Buf, Flags)
			end, Channel, Si),
		  Event = {Ref,Channel,Pos,Flags,UserData},
		  io:format("notify ~p\n", [Event]),
		  Pid ! Event,
		  Sj
	  end, State, Ms),
    notify(Marks, State1).

with_buffer(Fun, Channel, State) ->
    ChanMap = State#state.channels,
    Buf = maps:get(Channel, ChanMap),
    Buf1 = Fun(Buf),
    ChanMap1 = ChanMap#{ Channel => Buf1 },
    State#state { channels = ChanMap1 }.


play(State = #state {handle=Handle,output=undefined,
		     params=Params,channels=ChanMap}) ->
    PeriodSize = maps:get(period_size, Params),
    Channels = maps:get(channels, Params),
    Format = maps:get(format, Params),
    case read_buffer_list(ChanMap, PeriodSize) of
	{ChanMap1, [], Marks} -> %% nothing to play do internal pause?
	    State1 = notify(Marks, State),
	    State1#state{ output=undefined, marks=[], channels=ChanMap1 };
	{ChanMap1,BufferList,Marks} ->
	    FrameSize = alsa:format_size(Format, Channels),
	    Bin = mix_buffers(BufferList, FrameSize, Format, Channels),
	    case alsa:write_(Handle, Bin) of
		{error, eagain} ->
		    ?verbose("play: again\n"),
		    ok = alsa:select_(Handle),
		    State#state{ output=Bin, marks=Marks, channels=ChanMap1 };
		{error, epipe} -> 
		    ?verbose("play: epipe\n"),
		    alsa:recover_(Handle, epipe),
		    play(State#state{output=Bin,marks=Marks,channels=ChanMap1});
		{error, estrpipe} ->
		    ?verbose("play: estrpipe\n"),
		    alsa:recover_(Handle, estrpipe),
		    play(State#state{output=Bin,marks=Marks,channels=ChanMap1});
		{error, AlsaError} when is_integer(AlsaError) ->
		    _Error =  alsa:strerror(AlsaError),
		    ?verbose("play: error ~s\n", [_Error]),
		    play(State#state{output=Bin,marks=Marks,channels=ChanMap1});
		{ok, Written} ->
		    ?verbose("play: written ~w, remain=~w\n", 
			     [Written, byte_size(Bin)]),
		    if Written =:= byte_size(Bin) ->
			    State1 = notify(Marks, State),
			    play(State1#state{output=undefined,
					     marks=[],
					     channels=ChanMap1});
		       true ->
			    <<_:Written/binary, Bin1/binary>> = Bin,
			    play(State#state{output=Bin1,marks=Marks,
					     channels=ChanMap1 })
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

read_buffer_list(ChanMap, PeriodSize) ->
    read_buffer_list_(maps:keys(ChanMap), ChanMap, PeriodSize, [], []).

read_buffer_list_([Channel|Cs], ChanMap, PeriodSize, Acc, Marks) ->
    Cb = maps:get(Channel, ChanMap),
    case alsa_buffer:read_with_marks(Cb, PeriodSize) of
	{<<>>, Cb1, Ms} -> %% probably stopped
	    read_buffer_list_(Cs, ChanMap#{ Channel => Cb1}, PeriodSize, 
			      Acc, add_marks(Channel,Ms,Marks));
	{Samples,Cb1,Ms} ->
	    read_buffer_list_(Cs, ChanMap#{ Channel => Cb1}, PeriodSize, 
			      [Samples|Acc], add_marks(Channel,Ms,Marks))
    end;
read_buffer_list_([], ChanMap, _PeriodSize, Acc, Marks) ->
    {ChanMap, Acc, Marks}.

add_marks(_Channel,[],Marks) -> Marks;
add_marks(Channel,Ms,Marks) -> [{Channel,Ms}|Marks].
    
    
mix_buffers([B], _FrameSize, _Format, _Channels) ->
    B;
mix_buffers([B|Bs], FrameSize, Format, _Channels) ->
    UBs0 = unpack_buffer(B, FrameSize, Format),
    UBs1 = mix_buffers_(Bs, UBs0, FrameSize, Format),
    list_to_binary(pack_buffer(UBs1, Format)).

mix_buffers_([B|Bs], UBs0, FrameSize, Format) ->
    UBs1 = unpack_buffer(B, UBs0, FrameSize, Format),
    mix_buffers_(Bs, UBs1, FrameSize, Format);
mix_buffers_([], UBs0, _FrameSize, _Format) ->
    UBs0.
    
pack_buffer([Frame|Fs], Format) ->
    [ alsa_util:encode_frame(Frame, Format) | pack_buffer(Fs, Format)];
pack_buffer([], _Format) ->
    [].

unpack_buffer(Buffer, FrameSize, Format) ->
    case Buffer of
	<<Frame:FrameSize/binary, Rest/binary>> ->
	    UB = alsa_util:decode_frame(Frame, Format),
	    [UB | unpack_buffer(Rest, FrameSize, Format)];
	_ ->
	    []
    end.

unpack_buffer(_Buffer, [], _FrameSize, _Format) ->
    [];
unpack_buffer(Buffer, [UB0|UBs], FrameSize, Format) ->
    case Buffer of
	<<Frame:FrameSize/binary, Rest/binary>> ->
	    UB = alsa_util:decode_frame(Frame, Format),
	    [add(UB0,UB) | unpack_buffer(Rest, UBs, FrameSize, Format)];
	_ ->
	    []
    end.

add(Xs, []) -> Xs;
add([X|Xs],[Y|Ys]) -> [X+Y|add(Xs, Ys)];
add([], []) -> [].


%%
%% check argument types
%%
%% Len = integer() | {time, number()}
%% Pos = bof|eof|cur|{bof,Len}|{eof,Len}|{cur,Len}|unsigned()
is_pos(bof) -> true;
is_pos(eof) -> true;
is_pos(cur) -> true;
is_pos(Pos) when is_integer(Pos), Pos >= 0 -> true;
is_pos({bof,Len}) -> is_len(Len);
is_pos({eof,Len}) -> is_len(Len);
is_pos({cur,Len}) -> is_len(Len);
is_pos(_) -> false.

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

is_mark_flags([once|Fs]) -> is_mark_flags(Fs);
is_mark_flags([stop|Fs]) -> is_mark_flags(Fs);
is_mark_flags([reset|Fs]) -> is_mark_flags(Fs);
is_mark_flags([]) -> true;
is_mark_flags(_) -> false.

    
	     
	    

    
    
