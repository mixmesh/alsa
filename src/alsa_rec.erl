%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    ALSA microphone input
%%% @end
%%% Created :  1 Nov 2022 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(alsa_rec).

-behaviour(gen_server).

%% API
-export([start/0, start/1]).
-export([start_link/0, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, format_status/2]).

-export([pause/0, resume/0]).
-export([set_callback/1, set_callback/2,set_callback_args/1]).
-export([get_params/0]).
%% util
-export([and_play/0, play/4]).
%% debug
-export([pulse_address/0]).

-include_lib("inpevt/include/inpevt.hrl").
-include_lib("dbus/include/dbus.hrl").

-define(SERVER, ?MODULE).

%% -define(verbose(F), ok).
%% -define(verbose(F,A), ok).
-define(verbose(F), io:format((F),[])).
-define(verbose(F,A), io:format((F),(A))).
%%-define(info(F,A), ok).
-define(info(F,A), io:format((F),(A))).
-define(error(F,A), io:format((F),(A))).
-define(warning(F), io:format((F))).
-define(warning(F,A), io:format((F),(A))).

-type callback3() ::
	fun((Samples::binary(),NumBytes::integer(),Params::map()) -> ok).
-type callback4() ::
	fun((Samples::binary(),NumBytes::integer(),
	     Params::map(), Args::[term()]) -> ok).

-record(state, 
	{
	 handle :: alsa:handle(),
	 params :: #{},                   %% alsa open params
	 pause = true :: boolean(),       %% user call pause
	 recording = false :: boolean(),  %% alsa is recording
	 dbus :: pid(),                   %% pulse dbus connections
	 udev_mon :: reference(),         %% udev monitor
	 udev_ref :: reference(),         %% udev monitor ref
	 udev_names :: [{name,RE::string()}], %% names to match
	 devices :: map(),
	 callback = undefined :: undefined | callback3() | callback4(),
	 callback_args = undefined  %% list when callback4!	 
	}).

%%%===================================================================
%%% API
%%%===================================================================

%% play samples via a callback to alsa_play in stream mode
and_play() ->
    alsa_play:start(),
    #{ period_size := Period } = alsa_play:get_params(),
    VID = alsa_play:alloc([]), %% {buffer_size, Period*2 },{queue,true},{clear,true}]),
    alsa_rec:start(),
    alsa_rec:set_callback(fun ?MODULE:play/4, [VID]),
    alsa_play:run(),
    alsa_play:resume(),
    resume().

play(Data, _NumBytes, Params, [VID]) ->
    %% io:format("n: ~p\n", [_NumBytes]),
    %% alsa_play:set_samples(VID, 0, Params, Data).
    ok = alsa_play:insert(VID, 0, bof, Params, Data),
    alsa_play:restart(VID).

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

%% get alsa param map
-spec get_params() -> map().
get_params() ->
    gen_server:call(?SERVER, get_params).

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

start() -> 
    start(#{}).
start(Options) when is_map(Options) ->
    inpevt:start(),
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
    %% open dbus to keep track on headsets/microphones....
    PulseAddress = pulse_address(),
    {ok,Connection} = dbus_connection:open(PulseAddress),
    Signals = ["org.PulseAudio.Core1.NewCard",
               "org.PulseAudio.Core1.CardRemoved" ],
    Fs = [{path, "/org/pulseaudio/core1"}, {destination, "org.PulseAudio1"}],
    lists:foreach(
      fun(Signal) ->
	      %% Filter objects (paths) may be given as list
	      ?verbose("listen_for_signal: ~p\n", [Signal]),
	      {ok, _Ref} =
                  dbus_pulse:listen_for_signal(Connection, Fs, Signal, [])
      end, Signals),
    %% Setup udev to look for input/power-switch devices
    Udev = udev:new(),
    Umon = udev:monitor_new_from_netlink(Udev, udev),
    SubSys = "input",
    DevType = null,
    ok = udev:monitor_filter_add_match_subsystem_devtype(Umon,SubSys,DevType),
    ok = udev:monitor_filter_add_match_tag(Umon,"power-switch"),
    ok = udev:monitor_enable_receiving(Umon),
    Uref = erlang:make_ref(),
    select = udev:select(Umon, Uref),
    Enum = udev:enumerate_new(Udev),
    udev:enumerate_add_match_subsystem(Enum, "input"),
    udev:enumerate_add_match_tag(Enum, "power-switch"),
    udev:enumerate_add_match_property(Enum, "ID_BUS", "bluetooth"),
    udev:enumerate_add_match_property(Enum, "ID_BUS", "usb"),
    UDevNames = [{name, "Jabra"},
		 {name, "OpenMove"},
		 {name, "UInput Keyboard"}],
    case alsa_capture:open(Options) of
	{ok,H,Params} ->
	    ?verbose("Alsa open: params = ~p\n", [Params]),
	    Fun = maps:get(callback, Options, undefined),
	    Args = maps:get(callback_args, Options, undefiined),
	    State0 = #state{ handle = H,
			     dbus = Connection,
			     udev_mon = Umon,
			     udev_ref = Uref,
			     udev_names = UDevNames,
			     devices = #{},
			     params = Params,
			     callback = Fun,
			     callback_args = Args
			   },
	    State1 = add_existing_devices(Connection, Udev, Enum, State0),
	    {ok, State1};
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
	    State1 = continue_recording(State#state{pause=false}),
	    ?info("resume: recording=~p\n", [State1#state.recording]),
	    {reply, ok, State1};
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
    Reply = ok,
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
      Handle =:= State#state.handle ->
    State1 = rec(State),
    {noreply, State1};

%% pulseaudio dbus signals
handle_info({signal, _Ref, Header, Message} = Msg, State) ->
    ?verbose("dbus signal: ~p\n", [Msg]),
    Fds = Header#dbus_header.fields,
    case {Fds#dbus_field.interface,Fds#dbus_field.member} of
	{"org.PulseAudio.Core1", "NewCard"} ->
	    [Card|_] = Message,
	    ?verbose("card: ~p\n", [Card]),
	    new_dbus_card(State#state.dbus, Card),
	    {noreply, State};
	{"org.PulseAudio.Core1", "CardRemoved"} ->
	    [Card|_] = Message,
	    ?verbose("card: ~p\n", [Card]),
	    {noreply, State};
	_ ->
	    {noreply, State}
    end;
%% udev input_ready signal
handle_info({select, Umon, Uref, ready_input} = Msg, State) when
      State#state.udev_ref =:= Uref ->
    ?verbose("udev read_input: ~p\n", [Msg]),
    Recv = udev:monitor_receive_device(Umon),
    %% reselect should be ok to reuse the Uref...
    select = udev:select(Umon, Uref),
    case Recv of
	undefined ->
	    {noreply, State};
	Dev ->
	    case udev:device_get_action(Dev) of
		"add" ->
		    {noreply, add_udev_card(Dev, State)};
		"remove" ->
		    {noreply,remove_udev_card(Dev, State)}
	    end
    end;
%% input events
handle_info(#input_event{code_sym = playcd, value = 0} = Msg, State) ->
    ?verbose("playcd: ~p, recording: ~p\n", [Msg, State#state.recording]),
    State1 = case State#state.recording of
		 false -> continue_recording(State);
		 true -> State#state{recording=false}
	     end,
    {noreply, State1};
handle_info(#input_event{} = Msg, State) ->
    ?verbose("other input_event: ~p\n", [Msg]),
    {noreply, State};
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

add_existing_devices(Connection, Udev, Enum, State) ->
    {ok,Cards} = dbus_pulse:get_cards(Connection),
    lists:foreach(
      fun(Card) ->
              new_dbus_card(Connection, Card)
      end, Cards),
    lists:foldl(
      fun(Path, Si) ->
	      Dev = udev:device_new_from_syspath(Udev, Path),
	      Si2 = add_udev_card(Dev, Si),
              ?verbose("add_existing_devices: dev:~p dev':~p\n",
		       [Si#state.devices, Si2#state.devices]),
              Si2
      end, State, udev:enumerate_get_devices(Enum)).



add_udev_card(Dev, State = #state{ udev_names = MatchNames }) ->
    Prop = udev:device_get_properties(Dev),
    DevNode = udev:device_get_devnode(Dev),
    NAME = proplists:get_value("NAME",Prop,undefined),
    if is_list(DevNode), NAME =:= undefined ->
	    %% check&match NAME in parent
	    Parent = udev:device_get_parent(Dev),
	    PProp = udev:device_get_properties(Parent),
	    PNAME = stripq(proplists:get_value("NAME",PProp,undefined)),
	    Match = lists:any(
		      fun(N) ->
			      case re:run(PNAME, N) of
				  {match,_} -> true;
				  _ -> false
			      end
		      end, proplists:get_all_values(name, MatchNames)),
	    ?verbose(
               "add_udev_card: ~p ~p ~p\n",
	       [Match, {add, stripq(PNAME)}, {devnode, DevNode}]),
	    case Match of
		true ->
		    case inpevt:add_device(#{device => DevNode}) of
			[] ->
			    ?verbose("add_udev_card:not_added\n"),
			    State;
			[Added] ->
			    ?verbose("add_udev_card: ~p\n",[{added, Added}]),
			    SubscribeResult = inpevt:subscribe(Added),
			    ?verbose("add_udev_card: ~p\n", 
				     [{subscribe, SubscribeResult}]),
			    Devices = State#state.devices,
			    Devices1 = maps:put(DevNode, Added, Devices),
			    State#state{ devices = Devices1 }
		    end;
		false ->
		    State
	    end;
       true ->
	    State
    end.

remove_udev_card(Dev, State) ->
    Prop = udev:device_get_properties(Dev),
    DevNode = udev:device_get_devnode(Dev),
    NAME = proplists:get_value("NAME",Prop,undefined),
    if is_list(DevNode), NAME =:= undefined ->
	    case udev:device_get_parent(Dev) of
		false -> ok;
		Parent ->
		    PProp = udev:device_get_properties(Parent),
		    PNAME = stripq(proplists:get_value("NAME",PProp,undefined)),
		    io:format("~s: PNAME=~p\n", ["remove",stripq(PNAME)]),
		    io:format("    devnode=~p\n", [DevNode])
	    end,
	    Devices = State#state.devices,
	    case maps:take(DevNode, Devices) of
		error -> State;
		{D, Devices1} ->
		    inpevt:delete_device(D),
		    State#state{ devices = Devices1 }
	    end;
       true ->
	    State
    end.

new_dbus_card(Connection, Card) ->
    activate_headset_profile(Connection, Card).

activate_headset_profile(Connection, Card) ->
    case get_card_profile_by_name(Connection, Card,
				  "handsfree_head_unit") of
	{ok,[Profile]} ->
	    set_active_profile(Connection, Card, Profile);
	_Err1 ->
	    ?verbose("handsfree_head_unit profile returned ~p\n", [_Err1]),
	    case get_card_profile_by_name(Connection, Card,
					  "handset_head_unit") of
		{ok,[Profile]} ->
		    set_active_profile(Connection, Card, Profile);
		_Err2 ->
		    ?verbose("handset_head_unit profile returned ~p\n",
			     [_Err2]),
		    ?verbose("no headset profile found:\n"),
		    ignore
	    end
    end.

set_active_profile(Connection, Card, Profile) ->
    ?verbose("set_active_profile: ~p\n", [Profile]),
    dbus_pulse:set_card_active_profile(Connection, Card, Profile),
    {ok,[OffProfile]} = get_card_profile_by_name(Connection, Card, "off"),
    ?verbose("OffProfile = ~p\n", [OffProfile]),
    dbus_pulse:set_card_active_profile(Connection, Card, OffProfile),
    dbus_pulse:set_card_active_profile(Connection, Card, Profile).

get_card_profile_by_name(Connection, Card, Name) ->
    dbus_pulse:get_card_profile_by_name(Connection, Card, Name).


stripq(Atom) when is_atom(Atom) ->
    Atom;
stripq(String) when is_list(String) ->
    case String of
	[$"|String0] ->
	    case lists:reverse(String0) of
		[$"|String1] -> lists:reverse(String1);
		_ -> String0
	    end;
	_ -> String
    end.


pulse_address() ->
    UserId = string:strip(os:cmd("id --user"), right, $\n),
    UserDbusSocketPath =
        filename:join(["run", "user", UserId, "pulse", "dbus-socket"]),
    case filelib:is_file(UserDbusSocketPath) of
	true ->
	    dbus:pulse_address();
	false ->
	    try
		dbus:pulse_address()
	    catch error:_ ->
		    %% We assume that pulseaudio run as a system daemon
		    {"unix:path=/var/run/pulse/dbus-socket",external,false}
	    end
    end.

continue_recording(State) ->
    if State#state.pause -> State;
       State#state.recording -> State;
       true -> rec(State#state{recording=true})
    end.

rec(State = #state {pause = true }) -> State;
rec(State = #state {recording = false }) -> State;
rec(State = #state {handle=Handle,params=#{period_size:=Period}}) ->
    case alsa:read_(Handle, Period) of
	{error, eagain} ->
	    %% ?verbose("rec: again\n"),
	    ok = alsa:select_(Handle),
	    State;
	{error, epipe} ->
	    ?warning("rec: epipe\n"),
	    alsa:recover_(Handle, epipe),
	    rec(State);
	{error, estrpipe} ->
	    ?warning("rec: estrpipe\n"),
	    alsa:recover_(Handle, estrpipe),
	    rec(State);
	{error, AlsaError} when is_integer(AlsaError) ->
	    _Error =  alsa:strerror(AlsaError),
	    ?error("rec: error ~s\n", [_Error]),
	    %% crash?
	    State;
	{ok, {_ReadFrames, Samples}} ->
	    %% ?verbose("rec: read ~w\n", [_ReadFrames]),
	    user_callback(Samples, byte_size(Samples), State),
	    %% keep count?
	    rec(State)
    end.

user_callback(_Data, _NumBytes, #state{ callback=undefined}) ->
    ok;
user_callback(Data, NumBytes, #state{ callback=Fun, callback_args=undefined,
				      params=Params}) ->
    Fun(Data, NumBytes, Params);
user_callback(Data, NumBytes, #state{ callback=Fun, callback_args=Args,
				      params=Params}) ->
    Fun(Data, NumBytes, Params, Args).
