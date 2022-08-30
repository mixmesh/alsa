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

-export([test/0]).
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
-export([insert/2, insert_file/2]).
-export([append/2, append_file/2]).
-export([reset/1, clear/1]).
-export([reset/0, clear/0]).
-export([resume/0, pause/0]).


test() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    start(#{}),
    new(1),
    new(2),
    append_file(1, filename:join(Sounds, "Front_Center.wav")),
    append_file(2, filename:join(Sounds, "Front_Left.wav")),
    append_file(2, filename:join(Sounds, "Front_Right.wav")),
    loop(1, true),
    loop(2, true),
    resume().
    

new(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {new, Channel}).

insert(Channel, Samples) when ?is_channel(Channel), is_binary(Samples) ->
    gen_server:call(?SERVER, {insert, Channel, Samples}).

insert_file(Channel, Filename) when ?is_channel(Channel), is_list(Filename) ->
    gen_server:call(?SERVER, {insert_file, Channel, Filename}).

append(Channel, Samples) when ?is_channel(Channel), is_binary(Samples) ->
    gen_server:call(?SERVER, {append, Channel, Samples}).

append_file(Channel, Filename) when ?is_channel(Channel), is_list(Filename) ->
    gen_server:call(?SERVER, {append_file, Channel, Filename}).

reset(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {reset, Channel}).

clear(Channel) when ?is_channel(Channel) ->
    gen_server:call(?SERVER, {clear, Channel}).

mute(Channel, On) when ?is_channel(Channel), is_boolean(On) ->
    gen_server:call(?SERVER, {mute, Channel, On}).

loop(Channel, On) when ?is_channel(Channel), is_boolean(On) ->
    gen_server:call(?SERVER, {loop, Channel, On}).

reset() ->
    gen_server:call(?SERVER, reset).

clear() ->
    gen_server:call(?SERVER, clear).

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
	 output = undefined :: undefine | binary()
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
	    io:format("Alsa open: params = ~p\n", [Params]),
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
	    Buf = alsa_buffer:new(State#state.params),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf }}};
	_Buf ->
	    {reply, {error, ealready}, State}
    end;

handle_call({insert,Channel,Samples}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:insert(Buf, Samples),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;
handle_call({append,Channel,Samples}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:append(Buf, Samples),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;

handle_call({insert_file,Channel,Filename}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:insert_file(Buf, Filename),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;
handle_call({append_file,Channel,Filename}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:append_file(Buf, Filename),
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
	    Buf1 = alsa_buffer:clear(Buf),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;    

handle_call({mute,Channel,On}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:setopts(Buf,[{mute,On}]),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;    

handle_call({loop,Channel,On}, _From, State) ->
    ChanMap = State#state.channels,
    case maps:get(Channel, ChanMap, undefined) of
	undefined ->
	    {reply, {error, enoent}, State};
	Buf ->
	    Buf1 = alsa_buffer:setopts(Buf,[{loop,On}]),
	    {reply, ok, State#state { channels = ChanMap#{ Channel => Buf1 }}}
    end;    

handle_call(reset, _From, State) ->
    ChanMap1 = maps:map(
		 fun(_Channel, Buf) ->
			 alsa_buffer:reset(Buf)
		 end, State#state.channels),
    {reply, ok, State#state { channels = ChanMap1 }};

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
	    ok = alsa:select_(Handle),
	    {norereply, State};
	{error, epipe} -> 
	    alsa:recover_(Handle, epipe),
	    erlang:display({alsa_play, underrun}),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{error, estrpipe} ->
	    alsa:recover_(Handle, estrpipe),
	    erlang:display({alsa_play, suspend_event}),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{error, AlsaError} when is_integer(AlsaError) ->
	    %% restart?
	    erlang:display({alsa_play,{error, alsa:strerror(AlsaError)}}),
	    State1 = play(State#state{output = undefined}),
	    {noreply, State1};
	{ok, Written} ->
	    Size = byte_size(State#state.output),
	    if Written =:= Size ->
		    State1 = play(State#state{output = undefined}),
		    {noreply, State1};
	       true ->
		    <<_:Written/binary, Bin1/binary>> = State#state.output,
		    State1 = play(State#state{output = Bin1 }),
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

play(State = #state {handle=Handle,output=undefined,
		     params=Params,channels=ChanMap}) ->
    PeriodSize = maps:get(period_size, Params),
    Channels = maps:get(channels, Params),
    Format = maps:get(format, Params),
    %% assume all buffer have same format and same number of channels
    {ChanMap1,BufferList} = read_buffer_list(ChanMap, PeriodSize),
    FrameSize = alsa:format_size(Format, Channels),
    Bin = mix_buffers(BufferList, FrameSize, Format, Channels),
    case alsa:write_(Handle, Bin) of
	{error, eagain} ->
	    io:format("play: again\n"),
	    ok = alsa:select_(Handle),
	    State#state{ output = Bin, channels=ChanMap1 };
	{error, epipe} -> 
	    io:format("play: epipe\n"),
	    alsa:recover_(Handle, epipe),
	    erlang:display({alsa_play, underrun}),
	    play(State#state{output = Bin, channels=ChanMap1});
	{error, estrpipe} ->
	    io:format("play: estrpipe\n"),
	    alsa:recover_(Handle, estrpipe),
	    erlang:display({alsa_play, suspend_event}),
	    play(State#state{output = Bin, channels=ChanMap1});
	{error, AlsaError} when is_integer(AlsaError) ->
	    Error =  alsa:strerror(AlsaError),
	    io:format("play: error ~s\n", [Error]),
	    %% restart?
	    erlang:display({alsa_play,{error,Error}}),
	    play(State#state{output = Bin, channels=ChanMap1});
	{ok, Written} ->
	    io:format("play: written ~w, remain=~w\n", 
		      [Written, byte_size(Bin)]),
	    if Written =:= byte_size(Bin) ->
		    play(State#state{output = undefined, channels=ChanMap1});
	       true ->
		    <<_:Written/binary, Bin1/binary>> = Bin,
		    play(State#state{output = Bin1, channels=ChanMap1 })
	    end
    end;
%% send more!
play(State = #state {handle=Handle,output=Bin}) ->
    case alsa:write_(Handle, Bin) of
	{error, eagain} ->
	    io:format("play1: again\n"),
	    ok = alsa:select_(Handle),
	    State;
	{error, epipe} ->
	    io:format("play1: epipe\n"),
	    alsa:recover_(Handle, epipe),
	    erlang:display({alsa_play, underrun}),
	    play(State);
	{error, estrpipe} ->
	    io:format("play1: estrpipe\n"),
	    alsa:recover_(Handle, estrpipe),
	    erlang:display({alsa_play, suspend_event}),
	    play(State);
	{error, AlsaError} when is_integer(AlsaError) ->
	    Error =  alsa:strerror(AlsaError),
	    io:format("play1: error ~s\n", [Error]),	    
	    erlang:display({alsa_play,{error, Error}}),
	    play(State);
	{ok, Written} ->
	    io:format("play1: written ~w, remain=~w\n", 
		      [Written, byte_size(Bin)]),
	    if Written =:= byte_size(Bin) ->
		    play(State#state{output = undefined});
	       true ->
		    <<_:Written/binary, Bin1/binary>> = Bin,
		    play(State#state{output = Bin1 })
	    end
    end.

read_buffer_list(ChanMap, PeriodSize) ->
    read_buffer_list_(maps:keys(ChanMap), ChanMap, PeriodSize, []).

read_buffer_list_([Channel|Cs], ChanMap, PeriodSize, Acc) ->
    Cb = maps:get(Channel, ChanMap),
    {Samples,Cb1} = alsa_buffer:read_samples(Cb, PeriodSize),
    read_buffer_list_(Cs, ChanMap#{ Channel => Cb1}, PeriodSize, [Samples|Acc]);
read_buffer_list_([], ChanMap, _PeriodSize, Acc) ->
    {ChanMap, Acc}.
    

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

