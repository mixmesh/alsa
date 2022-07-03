%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Generate square/sine/triangle waves
%%% @end
%%% Created : 28 Nov 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_wave).
-compile(export_all).

-include("../include/alsa.hrl").

%% -define(verbose(F,A), io:format((F),(A))).
-define(verbose(F,A), ok).

-type float01() :: float().
-type time_s() :: float().
-type envelope_id() :: integer().
-type radians() :: number().

%% amplitude envelope
-record(envelope,
	{
	 id      :: envelope_id(),   %% envelope number
	 attack  = 0.0 :: time_s(),
	 decay   = 0.0 :: time_s(),
	 sustain = 0.0 :: time_s(),
	 release = 0.0 :: time_s(),
	 low_level = 0.0 :: float01(),
	 peek_level :: float01(),
	 sustain_level :: float01()
	}).

-record(waveform,
	{
	 form :: sine | square | triangle | saw,
	 f1 :: radians(),   %% zero -> peek   : f1 -> f2
	 f2 :: radians(),   %% peek -> sust   : f2 -> f3
	 f3 :: radians(),   %% sust1 -> sust2 : f3 -> f4
	 f4 :: radians(),   %% sust2 -> zero  : f4 -> f5
	 f5 :: radians(),
	 phase = 0.0 :: radians(),
	 noice = 0 :: non_neg_integer()   %% 1/N*(uniform (-1,1))
	}).

-record(wave,
	{
	 id :: envelope_id(),
	 w1 :: #waveform{},
	 w2 :: undefined | #waveform{},
	 w3 :: undefined | #waveform{}
	}).

play() ->
    play(#{}).

play(Opts) when is_list(Opts) ->
    init_(maps:from_list(Opts));
play(Opts) when is_map(Opts) ->
    init_(Opts).

-define(EZ,
	#{ id => 0,
	   sustain => 1.5,
	   peek_level => 0.0, sustain_level => 0.0 }).
-define(E1,
	#{ id => 1,
	   sustain => 0.1, release => 0.5, decay => 0.1,
	   peek_level => 0.9, sustain_level => 0.2 }).
-define(E2,
	#{ id => 2,
	   sustain => 0.1, release => 0.2, decay => 0.1,
	   peek_level => 0.3, sustain_level => 0.2 }).

-define(E3, #{ id => 3,
	       sustain => 0.1, release => 0.5, decay => 0.1,
	       peek_level => 0.2, sustain_level => 0.3 }).

-define(E4, #{ id => 4,
	       sustain => 0.1, release => 0.2, decay => 0.1,
	       peek_level => 0.3, sustain_level => 0.2 }).

-define(E5, #{ id => 5,
	       sustain => 0.1, release => 0.2, decay => 0.1,
	       peek_level => 0.2, sustain_level => 0.1 }).

-define(PAUSE, [0, {square, [1]}]).
%% enter
-define(NOTE1, [1, {sine, ["C#4"]}, {sine,["G#4"]}]).
-define(NOTE2, [2, {sine, ["G#4"]}, {sine, ["D#5"]}]).

-define(PONG, [5, {saw, ["D#4"]},{sine,["A#3"]},{sine, ["D#6"]}]).

-define(NOTE12, [3, {sine, ["C#4"]}, {sine,["G#4"]}]).
-define(NOTE22, [4, {sine, ["G#4"]}, {sine, ["D#5"]}]).

enter() ->
    play(#{ rate => 16000,
	    envelopes => [?EZ, ?E1, ?E2, ?E2, ?E3, ?E4, ?E5],
	    waves => [?NOTE1, ?NOTE2]
	  }).

mute() ->
    play(#{ rate => 16000,
	    envelopes => [?EZ, ?E1, ?E2, ?E2, ?E3, ?E4, ?E5],
	    waves => [
		      ?PONG, ?PAUSE,
		      ?PONG, ?PAUSE,
		      ?PONG, ?PAUSE,
		      ?PONG, ?PAUSE
		     ]
	  }).

leave() ->
    play(#{ rate => 16000,
	    envelopes => [?EZ, ?E1, ?E2, ?E2, ?E3, ?E4, ?E5],
	    waves => [ ?NOTE22, ?NOTE12 ] }).


demo() ->
    play(#{ rate => 16000,
	    envelope => #{ sustain => 0.1, release => 0.2,
			   peek_level => 0.9, sustain_level => 0.7},
	    waves => [ [{sine, ["C4"]}],
		       [{sine, ["E4"]}],
		       [{sine, ["G4"]}] ] }).

demo2() ->
    play(#{ rate => 16000,
	    envelope => #{ sustain => 0.1, release => 0.2,
			   peek_level => 0.9, sustain_level => 0.7},
	    waves => [ [{sine, ["C4"]}, {sine, ["E5"]}],
		       [{sine, ["E4"]}],
		       [{sine, ["G4"]}] ] }).



init_(Options0) ->
    Options = maps:merge(Options0, #{ latency =>  100}),
    case alsa_playback:open(Options) of
	{ok, H, Params} ->
	    %%io:format("Params: ~p\n", [Params]),
	    Time = maps:get(time, Options, 2.0),
	    Es0 = maps:get(envelopes, Options, []),
	    E0 = maps:get(envelope, Options, undefined),
	    Es1 = if Es0 =:= [] ->
			  if E0 =:= undefined ->
				  [#{ id => 1,
				      sustain => Time,
				      peek_level => 1.0,
				      sustain_level => 0.8}];
			     true ->
				  [E0]
			  end;
		     true ->
			  Es0
		  end,
	    Rate = maps:get(rate, Params),
	    Format = maps:get(format, Params),
	    Channels = maps:get(channels, Params),
	    Ws = maps:get(waves, Options, [{sine,[440]}]),
	    Waves = [wave_def(W) || W <- Ws],
	    EnvMap = make_envelope_map(Es1, #{}),
	    P = maps:get(period_size, Params),
	    %%io:format("P = ~p\n", [P]),
	    %%io:format("Waves = ~p\n", [Waves]),
	    run_(H,P,Rate,0.0,1/Rate,Waves,EnvMap,Format,Channels);

	{error, Reason} ->
	    {error, alsa:strerror(Reason)}
    end.


run_(H,_P,_Rate,_T,_Dt,[],_EnvMap,_Format,_Channels) ->
    stop(H),
    ok;
run_(H,P,Rate,T,Dt,[W=#wave{id=ID}|Ws],EnvMap,Format,Channels) ->
    E = maps:get(ID, EnvMap),
    Tot = envelope_time(E),
    N  = trunc(Tot*Rate),
    run_(H,N,N,P,Rate,T,Dt,W,Ws,EnvMap,Format,Channels).

run_(H,N,N0,P,Rate,T,Dt,W,Ws,EnvMap,Format,Channels) ->
    if N =< 0 ->
	    run_(H,P,Rate,0.0,Dt,Ws,EnvMap,Format,Channels);
       true ->
	    Samples01 = generate(P,T,Dt,W,EnvMap),
	    Bin = native_samples(Samples01,Format,Channels),
	    case alsa:write(H, Bin) of
		{ok, M} when is_integer(M), M >= 0 ->
		    {ok,Mf} = alsa:bytes_to_frames(H, M),
		    T1 = T + Mf*Dt,
		    run_(H,N-Mf,N0,P,Rate,T1,Dt,W,Ws,EnvMap,Format,Channels);
		{ok, underrun} ->
		    %%io:format("Recovered from underrun\n"),
		    run_(H,N,N0,P,Rate,T,Dt,W,Ws,EnvMap,Format,Channels);
		{ok, suspend_event} ->
		    %%io:format("Recovered from suspend event\n"),
		    run_(H,N,N0,P,Rate,T,Dt,W,Ws,EnvMap,Format,Channels);
		{error, Reason} ->
		    %%io:format("write failed ~p\n", [Reason]),
		    stop(H),
		    {error, alsa:strerror(Reason)}
	    end
    end.

wave_def([ID,F1,F2,F3]) when is_integer(ID) ->
    #wave { id = ID, w1 = wdef(F1), w2 = wdef(F2), w3 = wdef(F3) };
wave_def([ID,F1,F2]) when is_integer(ID) ->
    #wave { id = ID, w1 = wdef(F1), w2 = wdef(F2) };
wave_def([ID,F1]) when is_integer(ID) ->
    #wave { id = ID, w1 = wdef(F1) };
wave_def([F1,F2,F3]) ->
    #wave { id = 1, w1 = wdef(F1), w2 = wdef(F2), w3 = wdef(F3) };
wave_def([F1,F2]) ->
    #wave { id = 1, w1 = wdef(F1), w2 = wdef(F2) };
wave_def([F1]) ->
    #wave { id = 1, w1 = wdef(F1) };
wave_def(F1) ->
    #wave { id = 1, w1 = wdef(F1) }.

wdef({Form,Fs}) -> wdef_(Form, Fs, 0.0);
wdef({Form,Fs,Phase}) -> wdef_(Form, Fs, Phase).

wdef_(Form, Fs, Phase) when Form =:= sine;
				Form =:= square;
				Form =:= triangle;
				Form =:= saw ->
    case Fs of
	[F1] ->
	    Fa = wave_freq(F1),
	    make_waveform(Form, Phase, Fa, Fa, Fa, Fa, Fa);
	[F1,F2] ->
	    Fa = wave_freq(F1),
	    Fb = wave_freq(F2),
	    make_waveform(Form, Phase, Fa, Fa, Fb, Fb, Fb);
	[F1,F2,F3] ->
	    Fa = wave_freq(F1),
	    Fb = wave_freq(F2),
	    Fc = wave_freq(F3),
	    make_waveform(Form, Phase, Fa, Fa, Fb, Fb, Fc);
	[F1,F2,F3,F4] ->
	    Fa = wave_freq(F1),
	    Fb = wave_freq(F2),
	    Fc = wave_freq(F3),
	    Fd = wave_freq(F4),
	    make_waveform(Form, Phase, Fa, Fb, Fc, Fd, Fd);
	[F1,F2,F3,F4,F5] ->
	    Fa = wave_freq(F1),
	    Fb = wave_freq(F2),
	    Fc = wave_freq(F3),
	    Fd = wave_freq(F4),
	    Fe = wave_freq(F5),
	    make_waveform(Form, Phase, Fa, Fb, Fc, Fd, Fe)
    end.

make_waveform(Form, Phase, F1, F2, F3, F4, F5) ->
    #waveform { form=Form, f1=F1, f2=F2, f3=F3, f4=F4, f5=F5, phase=Phase,
		noice = 0 }.


%% wave_def({const,X}) -> {const,X};
%% wave_def({mix,A,B}) -> {mix,wave_def(A),wave_def(B)};
%% wave_def({mix,A,B,C}) -> {mix,wave_def(A),wave_def(B),wave_def(C)};
%% wave_def({mult,A,B}) -> {mult,wave_def(A),wave_def(B)};
%% wave_def(X) when is_float(X) -> {const,clamp(X,0.0,1.0)}.

make_envelope_map(Es, Map) ->
    make_envelope_map(Es, 1, Map).

make_envelope_map([E|Es], I, Map) ->
    ID = maps:get(id, E, I),
    A = maps:get(attack, E, 0.0),
    D = maps:get(decay, E, 0.0),
    S = maps:get(sustain, E, 0.0),
    R = maps:get(release, E, 0.0),
    Lz = maps:get(low_level, E, 0.0),
    Lp = maps:get(peek_level, E, 1.0),
    Ls = maps:get(sustain_level, E, 0.8),
    Map1 = maps:put(ID,
		    #envelope{id=ID,attack=A,decay=D,sustain=S,release=R,
			      low_level=Lz, peek_level=Lp, sustain_level=Ls},
		    Map),
    make_envelope_map(Es, max(ID+1, I+1), Map1);
make_envelope_map([], _I, Map) ->
    Map.

wave_freq({note,N}) when is_integer(N), N>=0, N=<127 ->
    alsa_util:midi_note_to_frequency(N);
wave_freq(F) when is_number(F) -> float(F);
wave_freq(Name) when is_list(Name) ->
    alsa_util:midi_note_to_frequency(
      alsa_util:midi_name_to_note(Name));
wave_freq(Name) when is_atom(Name) ->
    alsa_util:midi_note_to_frequency(
      alsa_util:midi_name_to_note(atom_to_list(Name))).

%% calculate total time (ms) for the envelope
-spec envelope_time(#envelope{}) -> time_s().

envelope_time(#envelope{attack=A,decay=D,sustain=S,release=R}) ->
    A+D+S+R.

%% Calculate current level given envelop and time (0 <= T <= A+D+S+R)
-spec envelope_level(T::time_s(), E::#envelope{}) -> float01().

envelope_level(T, #envelope{attack=A,decay=D,sustain=S,release=R,
			    low_level=L0, peek_level=L1, sustain_level=L2 }) ->
    if A>0, T < A -> interp(T/A, L0, L1);
       D>0, T < A+D -> interp((T-A)/D, L1, L2);
       S>0, T < A+D+S -> L2; %% interp((T-(A+D))/S, L2, L2);
       R>0, T < A+D+S+R -> interp((T-(A+D+S))/R, L2, L0);
       true -> L0
    end.

envelope_freq(T, #envelope{attack=A,decay=D,sustain=S,release=R },
	      #waveform{f1=F1,f2=F2,f3=F3,f4=F4,f5=F5}) ->
    if A>0, T < A -> interp(T/A, F1, F2);
       D>0, T < A+D -> interp((T-A)/D, F2, F3);
       S>0, T < A+D+S -> interp((T-(A+D))/S, F3, F4);
       R>0, T < A+D+S+R -> interp((T-(A+D+S))/R, F4, F5);
       true -> F5
    end.

%% interpolate A -> B when T : 0 -> 1
interp(T, A, B) ->
    A*(1-T) + B*T.


stop(H) ->
    alsa:drain(H),
    alsa:close(H).

generate(N, T, Dt, #wave{id=ID,w1=W1,w2=W2,w3=W3},EnvMap) ->
    E = maps:get(ID, EnvMap),
    if W2 =:= undefined, W3 =:= undefined ->
	    generate_form(N,W1,T,Dt,E,[]);
       W3 =:= undefined ->
	    S1 = generate_form(N,W1,T,Dt,E,[]),
	    S2 = generate_form(N,W2,T,Dt,E,[]),
	    mix(S1,S2);
       true ->
	    S1 = generate_form(N,W1,T,Dt,E,[]),
	    S2 = generate_form(N,W2,T,Dt,E,[]),
	    S3 = generate_form(N,W3,T,Dt,E,[]),
	    mix(S1,S2,S3)
    end.

generate_form(0,_W,_T,_Dt,_E,Acc) ->
    lists:reverse(Acc);
generate_form(N,W,T,Dt,E,Acc) ->
    F = envelope_freq(T, E, W),
    ?verbose("T=~.2f, F=~.2f\n", [T, F]),
    Y = case W#waveform.form of
	    sine ->
		math:sin(2*math:pi()*F*T+W#waveform.phase);
	    square ->
		Ti = F*math:fmod(T, 1/F),
		if Ti < 0.5 -> -0.5;  true -> 0.5 end;
	    triangle ->
		Ti = F*math:fmod(T, 1/F),
		if Ti =< 0.5 ->  %% ( 0 => 0.5 )
			(4*Ti - 1.0);
		   true ->  %% falling 0.5 => 1
			(1.0 - 4*Ti)
		end;
	    saw ->
		Ti = F*math:fmod(T, 1/F),
		(2*Ti - 1.0)
	end,
    Z = if W#waveform.noice > 0 ->
		2*(0.5 - rand:uniform()) / W#waveform.noice;
	   true ->
		0
	end,
    L = envelope_level(T, E),
    generate_form(N-1,W,T+Dt,Dt,E,[L*Y+Z|Acc]).

mix([A|As], [B|Bs]) ->
    [alsa_util:mix_float(A,B) | mix(As,Bs)];
mix([], []) -> [].

mix([A|As], [B|Bs], [C|Cs]) ->
    [alsa_util:mix_float(A,B,C) | mix(As,Bs,Cs)];
mix([], [], []) -> [].

%% multiply two sample sequences
-spec mult([float01()], [float01()]) -> [float01()].
mult([X|Xs], [Y|Ys]) ->
    [X*Y | mult(Xs,Ys)];
mult([], []) ->
    [].

clamp(X, Min, Max) -> min(max(X,Min), Max).


native_samples(Ys, s16_le, 1) ->
    << <<(trunc(Y*32767)):16/little>> || Y <- Ys >>;
native_samples(Ys, s16_le, 2) ->
    << <<(trunc(Y*16383)):16/little,(trunc(Y*16383)):16/little>> ||
	Y <- Ys >>.
