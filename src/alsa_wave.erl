%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Generate square/sine/triangle waves
%%% @end
%%% Created : 28 Nov 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_wave).
-compile(export_all).

-include("../include/alsa.hrl").

-type float01() :: float().

play() ->	 
    play(#{}).

play(Opts) when is_list(Opts) ->
    init_(maps:from_list(Opts));
play(Opts) when is_map(Opts) ->
    init_(Opts).

init_(Options0) ->
    Options = maps:merge(Options0, #{ latency =>  100}),
    case alsa_playback:open(Options) of
	{ok, H, Params} ->
	    io:format("Params: ~p\n", [Params]),
	    Time = maps:get(time, Options, 5.0),
	    Rate = maps:get(rate, Params),
	    Format = maps:get(format, Params),
	    Channels = maps:get(channels, Params),
	    Wave = maps:get(wave, Options, {sine,440}),
	    WaveDef = wave_def(Wave),
	    Np = maps:get(period_size, Params),
	    N  = trunc(Time*Rate),
	    io:format("N = #samples ~p\n", [N]),
	    io:format("Np = period ~p\n", [Np]),
	    io:format("WaveDef = ~p\n", [WaveDef]),
	    run_(H,N,Np,0.0,1/Rate,WaveDef,Format,Channels);
	{error, Reason} ->
	    {error, alsa:strerror(Reason)}
    end.

run_(H,N,Np,T,Dt,WaveDef,Format,Channels) ->
    if N =< 0 ->
	    stop(H),
	    ok;
       true ->
	    Samples01 = generate(Np,T,Dt,WaveDef),
	    Bin = native_samples(Samples01,Format,Channels),
	    case alsa:write(H, Bin) of
		{ok, M} when is_integer(M), M >= 0 ->
		    {ok,Mf} = alsa:bytes_to_frames(H, M),
		    T1 = T + Mf*Dt,
		    run_(H,N-Mf,Np,T1,Dt,WaveDef,Format,Channels);
		{ok, underrun} ->
		    io:format("Recovered from underrun\n"),
		    run_(H,N,Np,T,Dt,WaveDef,Format,Channels);
		{ok, suspend_event} ->
		    io:format("Recovered from suspend event\n"),
		    run_(H,N,Np,T,Dt,WaveDef,Format,Channels);
		{error, Reason} ->
		    io:format("write failed ~p\n", [Reason]),
		    stop(H),
		    {error, alsa:strerror(Reason)}
	    end
    end.


wave_def({sine,F}) -> {sine,2*math:pi()*wave_freq(F),0};
wave_def({sine,F,Phi}) -> {sine,2*math:pi()*wave_freq(F),Phi};
wave_def({square,F}) -> {square,1/wave_freq(F)};
wave_def({triangle,F}) -> {triangle,1/wave_freq(F)};
wave_def({saw,F}) -> {saw,1/wave_freq(F)};
wave_def({const,X}) -> {const,X};
wave_def({mix,A,B}) -> {mix,wave_def(A),wave_def(B)};
wave_def({mix,A,B,C}) -> {mix,wave_def(A),wave_def(B),wave_def(C)};
wave_def({mult,A,B}) -> {mult,wave_def(A),wave_def(B)};
wave_def(X) when is_float(X) -> {const,clamp(X,0.0,1.0)}.

wave_freq({note,N}) when is_integer(N), N>=0, N=<127 -> 
    alsa_util:midi_note_to_frequency(N);
wave_freq(F) when is_number(F) -> F;
wave_freq(Name) when is_list(Name) -> 
    alsa_util:midi_note_to_frequency(
      alsa_util:midi_name_to_note(Name));
wave_freq(Name) when is_atom(Name) -> 
    alsa_util:midi_note_to_frequency(
      alsa_util:midi_name_to_note(atom_to_list(Name))).

stop(H) ->
    alsa:drain(H),
    alsa:close(H).

generate(N, T, Dt, {mix,A,B}) ->
    As = generate(N, T, Dt, A),
    Bs = generate(N, T, Dt, B),
    mix(As, Bs);
generate(N, T, Dt, {mix,A,B,C}) ->
    As = generate(N, T, Dt, A),
    Bs = generate(N, T, Dt, B),
    Cs = generate(N, T, Dt, C),
    mix(As, Bs, Cs);
generate(N, T, Dt, {mult,A,B}) ->
    As = generate(N, T, Dt, A),
    Bs = generate(N, T, Dt, B),
    mult(As, Bs);
generate(N, T, Dt, {sine,W,Phi}) ->
    generate_sine(N,W,Phi,T,Dt);
generate(N, T, Dt, {square,P}) ->
    generate_square(N,P,T,Dt);
generate(N, T, Dt, {triangle,P}) ->
    generate_triangle(N,P,T,Dt);
generate(N, T, Dt, {saw,P}) ->
    generate_saw(N,P,T,Dt);
generate(N, _T, _Dt, {const,X}) ->
    lists:duplicate(N, X).

%% sine wave
generate_sine(N,W,Phi,T,Dt) ->
    generate_sine_(N,W,Phi,T,Dt,[]).
generate_sine_(0,_W,_Phi,_T,_Dt,Acc) ->
    lists:reverse(Acc);
generate_sine_(N,W,Phi,T,Dt,Acc) ->
    Y = math:sin(W*T+Phi),
    generate_sine_(N-1,W,Phi,T+Dt,Dt,[Y|Acc]).

%% square wave
generate_square(N,P,T,Dt) ->
    generate_square_(N,P,T,Dt,[]).
generate_square_(0,_P,_T,_Dt,Acc) ->
    lists:reverse(Acc);
generate_square_(N,P,T,Dt,Acc) ->
    Ti = math:fmod(T, P)/P,
    Y = if Ti < 0.5 -> -0.5;  true -> 0.5 end,
    generate_square_(N-1,P,T+Dt,Dt,[Y|Acc]).

%% triangle wave
generate_triangle(N,P,T,Dt) ->
    generate_triangle_(N,P,T,Dt,[]).
generate_triangle_(0,_P,_T,_Dt,Acc) ->
    lists:reverse(Acc);
generate_triangle_(N,P,T,Dt,Acc) ->
    Ti = math:fmod(T, P)/P,
    Y = if Ti =< 0.5 ->  %% ( 0 => 0.5 )
		(4*Ti - 1.0); 
	   true ->  %% falling 0.5 => 1
		(1.0 - 4*Ti)
	end,
    generate_triangle_(N-1,P,T+Dt,Dt,[Y|Acc]).

%% triangle saw
generate_saw(N,P,T,Dt) ->
    generate_saw_(N,P,T,Dt,[]).
generate_saw_(0,_P,_T,_Dt,Acc) ->
    lists:reverse(Acc);
generate_saw_(N,P,T,Dt,Acc) when N > 0 ->
    Ti = math:fmod(T, P)/P,
    Y = (2*Ti - 1.0),
    generate_saw_(N-1,P,T+Dt,Dt,[Y|Acc]).


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
