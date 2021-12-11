%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Generate square/sine/triangle waves
%%% @end
%%% Created : 28 Nov 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_wave).
-compile(export_all).

-include("../include/alsa.hrl").

play() ->	 
    play(#{}).

play(Opts) when is_list(Opts) ->
    play_(maps:from_list(Opts));
play(Opts) when is_map(Opts) ->
    play_(Opts).

play_(Opts) when is_map(Opts) ->
    wave_init(Opts).

wave_init(Options) ->
    case alsa_playback:open(Options) of
	{ok, H, Params} ->
	    io:format("Params: ~p\n", [Params]),
	    Wave = maps:get(wave_form, Options, sine),
	    Amp  = maps:get(amplitude, Options, 0.5),
	    Frequency = maps:get(frequency, Options, 440),
	    Time = maps:get(time, Options, 5.0),
	    Rate = maps:get(rate, Params),
	    Format = maps:get(format, Params),
	    Channels = maps:get(channels, Params),
	    SW = Rate/Frequency,    %% samples per wave (period)
	    TW = (Time*Rate),      %% total number of samples in s
	    T = 0.0,
	    Dt = 1/SW,
	    W  = 2*math:pi(),
	    io:format("SW = samples/wave ~p\n", [SW]),
	    io:format("TW = #samples ~p\n", [TW]),
	    io:format("W = ~p\n", [W]),
	    play_(H,TW,W,T,Dt,SW,Wave,Amp,Format,Channels);
	{error, Reason} ->
	    {error, alsa:strerror(Reason)}
    end.

play_(H,TW,W,T,Dt,SW,Wave,Amp,Format,Channels) ->
    if TW =< 0 ->
	    stop(H),
	    ok;
       true ->
	    {T1,Samples01} = generate_period(Wave,SW,W,Amp,T,Dt),
	    Bin = native_samples(Samples01,Format,Channels),
	    case alsa:write(H, Bin) of
		{ok, N} when is_integer(N), N >= 0 ->
		    play_(H,TW-N,W,T1,Dt,SW,Wave,Amp,Format,Channels);
		{ok, underrun} ->
		    io:format("Recovered from underrun\n"),
		    play_(H,TW,W,T1,Dt,SW,Wave,Amp,Format,Channels);
		{ok, suspend_event} ->
		    io:format("Recovered from suspend event\n"),
		    play_(H,TW,W,T1,Dt,SW,Wave,Amp,Format,Channels);
		{error, Reason} ->
		    io:format("write failed ~p\n", [Reason]),
		    stop(H),
		    {error, alsa:strerror(Reason)}
	    end
    end.

stop(H) ->
    alsa:drain(H),
    alsa:close(H).

generate_period(sine,N,W,Amp,T,Dt) ->
    generate_sine_(N,W,Amp,T,Dt,[]);
generate_period(square,N,Amp,_W,T,Dt) ->
    A1 = Amp/2,
    generate_square_(N,(A1/2),-(A1/2),T,Dt,[]);
generate_period(triangle,N,_W,Amp,T,Dt) ->
    A1 = Amp/2,
    generate_triangle_(N,A1,T,Dt,[]);
generate_period(saw,N,_W,Amp,T,Dt) ->
    A1 = Amp/2,
    generate_saw_(N,A1,T,Dt,[]).

generate_sine_(N,W,Amp,T,Dt,Acc) when N > 0 ->
    Y = Amp*math:sin(W*T),
    generate_sine_(N-1,W,Amp,tstep(T,Dt),Dt,[Y|Acc]);
generate_sine_(_N,_W,_Amp,T,_Dt,Acc) ->
    {T,lists:reverse(Acc)}.

generate_square_(N,H,L,T,Dt,Acc) when N > 0 ->
    Y = if T > 0.5 -> L;  true -> H end,
    generate_square_(N-1,H,L,tstep(T,Dt),Dt,[Y|Acc]);
generate_square_(_N,_H,_L,T,_Dt,Acc) ->
    {T,lists:reverse(Acc)}.

generate_triangle_(N,Amp,T,Dt,Acc) when N > 0 ->
    Y = if  T =< 0.5 ->  %% ( 0 => 0.5 )
		Amp*(4*T - 1.0); 
	    true ->  %% falling 0.5 => 1
		Amp*(1.0 - 4*T)
	end,
    generate_triangle_(N-1,Amp,tstep(T,Dt),Dt,[Y|Acc]);
generate_triangle_(_N,_Amp,T,_Dt,Acc) ->
    {T,lists:reverse(Acc)}.

generate_saw_(N,Amp,T,Dt,Acc) when N > 0 ->
    Y = Amp*(2*T - 1.0),
    generate_saw_(N-1,Amp,tstep(T,Dt),Dt,[Y|Acc]);
generate_saw_(_N,_Amp,T,_Dt,Acc) ->
    {T,lists:reverse(Acc)}.

tstep(T, Dt) ->
    T1 = T+Dt,
    if T1 > 1.0 -> T - trunc(T1);
       true -> T1
    end.

native_samples(Ys, s16_le, 1) ->
    << <<(trunc(Y*32768)):16/little>> || Y <- Ys >>;
native_samples(Ys, s16_le, 2) ->
    << <<(trunc(Y*32768)):16/little,(trunc(Y*32768)):16/little>> || 
	Y <- Ys >>.
