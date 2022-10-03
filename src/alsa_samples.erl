%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Sample buffer library
%%% @end
%%% Created : 5 Sep 2022 by Tony Rogvall <tony@rogvall.se>

-module(alsa_samples).

-on_load(init/0).

-export([mix/4]).
-export([resample/5]).
-export([reformat/5]).
%% wave
-export([wave_new/0]).
-export([wave_set_nwaves/2]).
-export([wave_set_rate/2]).
-export([wave_set_mode/2]).
-export([wave_set_time/2]).

-export([wave_set_envelope/3]).
-export([wave_set_adsr/6]).
-export([wave_set_attack/3]).
-export([wave_set_decay/3]).
-export([wave_set_sustain/3]).
-export([wave_set_release/3]).
-export([wave_set_delay/3]).

-export([wave_set_wave/3]).
-export([wave_set_env/3]).
-export([wave_set_chan/3]).
-export([wave_set_level/3]).
-export([wave_set_form/3, wave_set_form/4]).
-export([wave_set_freq/3, wave_set_freq/4]).
-export([wave_set_phase/3, wave_set_phase/4]).
-export([wave_set_noice/3, wave_set_noice/4]).
-export([wave_set_f1/3]).
-export([wave_set_f2/3]).
-export([wave_set_f3/3]).
-export([wave_set_f4/3]).
-export([wave_set_f5/3]).
-export([wave/4]).
%% util
-export([create_wave/2]).
-export([to_frequency/1]).

%% TEST
-export([test_reformat/0]).
-export([test_amu_reformat/0]).
-export([test_play/0, test_play/2]).
-export([play/1]).
-export([test_plot/0, test_plot/2]).
-export([plot/1]).

-define(MAX_WAVE, 8).
-define(MAX_ENV,  8).
-define(MAX_CHANNELS, 8).

-type float01() :: float().               %% float in range 0..1
-type frequency() :: float().             %% Hertz 0..
-type waveind() :: 0..(?MAX_WAVE-1).      %% wave index
-type envind()  :: 0..(?MAX_ENV-1).       %% envelope index
-type channel()  :: 0..(?MAX_CHANNELS-1). %% channel number
-type wavedef() :: reference().
-type waveform() :: sine | square | pulse | triangle | saw | const.
-type envmode() :: off|linear|quadratic|sustain.  %% (linear)
-type envelem() :: {Duration::number(), Mode::envmode()} |
		   {Duration::number()} |
		   Duration::number().
-type formdef() :: #{ form => waveform(),
		      freq => string() | number(),
		      level => float01(),
		      phase => float(),     %% mod 2pi
		      noice => float01() } |
		   [{Form::waveform(),
		     Freq::frequency(),
		     Level::float01(),
		     Phase::float(),
		     Noice::float01()}].

-define(nif_stub,nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(alsa), alsa_samples_nif), none).

-spec mix(Format::alsa:format(), Channels::alsa:unsigned(),
	  [Src::binary()], [Control::alsa:channel_control()]) ->
	  Dst::binary().

mix(_Format, _Channels, _SrcList, _ControlList) ->
    ?nif_stub.    

-spec resample(SrcRate::alsa:unsigned(), DstRate::alsa:unsigned(),
	       Format::alsa:format(), Channels::alsa:unsigned(),
	       Src::binary()) -> binary().

resample(_SrcRate, _DstRate, _Format, _Channels, _Src) ->
    ?nif_stub.    

-spec reformat(SrcFormat::alsa:format(), DstFormat::alsa:format(),
	       SrcChannels::alsa:unsigned(), DstChannels::alsa:unsigned(),
	       Src::binary()) -> binary().

reformat(_SrcFormat, _DstFormat, _SrcChannels, _DstChannels, _Src) ->
    ?nif_stub.

-spec wave_new() -> wavedef().
wave_new() ->
    ?nif_stub.

-spec wave(wavedef(), Format::alsa:format(), Channels::alsa:unsigned(),
	   NumFrames::alsa:unsigned()) ->
	  binary().
wave(_WaveDef, _Format, _Channels, _NumFrames) ->
    ?nif_stub.

-spec wave_set_nwaves(wavedef(), Num::non_neg_integer()) -> ok.
wave_set_nwaves(_WaveDef, _Num) ->
        ?nif_stub.

-spec wave_set_rate(wavedef(), Rate::non_neg_integer()) -> ok.
wave_set_rate(_WaveDef, _Rate) ->
        ?nif_stub.

-spec wave_set_mode(wavedef(), sustain|off) -> ok.
wave_set_mode(_WaveDef, _Mode) ->
        ?nif_stub.

%% set current time in seconds (typically 0.0 to reset time)
-spec wave_set_time(wavedef(), Time::number()) -> ok.
wave_set_time(_WaveDef, _Time) ->
        ?nif_stub.

-spec wave_set_envelope(wavedef(), Index::envind(), [envelem()]) -> ok.
wave_set_envelope(_WaveDef, _Index, _Elems) ->
    ?nif_stub.

-spec wave_set_adsr(wavedef(), Index::envind(), 
		    Attack::number(), Decay::number(),
		    Sustain::number(), Relase::number()) -> ok.
wave_set_adsr(_WaveDef, _Index, _Attach, _Decay, _Sustain, _Release) ->
    ?nif_stub.

-spec wave_set_level(wavedef(), Index::waveind(), [float01()]) -> ok.
wave_set_level(_Wavedef, _Index, _Pts) ->
    ?nif_stub.

-spec wave_set_attack(wavedef(), Index::envind(), Value::number()) -> ok.
wave_set_attack(_WaveDef, _Index, _Value) ->
    ?nif_stub.

-spec wave_set_decay(wavedef(), Index::envind(), Value::number()) -> ok.
wave_set_decay(_WaveDef, _Index, _Value) ->
    ?nif_stub.

-spec wave_set_sustain(wavedef(), Index::envind(), Value::number()) -> ok.
wave_set_sustain(_WaveDef, _Index, _Value) ->
    ?nif_stub.

-spec wave_set_release(wavedef(), Index::envind(), Value::number()) -> ok.
wave_set_release(_WaveDef, _Index, _Value) ->
    ?nif_stub.

-spec wave_set_delay(wavedef(), Index::envind(), Value::number()) -> ok.
wave_set_delay(_WaveDef, _Index, _Value) ->
    ?nif_stub.

-spec wave_set_wave(W::wavedef(), Index::waveind(), [formdef()]) -> ok.

wave_set_wave(_W, _Index, _Ws) ->
    ?nif_stub.

-spec wave_set_env(W::wavedef(), Index::waveind(), Env::envind()) -> ok.
wave_set_env(_W, _Index, _Env) ->
    ?nif_stub.

-spec wave_set_chan(W::wavedef(), Index::waveind(), Chan::channel()) -> ok.
wave_set_chan(_W, _Index, _Chan) ->
    ?nif_stub.

-spec wave_set_form(W::wavedef(), Index::waveind(), Pt::integer(), 
		    Form::waveform()) -> ok.
wave_set_form(_W, _Index, _Pt, _Form) ->
    ?nif_stub.

-spec wave_set_phase(W::wavedef(), Index::waveind(), Pos::integer(), 
		     Phase::float()) -> ok.
wave_set_phase(_W, _Index, _Pos, _Phase) ->
    ?nif_stub.

-spec wave_set_phase(W::wavedef(), Index::waveind(), Phase::float()) -> ok.
wave_set_phase(W, Index, Phase) ->
    wave_set_phase(W, Index, 0, Phase).

-spec wave_set_noice(wavedef(), Index::waveind(), Pos::integer(),
		     Noice::float01()) -> ok.
wave_set_noice(_WavDef, _Index, _Pos, _Noice) ->
    ?nif_stub.

-spec wave_set_noice(wavedef(), Index::waveind(), Noice::float01()) -> ok.
wave_set_noice(W, Index, Noice) -> wave_set_noice(W, Index, 0, Noice).

-spec wave_set_form(wavedef(), Index::waveind(), Form::waveform()) -> ok.
wave_set_form(W, Index, Form) ->
    wave_set_form(W, Index, 0, Form).

-spec wave_set_freq(W::wavedef(), Index::waveind(), Pos::integer(), Freq::frequency()) -> ok.
wave_set_freq(_W, _Index, _Pos, _Freq) ->
    ?nif_stub.

-spec wave_set_freq(W::wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_freq(W, Index, Freq) ->
    wave_set_freq(W, Index, 0, Freq).

-spec wave_set_f1(W::wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f1(W, Index, Freq) ->
    wave_set_freq(W, Index, 0, Freq).

-spec wave_set_f2(W::wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f2(W, Index, Freq) ->
    wave_set_freq(W, Index, 1, Freq).

-spec wave_set_f3(W::wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f3(W, Index, Freq) ->
    wave_set_freq(W, Index, 2, Freq).

-spec wave_set_f4(W::wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f4(W, Index, Freq) ->
    wave_set_freq(W, Index, 3, Freq).

-spec wave_set_f5(wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f5(W, Index, Freq) ->
    wave_set_freq(W, Index, 3, Freq).


create_wave(Rate, Def) ->
    W = wave_new(),
    wave_set_rate(W, Rate),
    N = num_waves(Def),
    io:format("n waves = ~w\n", [N]),
    ok = create_wave_(W, Def),
    wave_set_nwaves(W, N),
    {ok,W}.

%% calculate number of waves in def
num_waves(T) when is_tuple(T) ->
    if element(1, T) =:= wave -> element(2, T)+1;
       true -> 0
    end;
num_waves(Ts) when is_list(Ts) ->
    lists:max([num_waves(T) || T <- Ts, is_tuple(T)]).

duration({adsr,_J, A,D,S,R}) -> A+D+S+R;
duration({envelope,_J,Es}) ->
    lists:sum([case E of
		   {Ti,_Mode} -> Ti;
		   Ti when is_number(Ti) -> Ti
	       end || E <- Es]);
duration(T) when is_tuple(T) -> 0;
duration(Ts) when is_list(Ts) -> lists:max([duration(T) || T <- Ts]).

create_wave_(W, [D|Def]) ->
    set_wave_(W, D),
    create_wave_(W, Def);
create_wave_(_W, []) ->
    ok;
create_wave_(W, D) ->
    set_wave_(W, D).
    
set_wave_(W,{wave,Wi,Fs}) ->
    Fs1 = [to_frequency(F) || F <- Fs],
    wave_set_wave(W,Wi,Fs1);
set_wave_(W,{level, Wi, Levels}) ->
    wave_set_level(W, Wi, Levels);
set_wave_(W,{adsr,J, A,D,S,R}) ->
    wave_set_adsr(W,J,A,D,S,R);
set_wave_(W,{envelope,J, Elems}) ->
    wave_set_envelope(W,J,Elems).

to_frequency(T) when tuple_size(T) >= 2 ->
    setelement(2, T, to_frequency(element(2,T)));
to_frequency(M=#{ freq := Freq}) ->
    M#{ freq => to_frequency(Freq) };
to_frequency(Name) when is_list(Name) ->
    Note = alsa_util:midi_name_to_note(Name),
    alsa_util:midi_note_to_frequency(Note);
to_frequency(Freq) when is_number(Freq) ->
    Freq.

wave1(Form, Rate) ->
    Def = [{envelope,0, [ 2.0, 2.0 ]},
	   {wave, 0, [#{form=>sine,freq=>"C4",level=>0.0},
		      #{form=>square,freq=>"C4",level=>0.9},
		      #{form=>sine,freq=>"C4",level=>0.0}]},
	   {wave, 1, [#{form=>sine,freq=>"E4",level=>0.0},
		      #{form=>square,freq=>"E4",level=>0.9},
		      #{form=>sine,freq=>"E4",level=>0.0}]}
	  ],
    Dur = duration(Def),
    io:format("wave1: duration=~w\n", [Dur]),
    NFrames = round(Rate*Dur),
    io:format("wave1: nframes=~w\n", [NFrames]),
    {ok,W} = create_wave(Rate, Def),
    {W, NFrames}.
    
test_play() -> test_play(sine,8000).
test_play(Form,Rate) ->
    {W,NFrames} = wave1(Form,Rate),
    Samples = wave(W, s16_le, 1, NFrames),
    play({[{format,s16_le},{rate,Rate},{channels,1}], Samples}).

play(Samples) when is_binary(Samples) ->
    play_([{format,s16_le},{rate, 16000},{channels,1}], Samples);
play({Params,Samples}) ->
    play_(Params, Samples).

play_(Params, Samples) when is_list(Params) ->
    case file:open(Samples, [ram, read, binary]) of
	{ok,Fd} ->
	    try alsa_playback:fd(Fd, Params) of
		Result -> Result
	    after
		file:close(Fd)
	    end;
	{error,Reason} ->
	    {error, file:format_error(Reason)}
    end.

test_plot() -> test_plot(sine,8000).
test_plot(Form,Rate) ->
    {W,NFrames} = wave1(Form,Rate),
    Samples = wave(W, s16_le, 1, NFrames),
    plot({[{format,s16_le},{rate,Rate},{channels,1}], Samples}).

plot(Samples) when is_binary(Samples) ->
    plot_([{format,s16_le},{rate, 16000},{channels,1}], Samples);
plot({Params,Samples}) ->
    plot_(Params, Samples).

plot_(Params, Samples) ->
    Format = proplists:get_value(format, Params),
    Rate = proplists:get_value(rate, Params),
    Channels = proplists:get_value(channels, Params),
    FrameSize = alsa:format_size(Format, Channels),
    Data = plot_(0.0, Samples, 1/Rate, Format, FrameSize),
    file:write_file("plot.dat", Data),
    file:write_file("plot.cmd", 
		    ["set title \"Sound Wave\"\n",
		     "set xlabel \"Time\"\n",
		     "set ylabel \"Amplitude\"\n",
		     "plot \"plot.dat\" using 1:2 with lines\n"
		     "pause -1 \"Hit any key to continue\"\n"]),
    ok.

plot_(_T, <<>>, _Dt, _Format, _FrameSize) ->
    [];
plot_(T, Bin, Dt, Format, FrameSize) ->
    <<Frame:FrameSize/binary, Bin1/binary>> = Bin,
    [Y0|_Ys] = alsa_util:decode_frame(Frame, Format),
    Y = if is_integer(Y0) -> Y0/16#80000000;
	   is_float(Y0) -> Y0
	end,
    [[io_lib_format:fwrite_g(T),$\s,io_lib_format:fwrite_g(Y),$\n] |
     plot_(T+Dt, Bin1, Dt, Format, FrameSize)].

%% Linear formats
test_reformat() ->
    Fs = [s8, u8,
	  s16_le, s16_be, u16_le, u16_be,
	  s24_le, s24_be, u24_le, u24_be,
	  s32_le, s32_be, u32_le, u32_be,
	  float_le, float_be,
	  float64_le, float64_be
	 ],
    Samples =  lists:seq(-1,-100,-5) ++ [0] ++ lists:seq(1,100,5),
    Data = << <<S:8/signed>> || S <- Samples>>,
    lists:foreach(
      fun(F) ->
	      io:format("Testing s8 => ~s\n", [F]),
	      Data1 = reformat(s8, F, 1, 1, Data),
	      io:format("Data1 = ~p\n", [Data1]),
	      Data = reformat(F, s8, 1, 1, Data1)
      end, Fs).
    
%%	
test_amu_reformat() ->
    Fs = [ mu_law, a_law ],
    Samples =  lists:seq(-1,-10000,-1000) ++ [0] ++ lists:seq(1,10000,1000),
    Data = << <<S:16/signed>> || S <- Samples>>,
    lists:foreach(
      fun(F) ->
	      io:format("Testing s8 => ~s\n", [F]),
	      Data1 = reformat(s16_be, F, 1, 1, Data),
	      io:format("Data1 = ~p\n", [Data1]),
	      Data2 = reformat(F, s16_be, 1, 1, Data1),
	      diff(Data, Data2, 174)
      end, Fs).

diff(<<A:16/signed, As/binary>>, <<B:16/signed, Bs/binary>>, Eps) ->
    D = abs(A - B),
    if D < Eps ->
	    diff(As, Bs, Eps);
       true ->
	    error({diff, D})
    end;
diff(<<>>, <<>>, _Eps) ->
    ok.
