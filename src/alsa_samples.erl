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
-export([wave_set_time/2]).

-export([wave_set_envelope/6]).
-export([wave_set_attack/3]).
-export([wave_set_decay/3]).
-export([wave_set_sustain/3]).
-export([wave_set_release/3]).

-export([wave_set_level/5]).
-export([wave_set_low/3]).
-export([wave_set_peek/3]).
-export([wave_set_sust/3]).

-export([wave_set_wave/10]).
-export([wave_set_form/3]).
-export([wave_set_env/3]).
-export([wave_set_chan/3]).
-export([wave_set_f1/3]).
-export([wave_set_f2/3]).
-export([wave_set_f3/3]).
-export([wave_set_f4/3]).
-export([wave_set_f5/3]).
-export([wave_set_phase/3]).
-export([wave_set_noice/3]).
-export([wave/4]).
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
-type waveform() :: sine | square | triangle | saw | const.


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

%% set current time in seconds (typically 0.0 to reset time)
-spec wave_set_time(wavedef(), Time::number()) -> ok.
wave_set_time(_WaveDef, _Time) ->
        ?nif_stub.

-spec wave_set_envelope(wavedef(), Index::envind(), 
			Attack::number(), Decay::number(),
			Sustain::number(), Relase::number()) -> ok.
wave_set_envelope(_WaveDef, _Index, _Attach, _Decay, _Sustain, _Release) ->
    ?nif_stub.

-spec wave_set_level(wavedef(), Index::envind(), 
	       Low::float01(), Peek::float01(), Sust::float01()) -> ok.
wave_set_level(_Wavedef, _Index, _Low, _Peek, _Sust) ->
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

-spec wave_set_low(wavedef(), Index::envind(), Value::float01()) -> ok.
wave_set_low(_WaveDef, _Index, _Value) ->
    ?nif_stub.

-spec wave_set_peek(wavedef(), Index::envind(), Value::float01()) -> ok.
wave_set_peek(_WaveDef, _Index, _Value) ->
    ?nif_stub.

-spec wave_set_sust(wavedef(), Index::envind(), Value::float01()) -> ok.
wave_set_sust(_WaveDef, _Index, _Value) ->
    ?nif_stub.

-spec wave_set_wave(wavedef(), Index::waveind(), Form::waveform(),
		    F1::frequency(), F2::frequency(), F3::frequency(),
		    F4::frequency(), F5::frequency(), 
		    Phase::float01(), Noice::float01()) -> ok.
wave_set_wave(_WavDef, _Index, _Form, _F1, _F2, _F3, _F4, _F5,
	      _Phase, _Noice) ->
    ?nif_stub.

-spec wave_set_form(wavedef(), Index::waveind(), Form::waveform()) -> ok.
wave_set_form(_WavDef, _Index, _Form) ->
    ?nif_stub.

-spec wave_set_env(wavedef(), Index::waveind(), Env::envind()) -> ok.
wave_set_env(_WavDef, _Index, _Env) ->
    ?nif_stub.

-spec wave_set_chan(wavedef(), Index::waveind(), Chan::channel()) -> ok.
wave_set_chan(_WavDef, _Index, _Chan) ->
    ?nif_stub.

-spec wave_set_f1(wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f1(_WavDef, _Index, _Freq) ->
    ?nif_stub.

-spec wave_set_f2(wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f2(_WavDef, _Index, _Freq) ->
    ?nif_stub.

-spec wave_set_f3(wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f3(_WavDef, _Index, _Freq) ->
    ?nif_stub.

-spec wave_set_f4(wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f4(_WavDef, _Index, _Freq) ->
    ?nif_stub.

-spec wave_set_f5(wavedef(), Index::waveind(), Freq::frequency()) -> ok.
wave_set_f5(_WavDef, _Index, _Freq) ->
    ?nif_stub.

-spec wave_set_phase(wavedef(), Index::waveind(), Phase::float()) -> ok.
wave_set_phase(_WavDef, _Index, _Phase) ->
    ?nif_stub.

-spec wave_set_noice(wavedef(), Index::waveind(), Noice::float01()) -> ok.
wave_set_noice(_WavDef, _Index, _Noice) ->
    ?nif_stub.

test_wave(Rate,Freq) ->
    W = wave_new(),
    wave_set_rate(W, Rate),
    wave_set_envelope(W, 0, 0.1, 0.1, 0.2, 0.4),
    wave_set_level(W, 0, 0.0, 0.1, 0.5),
    wave_set_wave(W, 0, sine, Freq, Freq, Freq, Freq, Freq, 0, 0),
%%    wave_set_wave(W, 1, saw, 2*Freq+100, 2*Freq, 2*Freq, 2*Freq, 2*Freq, 0, 0),
%%    wave_set_wave(W, 2, sine, 8*Freq+200, 4*Freq, 4*Freq, 4*Freq, 4*Freq, 0, 0),
    wave_set_nwaves(W, 1),
    W.
    
test_play() -> test_play(8000,440).
test_play(Rate,Freq) ->
    W = test_wave(Rate, Freq),
    Samples = wave(W, s16_le, 1, Rate*2),
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

test_plot() -> test_plot(8000,440).
test_plot(Rate,Freq) ->
    W = test_wave(Rate, Freq),
    Samples = wave(W, s16_le, 1, 1000), %% Rate*2),
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
		    ["#set terminal postfile\n",
		     "#set output \"plot.ps\n",
		     "set title \"Sound Wave\"\n",
		     "set xlabel \"Time\"\n",
		     "set ylabel \"Amplitude\"\n",
		     "plot \"plot.dat\" with points\n"
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
