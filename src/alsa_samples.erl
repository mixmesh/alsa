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
-export([filter/4]).
%% wave
-export([wave_new/0]).
-export([wave_clear/1]).
-export([wave_set_num_waves/2]).
-export([wave_get_rate/1, wave_set_rate/2]).
-export([wave_set_mode/2]).
-export([wave_get_time/1, wave_set_time/2]).
-export([wave_get_pos/1, wave_set_pos/2]).
-export([wave_get_duration/1]).
-export([wave_set_mute/2]).
-export([wave_set_state/2]).

-export([wave_set_envelope/2]).
-export([wave_set_adsr/5]).
-export([wave_set_attack/2]).
-export([wave_set_decay/2]).
-export([wave_set_sustain/2]).
-export([wave_set_release/2]).
-export([wave_set_delay/2]).

-export([wave_set_wave/3]).
-export([wave_set_chan/3]).
-export([wave_set_level/3]).
-export([wave_set_form/3,  wave_set_form/4]).
-export([wave_set_freq/3,  wave_set_freq/4]).
-export([wave_set_phase/3, wave_set_phase/4]).
-export([wave_set_noice/3, wave_set_noice/4]).
-export([wave_set_samples/8]).
-export([wave_set_num_samples/3]).
-export([wave_get_num_samples/2]).
-export([mark/6, unmark/2, get_marks/3, get_marks/2]).
-export([wave/4]).
%% util
-export([create_wave/2]).
-export([set_wave/2]).
-export([to_frequency/1]).

%% TEST
-export([test_reformat/0]).
-export([test_reformat2/0]).
-export([test_amu_reformat/0]).
-export([test_play/0, test_play/1]).
-export([play/1]).
-export([test_plot/0, test_plot/1]).
-export([plot/1]).

-define(MAX_WAVE, 8).
-define(MAX_ENV,  8).
-define(MAX_CHANNELS, 8).

-type float01() :: float().               %% float in range 0..1
-type frequency() :: float().             %% Hertz 0..
-type waveind() :: 0..(?MAX_WAVE-1).      %% wave index
-type channel()  :: 0..(?MAX_CHANNELS-1). %% channel number
-type wavedef() :: reference().
-type waveform() :: sine | square | pulse | triangle | saw | const | none.
-type envmode() :: off|linear|quadratic|sustain.  %% (linear)
-type envelem() :: Duration::number() | {Duration::number(), Mode::envmode()}.
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
-type event() :: {Ref::reference(),Pid::pid(),Pos::integer(),UserData::term()}.

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

-spec resample(SrcRate::alsa:sample_rate(), DstRate::alsa:sample_rate(),
	       Format::alsa:format(), Channels::alsa:unsigned(),
	       Src::binary()) -> binary().

resample(_SrcRate, _DstRate, _Format, _Channels, _Src) ->
    ?nif_stub.    

-spec reformat(SrcFormat::alsa:format(), DstFormat::alsa:format(),
	       SrcChannels::alsa:unsigned(), DstChannels::alsa:unsigned(),
	       Src::binary()) -> binary().

reformat(_SrcFormat, _DstFormat, _SrcChannels, _DstChannels, _Src) ->
    ?nif_stub.

-spec filter(SrcFormat::alsa:format(), DstFormat::alsa:format(),
	     Filter::[float()], Src::binary()) -> binary().

filter(_SrcFormat, _DstFormat, _Filter, _Src) ->
    ?nif_stub.

-spec wave_new() -> wavedef().
wave_new() ->
    ?nif_stub.

-spec mark(W::wavedef(), Pid::pid(), Ref::reference(), Pos::integer(),
	   Flags::[notify|once|stop|restart|
		   {set,Pos::integer()}|
		   {repeat,Pos::integer(),Count::integer()}],
	   UserData :: term()) ->
	  ok.
	  
mark(_W, _Pid, _Ref, _Pos, _Flags, _UserData) ->
    ?nif_stub.    

-spec unmark(W::wavedef(), Ref::reference()) -> ok.
unmark(_W, _Ref) ->
    ?nif_stub.

-spec get_marks(W::wavedef(), From::integer(), To::integer()) -> 
	  [{reference(),{pid(),Pos::integer(),[Flag::term()],
			 UserData::term()}}].
get_marks(_W, _From, _To) ->
    ?nif_stub.

-spec get_marks(W::wavedef(), Period::integer()) ->
	  [{reference(),{pid(),Pos::integer(),[Flag::term()],
			 UserData::term()}}].
get_marks(_W, _Period) ->
    ?nif_stub.

-spec wave(wavedef(), Format::alsa:format(), Channels::alsa:unsigned(),
	   NumFrames::alsa:unsigned()) ->
	  {[event()], Samples::binary()}.
wave(_WaveDef, _Format, _Channels, _NumFrames) ->
    ?nif_stub.

-spec wave_clear(W::wavedef()) -> ok.
wave_clear(_W) ->
    ?nif_stub.

-spec wave_set_num_waves(W::wavedef(), Num::non_neg_integer()) -> ok.
wave_set_num_waves(_W, _Num) ->
    ?nif_stub.

-spec wave_get_rate(W::wavedef()) -> Rate::alsa:sample_rate().
wave_get_rate(_W) ->
        ?nif_stub.

-spec wave_set_rate(W::wavedef(), Rate::alsa:sample_rate()) -> ok.
wave_set_rate(_W, _Rate) ->
        ?nif_stub.

-spec wave_set_mode(W::wavedef(), sustain|off) -> ok.
wave_set_mode(_W, _Mode) ->
        ?nif_stub.

-spec wave_set_mute(W::wavedef(), boolean()) -> ok.
wave_set_mute(_W, _On) ->
    ?nif_stub.

-spec wave_set_state(W::wavedef(), running|stopped) -> ok.
wave_set_state(_W, _State) ->
    ?nif_stub.

%% set current time in seconds (typically 0.0 to reset time)
-spec wave_set_time(W::wavedef(), Time::number()) -> ok.
wave_set_time(_W, _Time) ->
        ?nif_stub.

-spec wave_get_time(W::wavedef()) -> 
	  Time::float().
wave_get_time(_W) ->
    ?nif_stub.

%% set current sample pos
-spec wave_set_pos(W::wavedef(), Pos::integer()) -> ok.
wave_set_pos(_W, _Pos) ->
        ?nif_stub.

-spec wave_get_pos(W::wavedef()) -> Pos::integer().
wave_get_pos(_W) ->
    ?nif_stub.

-spec wave_set_envelope(wavedef(), [envelem()]) -> ok.
wave_set_envelope(_WaveDef, _Elems) ->
    ?nif_stub.

-spec wave_set_adsr(wavedef(),
		    Attack::number(), Decay::number(),
		    Sustain::number(), Relase::number()) -> ok.
wave_set_adsr(_WaveDef, _Attach, _Decay, _Sustain, _Release) ->
    ?nif_stub.

-spec wave_set_level(wavedef(), Index::waveind(), [float01()]) -> ok.
wave_set_level(_Wavedef, _Index, _Pts) ->
    ?nif_stub.

-spec wave_set_attack(wavedef(), Value::number()) -> ok.
wave_set_attack(_WaveDef, _Value) ->
    ?nif_stub.

-spec wave_set_decay(wavedef(), Value::number()) -> ok.
wave_set_decay(_WaveDef, _Value) ->
    ?nif_stub.

-spec wave_set_sustain(wavedef(), Value::number()) -> ok.
wave_set_sustain(_WaveDef, _Value) ->
    ?nif_stub.

-spec wave_set_release(wavedef(), Value::number()) -> ok.
wave_set_release(_WaveDef, _Value) ->
    ?nif_stub.

-spec wave_set_delay(wavedef(), Value::number()) -> ok.
wave_set_delay(_WaveDef, _Value) ->
    ?nif_stub.

-spec wave_set_wave(W::wavedef(), Index::waveind(), [formdef()]) -> ok.

wave_set_wave(_W, _Index, _Ws) ->
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

-spec wave_set_noice(W::wavedef(), Index::waveind(), Pos::integer(),
		     Noice::float01()) -> ok.
wave_set_noice(_W, _Index, _Pos, _Noice) ->
    ?nif_stub.

-spec wave_set_samples(W::wavedef(), Index::waveind(), Offset::alsa:unsigned(),
		       Channel::integer(),
		       SrcRate::alsa:sample_rate(), SrcFormat::alsa:format(), 
		       SrcChannels::alsa:unsigned(), Src::binary()) -> ok.

wave_set_samples(_W, _Index, _Offset, _Channel,
		 _SrcRate, _SrcFormat, _SrcChannels, _Src) ->
    ?nif_stub.

-spec wave_set_num_samples(W::wavedef(), Index::waveind(),
			   Num::alsa:unsigned()) -> ok.
wave_set_num_samples(_W, _Index, _Num) ->
    ?nif_stub.

-spec wave_get_num_samples(W::wavedef(), Index::waveind()) -> 
	  Num::alsa:unsigned().
wave_get_num_samples(_W, _Index) ->
    ?nif_stub.


-spec wave_get_duration(W::wavedef()) -> Time::float().
wave_get_duration(_W) ->
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

create_wave(Rate, Def) ->
    W = wave_new(),
    wave_set_rate(W, Rate),
    _N = set_wave(W, Def),
    W.

set_wave(W, Def) ->
    Ni = set_wave(W, Def, 0),
    wave_set_num_waves(W, Ni),
    Ni.
    
set_wave(W, [D|Def], Ni) ->
    Nii = set_wave_(W, D, Ni),
    set_wave(W, Def, Nii);
set_wave(_W, [], Ni) ->
    Ni;
set_wave(W, D, Ni) ->
    set_wave_(W, D, Ni).
    
set_wave_(W,{wave,Wi,Fs}, Ni) ->
    Fs1 = [to_frequency(F) || F <- Fs],
    ok = wave_set_wave(W,Wi,Fs1),
    max(Wi+1,Ni);
set_wave_(W,{level, Wi, Levels}, Ni) ->
    ok = wave_set_level(W, Wi, Levels),
    max(Wi+1,Ni);
set_wave_(W,{adsr,A,D,S,R}, Ni) ->
    ok = wave_set_adsr(W,A,D,S,R),
    Ni;
set_wave_(W,{envelope,Elems}, Ni) ->
    ok = wave_set_envelope(W,Elems),
    Ni.

duration({adsr,_J, A,D,S,R}) -> A+D+S+R;
duration({envelope,_J,Es}) ->
    lists:sum([case E of
		   {Ti,_Mode} -> Ti;
		   Ti when is_number(Ti) -> Ti
	       end || E <- Es]);
duration(T) when is_tuple(T) -> 0;
duration(Ts) when is_list(Ts) -> lists:max([duration(T) || T <- Ts]).


to_frequency(T) when tuple_size(T) >= 2 ->
    setelement(2, T, to_frequency(element(2,T)));
to_frequency(M=#{ freq := Freq}) ->
    M#{ freq => to_frequency(Freq) };
to_frequency(M=#{ form := const}) ->
    M;
to_frequency(Name) when is_list(Name) ->
    Note = alsa_util:midi_name_to_note(Name),
    alsa_util:midi_note_to_frequency(Note);
to_frequency(Freq) when is_number(Freq) ->
    Freq.

wave1(Rate) ->
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
    W = create_wave(Rate, Def),
    {W, NFrames}.
    
test_play() -> test_play(8000).
test_play(Rate) ->
    {W,NFrames} = wave1(Rate),
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

test_plot() -> test_plot(8000).
test_plot(Rate) ->
    {W,NFrames} = wave1(Rate),
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

%% check format conversions over float
test_reformat2() ->
    Fs = [s8, s16_le, s16_be, s32_le, s32_be, s24_le, s24_be,
	  u8, u16_le, u16_be, u32_le, u32_be, u24_le, u24_be ],

    Gs = [float_le, float_be,
	  float64_le, float64_be],
    lists:foreach(
      fun(Value) ->
	      lists:foreach(
		fun(F) ->
			lists:foreach(
			  fun(G) ->
				  io:format("~w ~w ~w\n", [F, G, Value]),
				  Bin = alsa_util:encode_sample(F, Value),
				  Bin1 = reformat(F, G, 1, 1, Bin),
				  Bin2 = reformat(G, F, 1, 1, Bin1),
				  Value1 = alsa_util:decode_sample(F, Bin2),
				  io:format("Value1 = ~p\n", [Value1]),
				  case  Value1 of
				      Value -> ok
				  end
			  end, Gs)
		end, Fs)
      end, lists:seq(-100,100)).

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
