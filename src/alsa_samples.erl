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

%% TEST
-export([test_reformat/0]).
-export([test_amu_reformat/0]).

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
