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

