%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Display info about alsa hardware
%%% @end
%%% Created :  5 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_info).

-export([device/1, device/2]).

device(Device) ->
    device(Device, capture).
device(Device, Stream) ->
    {ok, H} = alsa:open_(Device, Stream),
    Info = alsa:get_hw_params_range(H),
    alsa:close_(H),
    Info.
