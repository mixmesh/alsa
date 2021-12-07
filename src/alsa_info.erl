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
    Info = alsa:get_hw_params_(H, 
			       [rate_min, rate_max,
				channels_min, channels_max,
				period_size_min, period_size_max,
				buffer_size]),
    alsa:close_(H),
    Info.



    
