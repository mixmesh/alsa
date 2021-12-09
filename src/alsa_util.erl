%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Collection of useful functions
%%% @end
%%% Created : 10 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_util).

-compile(export_all).

-export([mono_to_stereo/2, mono_to_stereo/3]).
-export([stereo_to_mono/2]).
-exprot([async_wait_ready/1]).

mono_to_stereo(s16_le, Bin) ->
    mono_to_stereo(s16_le, Bin, 0.5).

mono_to_stereo(s16_le, Bin,Pan) ->
    << <<(trunc(X*(1.0-Pan))):16/signed-little, 
	 (trunc(X*Pan)):16/signed-little>> ||
	<<X:16/signed-little>> <= Bin >>.

stereo_to_mono(s16_le, Bin) ->
    << <<(max(X1,X2)):16/little-signed>> || 
	<<X1:16/little-signed,X2:16/little-signed>> <= Bin >>.

%% add timeout? probably not...
async_wait_ready(H) ->
    alsa:select_(H),
    receive
	{select,H,undefined,_Ready} ->
	    ok
    end.
