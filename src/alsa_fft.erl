%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    FFT library
%%% @end
%%% Created : 20 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(alsa_fft).

-on_load(init/0).

-export([new/1]).
-export([clear/1]).
-export([size/1]).
-export([resize/2]).
-export([fft/4]).
-export([rfft/4]).
-export([ifft/3]).
-export([set_rectangular/1]).
-export([set_bartlett/1]).
-export([set_hanning/1]).
-export([set_hamming/1]).
-export([set_blackman/1]).
-export([set_blackman_harris/1]).
-export([set_custom/5]).

-define(nif_stub,nif_stub_error(?LINE)).

nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(alsa), alsa_fft_nif), none).

new(_Size) ->
    ?nif_stub.

clear(_FFT) ->
    ?nif_stub.

size(_FFT) ->
    ?nif_stub.

resize(_FFT, _Size) ->
    ?nif_stub.

%% input is <<real,imag...>> output is [ <<float real, float imag>>.. ]
fft(_FFT, _Format, _Channels, _Data) ->
    ?nif_stub.
%% Data is <<readl ...> output is [  <<float ...>> ]
rfft(_FFT, _Format, _Channels, _Data) ->
    ?nif_stub.

%% inverse fft: input is <<float read,float imag...> 
%% output is <<real,imag...>> accoring to format 
ifft(_FFT, _Format, _Data) ->
    ?nif_stub.

set_rectangular(_FFT) ->
    ?nif_stub.

set_bartlett(_FFT) ->
    ?nif_stub.

set_hanning(_FFT) ->
    ?nif_stub.

set_hamming(_FFT) ->
    ?nif_stub.

set_blackman(_FFT) ->
    ?nif_stub.

set_blackman_harris(_FFT) ->
    ?nif_stub.

set_custom(_FFT, _C0, _C1, _C2, _C3) ->
    ?nif_stub.    

