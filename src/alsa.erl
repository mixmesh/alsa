-module(alsa).
-export([open/4, get_hw_params/1, set_hw_params/2, get_sw_params/1,
         set_sw_params/2, strerror/1, read/2, write/2]).
-export([init/0, preloaded_atoms/0]). % internal
-on_load(init/0).

-include("../include/alsa.hrl").

-type handle() :: reference().
-type pcm_name() :: string().
-type format() :: integer(). % SND_PCM_FORMAT_... in include/alsa.hrl
-type hw_params() :: #{format => format(),
                       channels => integer(),
                       rate => integer(),
                       period_size => integer(),
                       buffer_size => integer()}.
-type sw_params() :: #{start_threshold => integer()}.
-type alsa_reason() :: integer().
-type bad_param_reason() :: {bad_param, atom(), integer(), alsa_reason()}.
-type frames() :: integer().

%%
%% Exported: init
%%

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(alsa), alsa_nif), none).

%%
%% Exported: preloaded_atoms
%%

preloaded_atoms() ->
    [no_such_handle, playback, capture, bad_param, format, channels, rate,
     period_size, buffer_size, start_threshold, too_little_data, underrun,
     overrun, suspend_event].

%%
%% Exported: open
%%

-spec open(pcm_name(), playback | capture, hw_params(), sw_params()) ->
          {ok, handle(), hw_params(), sw_params()} |
          {error, alsa_reason() | bad_param_reason()}.

open(_PcmName, _Stream, _HwParams, _SwParams) ->
    exit(nif_library_not_loaded).

%%
%% Exported: get_hw_params
%%

-spec get_hw_params(handle()) ->
          {ok, hw_params()} | {error, alsa_reason() | no_such_handle}.

get_hw_params(_Handle) ->
    exit(nif_library_not_loaded).

%%
%% Exported: set_hw_params
%%

-spec set_hw_params(handle(), hw_params()) ->
          {ok, hw_params()} |
          {error, alsa_reason() | bad_param_reason() | no_such_handle}.

set_hw_params(_Handle, _HwParams) ->
    exit(nif_library_not_loaded).

%%
%% Exported: get_sw_params
%%

-spec get_sw_params(handle()) ->
          {ok, sw_params()} | {error, alsa_reason() | no_such_handle}.

get_sw_params(_Handle) ->
    exit(nif_library_not_loaded).

%%
%% Exported: set_sw_params
%%

-spec set_sw_params(handle(), sw_params()) ->
          {ok, sw_params()} |
          {error, alsa_reason() | bad_param_reason() | no_such_handle}.

set_sw_params(_Handle, _SwParams) ->
    exit(nif_library_not_loaded).

%%
%% Exported: strerror
%%

-spec strerror(alsa_reason() | bad_param_reason()) -> string().

strerror(_Reason) ->
    exit(nif_library_not_loaded).

%%
%% Exported: read
%%

-spec read(handle(), frames()) ->
          {ok, binary() | overrrun | suspend_event} |
          {error, alsa_reason() | no_such_handle | overrun | suspend_event}.

read(_Handle, _Frames) ->
    exit(nif_library_not_loaded).

%%
%% Exported: write
%%

-spec write(handle(), binary()) ->
          {ok, frames() | underrun | suspend_event} |
          {error,
           alsa_reason() |
           no_such_handle | underrun | suspend_event | too_little_data}.

write(_Handle, _Bin) ->
    exit(nif_library_not_loaded).
