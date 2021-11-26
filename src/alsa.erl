-module(alsa).
-export([init/0]).
-export([open/4, get_hw_params/1, set_hw_params/2, get_sw_params/1,
         set_sw_params/2, close/1, strerror/1, read/2, write/2, prepare/1,
         recover/3, drain/1]).

-include("../include/alsa.hrl").

-on_load(init/0).

-type handle() :: integer().
-type device_name() :: string().

-type format() :: integer(). % Any of SND_PCM_FORMAT_... in include/alsa.hrl

-type hw_params() :: #{format => format(),
                       channels => integer(),
                       rate => integer(),
                       period_size => integer(),
                       buffer_size => integer()}.

-type sw_params() :: #{start_threshold => integer()}.

-type alsa_reason() :: integer().

-type bad_param_reason() :: {bad_param, atom(), integer(), alsa_reason()}.

-type read_reason() ::
        ?ALSA_OVERRUN |
        ?ALSA_WAITING_FOR_RECOVERY |
        ?ALSA_NOT_PREPARED_NOR_RUNNING.

-type write_reason() ::
        ?ALSA_UNDERRUN |
        ?ALSA_WAITING_FOR_RECOVERY |
        ?ALSA_NOT_PREPARED_NOR_RUNNING.

%%
%% Exported: init
%%

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(alsa), alsa_nif), none).

%%
%% Exported: open
%%

-spec open(device_name(), playback | capture, hw_params(), sw_params()) ->
          {ok, handle(), hw_params(), sw_params()} |
          {error, alsa_reason() | bad_param_reason()}.

open(_DeviceName, _Stream, _HwParams, _SwParams) ->
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
%% Exported: close
%%

-spec close(handle()) -> ok | {error, alsa_reason() | no_such_handle}.

close(_Handle) ->
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

%% NOT DONE
-spec read(handle(), integer()) -> {ok, binary()} | {error, read_reason()}.

read(_Handle, _Frames) ->
    exit(nif_library_not_loaded).

%%
%% Exported: write
%%

%% NOT DONE
-spec write(handle(), binary()) -> {ok, integer()} | {error, write_reason()}.

write(_Handle, _Bin) ->
    exit(nif_library_not_loaded).

%%
%% Exported: prepare
%%

%% NOT DONE
-spec prepare(handle()) -> ok | {error, reason}.

prepare(_Handle) ->
    exit(nif_library_not_loaded).

%%
%% Exported: recover
%%

%% NOT DONE
-spec recover(handle(), alsa_reason(), boolean()) -> ok | {error, alsa_reason()}.

recover(_Handle, _ErrorReason, _Silent) ->
    exit(nif_library_not_loaded).

%%
%% Exported: drain
%%

%% NOT DONE
-spec drain(handle()) -> ok | {error, alsa_reason()}.

drain(_Handle) ->
    exit(nif_library_not_loaded).
