-module(alsa).
-export([init/0]).
-export([open/2, open/3, open/4, format_keys/0, close/1, strerror/1,
         print_setup/2, read/2, write/2, prepare/1, recover/3, drain/1]).
-export_type([handle/0, reason/0, read_reason/0, write_reason/0]).

-include("../include/alsa.hrl").

-on_load(init/0).

-type handle() :: integer().
-type device_name() :: string().

-type reason() :: integer().

-type format() ::
        s8 | u8 | s16_le | s16_be | u16_le | u16_be | s24_le | s24_be | u24_le |
        u24_be | s32_le | s32_be | u32_le | u32_be | float_le | float_be |
        float64_le | float64_be | iec958_subframe_le | iec958_subframe_be |
        mu_law | a_law | ima_adpcm | mpeg | gsm | s20_le | s20_be | u20_le |
        u20_be | special | s24_3le | s24_3be | u24_3le | u24_3be | s20_3le |
        s20_3be | u20_3le | u20_3be | s18_3le | s18_3be | u18_3le | u18_3be.
-type hw_params() :: #{format => format(),
                       channels => integer(),
                       rate => integer(),
                       period_size => integer(),
                       buffer_size => integer()}.

-type sw_params() :: #{start_threshold => integer()}.

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
           filename:join(code:priv_dir(malsa), ?MODULE), none).

%%
%% Exported: open
%%

-spec open(device_name(), playback | capture, hw_params(), sw_params()) ->
          {ok, handle(), hw_params(), sw_params()} | {error, reason()}.

open(DeviceName, Stream) ->
    open(DeviceName, Stream, #{}, #{}).

open(DeviceName, Stream, HwParams) ->
    open(DeviceName, Stream, HwParams, #{}).

open(_DeviceName, _Stream, _HwParams, _SwParams) ->
    exit(nif_library_not_loaded).

%%
%% Exported: format_keys
%%

format_keys() ->
    [u8, s16_le, s16_be, u16_le, u16_be, s24_le, s24_be, u24_le, u24_be, s32_le,
     s32_be, u32_le, u32_be, float_le, float_be, float64_le, float64_be,
     iec958_subframe_le, iec958_subframe_be, mu_law, a_law, ima_adpcm, mpeg,
     gsm, s20_le, s20_be, u20_le, u20_be, special, s24_3le, s24_3be, u24_3le,
     u24_3be, s20_3le, s20_3be, u20_3le, u20_3be, s18_3le, s18_3be, u18_3le,
     u18_3be].

%%
%% Exported: close
%%

-spec close(handle()) -> ok | {error, reason()}.

close(_Handle) ->
    exit(nif_library_not_loaded).

%%
%% Exported: strerror
%%

-spec strerror(reason() | write_reason() | read_reason()) -> string().

strerror(_Reason) ->
    exit(nif_library_not_loaded).

%%
%% Exported: print_setup
%%

-spec print_setup(handle(), stdout | stderr) -> ok.

print_setup(_Handle, _InputStream) ->
    exit(nif_library_not_loaded).

%%
%% Exported: read
%%

-spec read(handle(), integer()) -> {ok, binary()} | {error, read_reason()}.

read(_Handle, _Frames) ->
    exit(nif_library_not_loaded).

%%
%% Exported: write
%%

-spec write(handle(), binary()) -> {ok, integer()} | {error, write_reason()}.

write(_Handle, _Bin) ->
    exit(nif_library_not_loaded).

%%
%% Exported: prepare
%%

-spec prepare(handle()) -> ok | {error, reason}.

prepare(_Handle) ->
    exit(nif_library_not_loaded).

%%
%% Exported: recover
%%

-spec recover(handle(), reason(), boolean()) -> ok | {error, reason()}.

recover(_Handle, _ErrorReason, _Silent) ->
    exit(nif_library_not_loaded).

%%
%% Exported: drain
%%

-spec drain(handle()) -> ok | {error, reason()}.

drain(_Handle) ->
    exit(nif_library_not_loaded).
