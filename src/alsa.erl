-module(alsa).

-on_load(init/0).

%% blocking api
-export([open/4,
	 drain/1,
	 close/1,
	 read/2,
	 write/2]).

%% "internal" NIF (nonblocking) api
-export([open_/2, 
	 close_/1,
	 read_/2, 
	 write_/2,
	 prepare_/1,
	 start_/1,
	 drain_/1,
	 drop_/1,
	 recover_/2,
	 reset_/1,
	 pause_/1,
	 resume_/1,
	 avail_/1,
	 state_/1,
	 strerror/1,
	 get_hw_params_/1,
	 set_hw_params_/2,
	 get_sw_params_/1,
	 set_sw_params_/2, 
	 select_/1
	 ]).
-export([
	 format_is_signed/1,
	 format_is_unsigned/1,
	 format_is_little/1,
	 format_is_big/1,
	 format_width/1,
	 format_physical_width/1,
	 format_size/2,
	 format_silence/1,
	 make_silence/3
	]).
-export([formats/0]).

-export([preloaded_atoms_/0]). % internal


-include("../include/alsa.hrl").

-type handle() :: reference().
-type pcm_name() :: string().
-type format() :: atom() | integer(). % SND_PCM_FORMAT_... in include/alsa.hrl
-type hw_params() :: #{format => format(),
                       channels => integer(),
                       rate => integer(),
                       period_size => integer(),
                       buffer_size => integer()}.
-type sw_params() :: #{start_threshold => integer()}.
-type alsa_reason() :: integer().
-type bad_param_reason() :: {bad_param, atom(), integer(), alsa_reason()}.
-type frames() :: integer().
-type samples() :: binary().
-type samples_header() :: frames().  %% add sample type etc here!

-type bytes() :: integer().
-type alsa_state() ::
	open | setup | prepared | running | xrun | draining | paused |
	suspended | disconnected | private1.

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

%%
%% Exported: init
%%

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(alsa), alsa_nif), none).

%%
%% Internal Exported: preloaded_atoms_
%%

preloaded_atoms_() ->
    [
     %% general 
     undefined, true, false, ok, error,

     %% open mode
     playback, capture,
     %% hw-params
     format, channels, rate, period_size, buffer_size,
     %% sw-params
     start_threshold,
     %% high level errors
     underrun, overrun, suspend_event, would_block, system_call,
     %% posix errors used as atoms
     eagain, eintr, estrpipe, epipe,
     %% application errors
     no_such_handle, bad_param, select_already,   

     %% state atoms - mostly debugging
     open, setup, prepared, running, xrun, draining, paused,
     suspended, disconnected, private1,
     
     %% formats
     s8, u8,
     s16_le, s16_be, u16_le, u16_be,
     s24_le, s24_be, u24_le, u24_be,
     s32_le, s32_be, u32_le, u32_be,
     float_le, float_be,
     float64_le, float64_be,
     iec958_subframe_le,
     iec958_subframe_be,
     mu_law, a_law ,
     ima_adpcm,
     g723_24, g723_40,
     dsd_u8, dsd_u16_le, dsd_u32_le,
     dsd_u16_be, dsd_u32_be,
     mpeg, gsm,
     s20_le, s20_be, u20_le, u20_be,
     special,
     s24_3le, s24_3be, u24_3le, u24_3be,
     s20_3le, s20_3be, u20_3le, u20_3be,
     s18_3le, s18_3be, u18_3le, u18_3be,
     g723_24_1b, g723_40_1b
    ].

formats() ->
    [
     s8, u8,
     s16_le, s16_be, u16_le, u16_be,
     s24_le, s24_be, u24_le, u24_be,
     s32_le, s32_be, u32_le, u32_be,
     float_le, float_be,
     float64_le, float64_be,
     iec958_subframe_le,
     iec958_subframe_be,
     mu_law, a_law ,
     ima_adpcm,
     g723_24, g723_40,
     dsd_u8, dsd_u16_le, dsd_u32_le,
     dsd_u16_be, dsd_u32_be,
     mpeg, gsm,
     s20_le, s20_be, u20_le, u20_be,
     special,
     s24_3le, s24_3be, u24_3le, u24_3be,
     s20_3le, s20_3be, u20_3le, u20_3be,
     s18_3le, s18_3be, u18_3le, u18_3be,
     g723_24_1b, g723_40_1b
    ].
	    
%%
%% Exported: open
%%

-spec open(pcm_name(), playback | capture, hw_params(), sw_params()) ->
          {ok, handle(), hw_params(), sw_params()} |
          {error, alsa_reason() | bad_param_reason()}.

open(PcmName, Stream, HwParams, SwParams) ->
    case open_(PcmName, Stream) of
	{ok, Handle} ->
	    case set_hw_params_(Handle, HwParams) of
		{ok, HwParams1} ->
		    case set_sw_params_(Handle, SwParams) of
			{ok, SwParams1} ->
			    case prepare_(Handle) of
				ok -> {ok, Handle, HwParams1, SwParams1};
				Error -> open_error_(Handle, Error)
			    end;
			Error -> open_error_(Handle, Error)
		    end;
		Error -> open_error_(Handle, Error)
	    end;
	Error ->
	    Error
    end.

open_error_(Handle, Error) ->
    close_(Handle),
    Error.

-spec open_(pcm_name(), playback | capture) ->
          {ok, handle()} |
          {error, alsa_reason() | bad_param_reason()}.

open_(_PcmName, _Stream) ->
    ?nif_stub.

%%
%% Exported: close
%%
-spec close(handle()) ->
          ok | {error, alsa_reason() | no_such_handle}.

close(Handle) ->
    close_(Handle).


-spec close_(handle()) ->
          ok | {error, alsa_reason() | no_such_handle}.

close_(_Handle) ->
    ?nif_stub.

%%
%% Exported: get_hw_params
%%

-spec get_hw_params_(handle()) ->
          {ok, hw_params()} | {error, alsa_reason() | no_such_handle}.

get_hw_params_(_Handle) ->
    ?nif_stub.

%%
%% Exported: set_hw_params
%%

-spec set_hw_params_(handle(), hw_params()) ->
          {ok, hw_params()} |
          {error, alsa_reason() | bad_param_reason() | no_such_handle}.

set_hw_params_(_Handle, _HwParams) ->
    ?nif_stub.

%%
%% Exported: get_sw_params
%%

-spec get_sw_params_(handle()) ->
          {ok, sw_params()} | {error, alsa_reason() | no_such_handle}.

get_sw_params_(_Handle) ->
    ?nif_stub.

%%
%% Exported: set_sw_params
%%

-spec set_sw_params_(handle(), sw_params()) ->
          {ok, sw_params()} |
          {error, alsa_reason() | bad_param_reason() | no_such_handle}.

set_sw_params_(_Handle, _SwParams) ->
    ?nif_stub.

%%
%% Exported: strerror
%%

-spec strerror(alsa_reason() | bad_param_reason()) -> string().

strerror(_Reason) ->
    ?nif_stub.

%%
%% Exported: read
%%
-spec read(handle(), frames()) ->
          {ok, binary() | overrrun | suspend_event} |
          {error, alsa_reason() | no_such_handle | overrun | suspend_event}.

read(Handle, Frames) ->
    read(Handle, Frames, []).

read(Handle, Frames, Acc) ->
    case read_(Handle, Frames) of
	{error, eagain} ->
	    ok = select_(Handle),
	    receive
		{select,Handle,undefined,_Ready} ->
		    io:format("read continue\n", []),
		    read(Handle, Frames, Acc)
	    end;
	{error, epipe} ->
	    case recover_(Handle, epipe) of
		ok ->
		    read(Handle, Frames, Acc);
		_ ->
		    {error, underrun}
	    end;
	{error, estrpipe} ->
	    case recover_(Handle, estrpipe) of
		ok ->
		    read(Handle, Frames, Acc);
		_ ->
		    {error, suspsend_event}
	    end;
	{error, AlsaError} when is_integer(AlsaError) ->
	    {error, strerror(AlsaError)};
	{ok, {ReadFrames,Samples}} ->
	    if Frames =:= ReadFrames ->
		    if Acc =:= [] ->
			    {ok, Samples};
		       true ->
			    {ok,iolist_to_binary(lists:reverse([Samples|Acc]))}
		    end;
	       true ->
		    read(Handle, Frames-ReadFrames, [Samples|Acc])
	    end
    end.


-spec read_(handle(), frames()) ->
          {ok, {samples_header(),samples()}} |
	  {ok, overrrun} | 
	  {ok, suspend_event} |
          {error, alsa_reason() | no_such_handle | overrun | suspend_event}.

read_(_Handle, _Frames) ->
    ?nif_stub.

%%
%% Exported: write
%%

-spec write_(handle(), binary()) ->
          {ok, bytes() | underrun | suspend_event} |
          {error,
           alsa_reason() |
           no_such_handle | underrun | suspend_event}.

write_(_Handle, _Bin) ->
    ?nif_stub.

%%
%% Exported: write  (blocking)
%%
-spec write(handle(), binary()) ->
          {ok, bytes() | underrun | suspend_event} |
          {error,
           alsa_reason() |
           no_such_handle | underrun | suspend_event}.

write(Handle, Bin) ->
    write(Handle, Bin, 0).

write(Handle, Bin, SoFar) ->
    Size = byte_size(Bin),
    case write_(Handle, Bin) of
	{error, eagain} ->
	    ok = select_(Handle),
	    receive
		{select,Handle,undefined,_Ready} ->
		    io:format("write continue: ~w\n", [SoFar]),
		    write(Handle, Bin, SoFar)
	    end;
	{error, epipe} ->
	    case recover_(Handle, epipe) of
		ok ->
		    {ok, underrun};
		_ ->
		    {error, underrun}
	    end;
	{error, estrpipe} ->
	    case recover_(Handle, estrpipe) of
		ok ->
		    {ok, suspend_event};
		_ ->
		    {error, suspsend_event}
	    end;
	{error, AlsaError} when is_integer(AlsaError) ->
	    {error, strerror(AlsaError)};
	{ok, Written} ->
	    if Written =:= Size ->
		    {ok, Written+SoFar};
	       true ->
		    <<_:Written/binary, Bin1/binary>> = Bin,
		    write(Handle, Bin1, SoFar+Written)
	    end
    end.

%%
%% Exported: draing (blocking)
%%
-spec drain(handle()) -> ok | {error, alsa_reason()}.

drain(Handle) ->
    case drain_(Handle) of
	{error, eagain} ->
	    ok = select_(Handle),
	    receive
		{select,Handle,undefined,_Ready} ->
		    io:format("drain continue\n"),
		    drain(Handle)
	    end;
	Result ->
	    Result
    end.

%%
%% Exported: draw_ (non-blocking)
%%
-spec drain_(handle()) -> ok | {error, alsa_reason()}.

drain_(_Handle) ->
    ?nif_stub.

%%
%% Exported: drop_
%%
-spec drop_(handle()) -> ok | {error, alsa_reason()}.

drop_(_Handle) ->
    ?nif_stub.

%%
%% Exported: prepare_
%%
-spec prepare_(handle()) -> ok | {error, alsa_reason()}.

prepare_(_Handle) ->
    ?nif_stub.

%%
%% Exported: start_
%%
-spec start_(handle()) -> ok | {error, alsa_reason()}.

start_(_Handle) ->
    ?nif_stub.

%%
%% Exported: recover_
%%
-spec recover_(handle(), From::epipe|estrpipe|eintr) ->
	  ok | {error, alsa_reason()}.

recover_(_Handle, _Type) ->
    ?nif_stub.

%%
%% Exported: reset_
%%
-spec reset_(handle()) -> ok | {error, alsa_reason()}.

reset_(_Handle) ->
    ?nif_stub.

%%
%% Exported: pause_
%%
-spec pause_(handle()) -> ok | {error, alsa_reason()}.

pause_(_Handle) ->
    ?nif_stub.

%%
%% Exported: resume_
%%
-spec resume_(handle()) -> ok | {error, alsa_reason()}.

resume_(_Handle) ->
    ?nif_stub.

%%
%% Exported: avail_
%%
-spec avail_(handle()) -> {ok,bytes()} | {error, alsa_reason()}.

avail_(_Handle) ->
    ?nif_stub.

%%
%% Exported: state_
%%
-spec state_(handle()) -> {ok,alsa_state()}.

state_(_Handle) ->
    ?nif_stub.

%%
%% Exported: select_
%%
-spec select_(handle()) -> ok | {error, alsa_reason()}.

select_(_Handle) ->
    ?nif_stub.

%%
%% Exported: format_signed
%%

-spec format_is_signed(Format::atom()) -> boolean().

format_is_signed(_Format) ->
    ?nif_stub.

%%
%% Exported: format_unsigned
%%

-spec format_is_unsigned(Format::atom()) -> boolean().

format_is_unsigned(_Format) ->
    ?nif_stub.

%%
%% Exported: format_is_little
%%

-spec format_is_little(Format::atom()) -> boolean().

format_is_little(_Format) ->
    ?nif_stub.

%%
%% Exported: format_is_big
%%

-spec format_is_big(Format::atom()) -> boolean().

format_is_big(_Format) ->
    ?nif_stub.

%%
%% Exported: format_width
%%

-spec format_width(Format::atom()) -> integer().

format_width(_Format) ->
    ?nif_stub.

%%
%% Exported: format_physical_width
%%

-spec format_physical_width(Format::atom()) -> integer().

format_physical_width(_Format) ->
    ?nif_stub.

%%
%% Exported: format_size
%%

-spec format_size(Format::atom(), NumSamples::integer()) -> integer().

format_size(_Format, _NumSamples) ->
    ?nif_stub.

%%
%% Exported: format_silence
%%

-spec format_silence(Format::atom()) -> binary().

format_silence(_Format) ->
    ?nif_stub.

%%
%% Exported: make_silence
%%

-spec make_silence(Format::atom(), Channels::integer(), Samples::integer()) ->
	  binary().

make_silence(_Format, _Channels, _Samples) ->
    ?nif_stub.
