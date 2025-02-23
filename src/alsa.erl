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
	 pause_/2,
	 resume_/1,
	 avail_/1,
	 state_/1,
	 strerror/1,
	 hw_params/0,
	 get_hw_params/1, get_hw_params_range/1,
	 get_hw_params_/2, get_hw_params_/3,
	 set_hw_params_/2,
	 sw_params/0,
	 get_sw_params/1, get_sw_params_/2,
	 set_sw_params_/2, 
	 select_/1,
	 card_info/1, card_info/2,
	 card_next/1
	 ]).
-export([
	 format_is_signed/1,
	 format_is_unsigned/1,
	 format_is_little/1,
	 format_is_big/1,
	 format_is_linear/1,
	 format_is_float/1,
	 format_width/1,
	 format_physical_width/1,
	 format_size/2,
	 format_silence/1,
	 make_silence/3,
	 bytes_to_frames/2
	]).
-export([formats/0]).          % declare atoms and formats available
-export([preloaded_atoms_/0]). % internal
-export([display_cards/0]).    % show card information
-export([list_cards/0]).       % collect card information for all cards


-include("../include/alsa.hrl").

-define(dbg(F,A), ok).
%%-define(dbg(F,A), io:format((F),(A))).


-type unsigned() :: non_neg_integer().
-type handle() :: reference().
-type pcm_name() :: string().
-type format() :: 
	s8 |                    %% :: [-16#80, 16#80]
	u8 |                    %% :: [16#0,   16#FF]
	s16 | s16_le | s16_be |       %% :: [-16#8000, 16#8000]      
	u16 | u16_le | u16_be |       %% :: [16#0000, 16#FFFF]
	s24 | s24_le | s24_be |       %% :: [-16#8000, 16#8000]      
	u24 | u24_le | u24_be |       %% :: [16#000000, 16#FFFFFF]
	s32 | s32_le | s32_be |       %% :: [-16#80000000, 16#80000000]      
	u32 | u32_le | u32_be |       %% :: [16#80000000, 16#FFFFFFFF]      
	float | float_le | float_be |   %% :: [-1.0, 1.0]
	float64 | float64_le | float64_be |  %% :: [-1.0, 1.0]
	iec958_subframe | iec958_subframe_le | iec958_subframe_be |
	mu_law | a_law |        %% :: [-16#8000, 16#8000]
	ima_adpcm | 
	g723_24 | g723_40 |
	dsd_u8 | dsd_u16_le | dsd_u32_le |
	dsd_u16_be | dsd_u32_be |
	mpeg | gsm |
	s20 | s20_le | s20_be | 
	us20 | u20_le | u20_be |
	s24_3le | s24_3be | u24_3le | u24_3be |
	s20_3le | s20_3be | u20_3le | u20_3be |
	s18_3le | s18_3be | u18_3le | u18_3be |
	g723_24_1b | g723_40_1b |
	integer(). % SND_PCM_FORMAT_... in include/alsa.hrl
-type hw_get_param_key() ::
	is_double | is_half_duplex |
	can_pause | can_resume | can_sync_start |
	can_disable_period_wakeup | fifo_size.	
-type hw_param_key() ::
	hw_get_param_key() |
	format |
	rate | rate_min | rate_max |
	channels | channels_min | channels_max |
	period_size | period_size_min | period_size_max |
	buffer_size.

-type hw_param_key_value() ::
	{format, format()} |
	{rate, unsigned()} |
	{rate_min, unsigned()} |
	{rate_max, unsigned()} |
	{channels,unsigned()} |
	{channels_min,unsigned()} |
	{channels_max,unsigned()} |
	{period_size,unsigned()} |
	{period_size_min,unsigned()} |
	{period_size_max,unsigned()} |
	{buffer_size,unsigned()} |
	%% get-hw
	{is_double,boolean()} |
	{is_half_duplex,boolean()} |
	{can_pause,boolean()} |
	{can_resume,boolean()} |
	{can_sync_start,boolean()} |
	{can_disable_period_wakeup,boolean()} |
	{fifo_size,unsigned()}.

-type hw_params() :: [hw_param_key_value()].

-type sw_param_key() :: 
	start_threshold |
	avail_min.

-type sw_param_key_value() ::
	{start_threshold, unsigned()} |
	{avail_min, unsigned()}.
-type sw_params() :: [sw_param_key_value()].

-type info_key() ::  id | driver | name | longname | mixername | components.
-type info_key_value() ::
	{id, string()} |
	{driver, string()} |
	{name, string()} |
	{longname, string()} |
	{mixername, string()} |
	{components, string()}.


-type alsa_reason() :: integer().
-type bad_param_reason() :: {bad_param, atom(), integer(), alsa_reason()}.
-type frames() :: integer().
-type samples() :: binary().

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
     any, current,

     %% open mode
     playback, capture,
     
     %% high level errors
     underrun, overrun, suspend_event, would_block, system_call,
     %% posix errors used as atoms
     eagain, eintr, estrpipe, epipe,
     %% application errors
     no_such_handle, bad_param, select_already,   

     %% state atoms - mostly debugging
     open, setup, prepared, running, xrun, draining, paused,
     suspended, disconnected, private1
     
    ].

%% All formats must be present here, nif will assume all atoms are defined
formats() ->
    [
     s8, u8,
     s16, s16_le, s16_be, u16, u16_le, u16_be,
     s24, s24_le, s24_be, u24, u24_le, u24_be,
     s32, s32_le, s32_be, u32, u32_le, u32_be,
     float, float_le, float_be,
     float64, float64_le, float64_be,
     iec958_subframe,
     iec958_subframe_le,
     iec958_subframe_be,
     mu_law, a_law,
     ima_adpcm,
     g723_24, g723_40,
     dsd_u8, dsd_u16_le, dsd_u32_le,
     dsd_u16_be, dsd_u32_be,
     mpeg, gsm,
     s20, s20_le, s20_be, u20, u20_le, u20_be,
     %% special:
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
%% Exported: hw_params
%%
-spec hw_params() -> [hw_param_key()].

hw_params() ->
    [format, channels, rate, period_size, 
     buffer_size,
     is_double, is_half_duplex,
     can_pause, can_resume, can_sync_start,
     can_disable_period_wakeup, fifo_size
    ].

hw_range_params() ->
    [formats,
     channels_min, channels_max, 
     rate_min, rate_max,
     period_size_min, period_size_max,
     is_double, is_half_duplex,
     can_pause, can_resume, can_sync_start,
     can_disable_period_wakeup, fifo_size].


%%
%% Exported: get_hw_params
%%

-spec get_hw_params(handle()) ->
          {ok, hw_params()} | {error, alsa_reason() | no_such_handle}.

get_hw_params(Handle) ->
    get_hw_params_(Handle, hw_params(), current).

-spec get_hw_params_range(handle()) ->
          {ok, hw_params()} | {error, alsa_reason() | no_such_handle}.

get_hw_params_range(Handle) ->
    get_hw_params_(Handle, hw_range_params(), any).

-spec get_hw_params_(handle(), Params::[hw_param_key()]) ->
          {ok, hw_params()} | {error, alsa_reason() | no_such_handle}.
get_hw_params_(_Handle, _Params) ->
    ?nif_stub.

-spec get_hw_params_(handle(), Params::[hw_param_key()], any|current) ->
          {ok, hw_params()} | {error, alsa_reason() | no_such_handle}.
get_hw_params_(_Handle, _Params, _AnyOrCurrent) ->
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
%% Exported: sw_params
%%
-spec sw_params() -> [sw_param_key()].

sw_params() ->
    [start_threshold, avail_min].

%%
%% Exported: get_sw_params
%%
-spec get_sw_params(handle()) ->
          {ok, sw_params()} | {error, alsa_reason() | no_such_handle}.

get_sw_params(Handle) ->
    get_sw_params_(Handle, sw_params()).

%%
%% Exported: get_sw_params
%%

-spec get_sw_params_(handle(), Params::[sw_param_key_value()]) ->
          {ok, sw_params()} | {error, alsa_reason() | no_such_handle}.

get_sw_params_(_Handle, _Params) ->
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

read(Handle, NumFrames) ->
    read(Handle, NumFrames, []).

read(Handle, NumFrames, Acc) ->
    case read_(Handle, NumFrames) of
	{error, eagain} ->
	    ok = select_(Handle),
	    receive
		{select,Handle,undefined,_Ready} ->
		    %% io:format("read continue\n", []),
		    read(Handle, NumFrames, Acc)
	    end;
	{error, epipe} ->
	    case recover_(Handle, epipe) of
		ok ->
		    read(Handle, NumFrames, Acc);
		_ ->
		    {error, underrun}
	    end;
	{error, estrpipe} ->
	    case recover_(Handle, estrpipe) of
		ok ->
		    read(Handle, NumFrames, Acc);
		_ ->
		    {error, suspsend_event}
	    end;
	{error, AlsaError} when is_integer(AlsaError) ->
	    {error, strerror(AlsaError)};
	{ok, {ReadFrames,Samples}} ->
	    %% io:format("read_: frames=~w\n", [ReadFrames]),
	    if NumFrames =:= ReadFrames ->
		    if Acc =:= [] ->
			    {ok, Samples};
		       true ->
			    {ok,iolist_to_binary(lists:reverse([Samples|Acc]))}
		    end;
	       true ->
		    read(Handle, NumFrames-ReadFrames, [Samples|Acc])
	    end
    end.

%%
%% Exported: write return number of frames! and samples read
%%

-spec read_(handle(), frames()) ->
          {ok, {frames(),samples()}} |
	  {ok, overrrun} | 
	  {ok, suspend_event} |
          {error, alsa_reason() | no_such_handle | overrun | suspend_event}.

read_(_Handle, _Frames) ->
    ?nif_stub.

%%
%% Exported: write return number of bytes! written
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
	{ok, Written} ->
	    ?dbg("alsa:write/3, wrote ~w out of ~w\n", [Written, Size]),
	    if Written =:= Size ->
		    {ok, Written+SoFar};
	       Written =:= 0 ->
		    {ok, SoFar};
	       true ->
		    <<_:Written/binary, Bin1/binary>> = Bin,
		    write(Handle, Bin1, SoFar+Written)
	    end;
	{error, eagain} ->
	    ?dbg("alsa:write/3, eagain\n", []),
	    ok = select_(Handle),
	    receive
		{select,Handle,undefined,_Ready} ->
		    ?dbg("write continue: ~w\n", [SoFar]),
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
	    {error, strerror(AlsaError)}
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
		    %% io:format("drain continue\n"),
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
-spec pause_(handle(), Enable::boolean()) -> ok | {error, alsa_reason()}.

pause_(_Handle, _Enable) ->
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
%% Exported: format_is_linear
%%

-spec format_is_linear(Format::atom()) -> boolean().

format_is_linear(_Format) ->
    ?nif_stub.
%%
%% Exported: format_width
%%

-spec format_width(Format::atom()) -> integer().

format_width(_Format) ->
    ?nif_stub.

%%
%% Exported: format_is_float
%%

-spec format_is_float(Format::atom()) -> boolean().

format_is_float(_Format) ->
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
%% Exported: bytes_to_frames
%%

-spec bytes_to_frames(handle(), bytes()) -> frames().

bytes_to_frames(_Handle, _Bytes) ->
    ?nif_stub.

%%
%% Exported: make_silence
%%

-spec make_silence(Format::atom(), Channels::integer(), Samples::integer()) ->
	  binary().

make_silence(_Format, _Channels, _Samples) ->
    ?nif_stub.

%%
%% Exported: card_info
%%
card_info_keys() ->
    [id, driver, name,
     longname, mixername, components
    ].

-spec card_info(Card::integer()) -> [info_key_value()].
card_info(Card) ->
    case card_info(Card, card_info_keys()) of
	{ok, Info} ->
	    Info
    end.

-spec card_info(Card::integer(), Elems::[info_key()]) -> 
	  [info_key_value()].

card_info(_Card, _Elems) ->
    ?nif_stub.

-spec card_next(Card::integer()) -> false | integer().

card_next(_Card) ->
    ?nif_stub.

%% Small util to dump card info

-spec display_cards() -> ok.
display_cards() ->
    fold_cards(
      fun(Card, Acc) ->
	      io:format("Card ~w:~n", [Card]),
	      [io:format("  ~w: ~p~n", [Key, Value]) || 
		  {Key, Value} <- card_info(Card)],
	      display_formats(Card),
	      Acc
      end, ok).

display_formats(Card) when is_integer(Card) ->
    case open_("hw:"++integer_to_list(Card), playback) of
	{ok, Handle} ->
	    {ok, Params} = get_hw_params_range(Handle),
	    close_(Handle),
	    lists:foreach(
	      fun({Key,Value}) ->
		      io:format("  ~w: ~p~n", [Key, Value])
	      end, Params);
	_ ->
	    ok
    end.

-spec list_cards() -> [{Card::integer(), Info::[{Key::atom(),Value::term()}]}].
list_cards() ->
    List = fold_cards(
	     fun(Card, Acc) ->
		     [{Card, [card_info(Card)]} | Acc]
	     end, []),
    lists:reverse(List).

-spec fold_cards(Fun::function(), Acc::term()) -> Acc1::term().
fold_cards(Fun, Acc) ->
    fold_cards(-1, Fun, Acc).

fold_cards(Prev, Fun, Acc) ->
    case card_next(Prev) of
	false ->
	    Acc;
	Card ->
	    fold_cards(Card, Fun, Fun(Card, Acc))
    end.
