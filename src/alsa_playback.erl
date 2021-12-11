-module(alsa_playback).
-export([file/1, file/2]).
-export([open/1]).
-export([fd/2]).

-include("../include/alsa.hrl").
-include("../include/alsa_log.hrl").

-define(DEFAULT_DEVICE,         "default").
-define(DEFAULT_CHANNELS,       2).
-define(DEFAULT_RATE,           48000).
-define(DEFAULT_LATENCY_MS,     20).
-define(DEFAULT_BUFFER_PERIODS, 3).
-define(DEFAULT_FORMAT,         s16_le).

%% mono_to_stereo parameter
-define(DEFAULT_PAN, 0.5).  %%  %% pan 0.0=left .. 1.0 = right

file(FilePath) ->
    file(FilePath, #{}).

file(FilePath, Options) when is_map(Options) ->
    case file:open(FilePath, [read, raw, binary, read_ahead]) of
        {ok, Fd} ->
	    try fd(Fd, Options) of
		Result -> Result
	    after
		file:close(Fd)
	    end;
	{error, Reason} ->
	    {error, file:format_error(Reason)}
    end.

%% Play samples from file use [ram] option to send non-file samples...
fd(Fd, Options) when is_map(Options) ->
    fd_(Fd, Options);
fd(Fd, Options) when is_list(Options) ->
    fd_(Fd, maps:from_list(Options)).

fd_(Fd, Options0) ->
    %% Check for wav header and let the wav header hint what
    %% parameters to use.
    Options = read_header(Fd, Options0),
    SrcChannels = maps:get(channels, Options, ?DEFAULT_CHANNELS),
    DstChannels = maps:get(channels, Options0, SrcChannels),
    SrcFormat   = maps:get(format, Options, ?DEFAULT_FORMAT),
    _DstFormat   = maps:get(format, Options0, SrcFormat),
    {ok,Handle,Params} = open(Options#{ channels => DstChannels }),
    #{ format := ActualFormat,
       channels := ActualChannels,
       period_size := PeriodSize1,
       buffer_size := BufferSize1,
       pan := Pan
     } = Params,
    Transform =
	if
	    SrcChannels =:= 1, ActualChannels =:= 2 ->
		?dbg("mono_to_stereo pan=~p\n", [Pan]),
		fun (X) -> alsa_util:mono_to_stereo(s16_le, X, Pan) end;
	   SrcChannels =:= 2, ActualChannels =:= 1 ->
		?dbg("stereo_to_mono\n", []),
		fun (X) -> alsa_util:stereo_to_mono(s16_le, X) end;
	   true ->
		fun (X) -> X end
	   %% fixme: transform SrcFormat => ActualFormat 
	   %% fixme: transform SrcRate   => ActualRate
	end,
    %% generate silence period for all frames
    Silence = alsa:make_silence(ActualFormat, ActualChannels, PeriodSize1),
    %% start by fill with silence
    N = trunc(BufferSize1/PeriodSize1),
    lists:foreach(
      fun(_) ->
	      alsa:write(Handle, Silence)
      end, lists:seq(1, N-1)),
    PeriodSizeInBytes = alsa:format_size(ActualFormat,
					 PeriodSize1*ActualChannels),
    playback(Handle, Fd, PeriodSizeInBytes, Transform).

%% check for wav and au file headers
read_header(Fd, Options) ->
    case alsa_wav:read_header(Fd) of
	{ok, Params} ->
	    ?dbg("wav header: ~w\n", [Params]),
	    maps:merge(Options, Params);
	_ ->
	    file:position(Fd, 0),
	    case alsa_au:read_header(Fd) of
		{ok, Params} ->
		    ?dbg("au header: ~w\n", [Params]),
		    maps:merge(Options, Params);
		_ ->
		    Options
	    end
    end.


playback(Handle, Fd, PeriodSizeInBytes, Transform) ->
    %% fixme: start reading samples before generating silence, the first time!
    %% I suspect the glitches come because of delay in reading from file
    case file:read(Fd, PeriodSizeInBytes) of
        {ok, Bin} ->
	    Bin1 = Transform(Bin),
	    %% io:format("Write: ~p\n",  [Bin1]),
            case alsa:write(Handle, Bin1) of
                {ok, N} when N =:= byte_size(Bin1) ->
                    playback(Handle, Fd, PeriodSizeInBytes, Transform);
                {ok, underrun} ->
                    ?info("Recovered from underrun\n"),
                    playback(Handle, Fd, PeriodSizeInBytes, Transform);
                {ok, suspend_event} ->
                    ?info("Recovered from suspend event\n"),
                    playback(Handle, Fd, PeriodSizeInBytes, Transform);
                {error, Reason} ->
		    alsa:close(Handle),
                    {error, alsa:strerror(Reason)}
            end;
        eof ->
	    alsa:drain(Handle),
	    alsa:close(Handle),
            eof;
        {error, Reason} ->
	    alsa:close(Handle),
            {error, alsa:strerror(Reason)}
    end.

%% open and setup "realistic"? parameters
open(Options) ->
    ?dbg("open playback options ~p\n", [Options]),
    Device    = maps:get(device, Options, ?DEFAULT_DEVICE),
    Format0   = maps:get(format, Options, ?DEFAULT_FORMAT),
    Latency   = maps:get(latency, Options, ?DEFAULT_LATENCY_MS),
    Channels0 = maps:get(channels, Options, ?DEFAULT_CHANNELS),
    Rate0     = maps:get(rate, Options, ?DEFAULT_RATE),
    PeriodSize0 = trunc(Rate0*(Latency/1000)),
    Pan       = maps:get(pan, Options, ?DEFAULT_PAN),
    Params0 = [{channels, Channels0},
	       {rate, Rate0},
	       {format, Format0},
	       {period_size, PeriodSize0}],
    ?dbg("request params: ~w\n", [Params0]),
    {ok, H} = alsa:open_(Device, playback),
    {ok,Range} = alsa:get_hw_params_range(H),    
    ?dbg("range: ~w\n", [Range]),

    {ok,Params1} = alsa_util:adjust_params(Params0, Range),
    ?dbg("adjusted params: ~w\n", [Params1]),
    Channels1 = proplists:get_value(channels, Params1),
    Rate1 = proplists:get_value(rate, Params1),

    %% recalculate PeriodSize based on adjusted rate
    PeriodSize1_ = trunc(Rate1*(Latency/1000)),
    %% then adjust it again
    {ok,[{period_size,PeriodSize1}]} =
	alsa_util:adjust_params([{period_size,PeriodSize1_}], Range),
    Format1 = proplists:get_value(format, Params1),
    BufferSize = ?DEFAULT_BUFFER_PERIODS*PeriodSize1,
    HwParams0 = [
		 {channels,Channels1},
		 {rate,Rate1-1},  %% make final rate look better!
		 {format,Format1},
		 {period_size,PeriodSize1},
		 {buffer_size, BufferSize}
		],
    ?dbg("request hw params: ~w\n", [HwParams0]),
    {ok,HwParams1} = alsa:set_hw_params_(H, HwParams0),
    ?dbg("hw: got ~w\n", [HwParams1]),
    Rate2 = proplists:get_value(rate, HwParams1),
    Format2 = proplists:get_value(format, HwParams1),
    Channels2 = proplists:get_value(channels, HwParams1),
    PeriodSize2 = proplists:get_value(period_size, HwParams1),
    BufferSize2 = proplists:get_value(buffer_size, HwParams1),
    StartThreshold = PeriodSize2,
    {ok, SwParams1} = alsa:set_sw_params_(H,[{start_threshold,StartThreshold}]),
    ?dbg("sw: ~w\n", [SwParams1]),
    {ok, H, #{device => Device,
	      latency => Latency,  %% ms
	      channels => Channels2,
	      rate => Rate2,
	      format => Format2,
	      pan => Pan,
	      period_size => PeriodSize2,
	      buffer_size => BufferSize2,
	      start_threshold => StartThreshold
	     }}.

