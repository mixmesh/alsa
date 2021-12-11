-module(alsa_capture).
-export([file/1, file/2, file/3]).
-export([open/1]).

-include("../include/alsa.hrl").
-include("../include/alsa_log.hrl").

-define(DEFAULT_DEVICE,         "default").
-define(DEFAULT_CHANNELS,       1).
-define(DEFAULT_RATE,           48000).
-define(DEFAULT_LATENCY_MS,     20).
-define(DEFAULT_BUFFER_PERIODS, 3).
-define(DEFAULT_FORMAT,         s16_le).

-define(DEFAULT_DURATION, 5). %% in seconds

file(FilePath) ->
    file_(FilePath, #{duration=>?DEFAULT_DURATION}).

file(FilePath, Duration) when is_number(Duration) ->
    file_(FilePath, #{duration=>Duration});
file(FilePath, Options) when is_list(Options) ->
    file_(FilePath, maps:from_list(Options));
file(FilePath, Options) when is_map(Options) ->
    file_(FilePath, Options).

file(FilePath, Duration, Options) when is_number(Duration), is_map(Options) ->
    file_(FilePath, Options#{duration=>Duration});
file(FilePath, Duration, Options) when is_number(Duration), is_map(Options) ->
    file_(FilePath, Options#{duration=>Duration}).

file_(FilePath, Options) when is_map(Options) ->
    ?dbg("alsa_capture: options = ~p\n", [Options]),
    DurationInSeconds =	maps:get(duration,Options,?DEFAULT_DURATION),
    case file:open(FilePath, [write, raw, binary]) of
        {ok, Fd} ->
	    {ok, Handle, Params} = open(Options),
	    ProceedUntil = erlang:monotonic_time(second) + DurationInSeconds,
	    #{ format := Format,
	       channels := Channels,
	       rate := Rate,
	       buffer_size := BufferSize
	     } = Params,
	    ok = alsa_wav:write_header(Fd,
				       #{format=>Format,
					 channels=>Channels,
					 rate=>Rate}),
	    try capture(Handle, Fd, BufferSize, ProceedUntil) of
		ok ->
		    alsa_wav:poke_file_length(Fd);
		Error ->
		    Error
	    after
		alsa:close(Handle),
		file:close(Fd)
	    end;
	{error, Reason} ->
	    {error, alsa:strerror(Reason)}
    end.

capture(Handle, Fd, BufferSize, ProceedUntil) ->
    case ProceedUntil - erlang:monotonic_time(second) of
        TimeLeft when TimeLeft < 0 ->
            ok;
        _ ->
	    capture_(Handle, Fd, BufferSize, ProceedUntil)
    end.

capture_(H, Fd, NumFrames, ProceedUntil) ->
    case alsa:read_(H, NumFrames) of
	{ok, {_ReadFrames, Samples}} ->
	    case file:write(Fd, Samples) of
		ok ->
		    alsa_util:async_wait_ready(H),
		    capture(H,Fd,NumFrames,ProceedUntil);
		{error, Reason} ->
		    {error, file:format_error(Reason)}
	    end;
	{error, eagain} ->
	    ?warning("error: eagain\n"),
	    alsa_util:async_wait_ready(H),
	    capture(H,Fd,NumFrames,ProceedUntil);
	Err = {error, Reason} ->
	    ?error("Error ~p\n", [alsa:strerror(Reason)]),
	    alsa:close(H),
	    Err
    end.
			

%% open alsa for capture
open(Options) ->
    Device    = maps:get(device, Options, ?DEFAULT_DEVICE),
    Format0   = maps:get(format, Options, ?DEFAULT_FORMAT),
    Latency   = maps:get(latency, Options, ?DEFAULT_LATENCY_MS),
    Channels0 = maps:get(channels, Options, ?DEFAULT_CHANNELS),
    Rate0     = maps:get(rate, Options, ?DEFAULT_RATE),
    PeriodSize0 = trunc(Rate0*(Latency/1000)),
    Params0 = [{channels, Channels0},
	       {rate, Rate0},
	       {format, Format0},
	       {period_size, PeriodSize0}],
    ?dbg("request params: ~w\n", [Params0]),
    {ok, H} = alsa:open_(Device, capture),
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
    Channels2 = proplists:get_value(channels, HwParams1),
    Rate2 = proplists:get_value(rate, HwParams1),
    Format2 = proplists:get_value(format, HwParams1),
    PeriodSize2 = proplists:get_value(period_size, HwParams1),
    BufferSize2 = proplists:get_value(buffer_size, HwParams1),
    AvailMin = PeriodSize2,
    {ok, SwParams1} = alsa:set_sw_params_(H, 
					  [
					   {start_threshold,PeriodSize2},
					   {avail_min, AvailMin}
					  ]),
    ?dbg("sw: ~w\n", [SwParams1]),
    {ok, H, #{device => Device,
	      latency => Latency,  %% ms
	      channels=>Channels2,
	      rate=>Rate2,
	      format=>Format2,
	      period_size=>PeriodSize2,
	      buffer_size=>BufferSize2,
	      avail_min=>AvailMin}}.
