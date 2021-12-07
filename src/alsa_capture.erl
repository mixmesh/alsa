-module(alsa_capture).
-export([file/1, file/2, file/3]).

-include("../include/alsa.hrl").

-define(DEFAULT_FORMAT, s16_le).
-define(DEFAULT_RATE, 48000).
-define(DEFAULT_CHANNELS, 2).
-define(PERIOD_SIZE_IN_FRAMES, 4800).  %% 100ms
-define(BUFFER_PERIODS, 8).
-define(DEFAULT_DURATION, 5). %% in seconds
-define(DEFAULT_DEVICE, "hw:0,0").

file(FilePath) ->
    file_(FilePath, [{duration,?DEFAULT_DURATION}]).

file(FilePath, Duration) when is_number(Duration) ->
    file_(FilePath, [{duration,Duration}]);
file(FilePath, Params) when is_list(Params) ->
    file_(FilePath, Params);
file(FilePath, Params) when is_map(Params) ->
    file_(FilePath, maps:to_list(Params)).

file(FilePath, Duration, Params) when is_number(Duration), is_list(Params) ->
    file_(FilePath, [{duration,Duration}|Params]);
file(FilePath, Duration, Params) when is_number(Duration), is_map(Params) ->
    file_(FilePath, [{duration,Duration}|maps:to_list(Params)]).


file_(FilePath, Params) when is_list(Params) ->
    io:format("alsa_capture: file_ params = ~p\n", [Params]),
    DurationInSeconds =	proplists:get_value(duration,Params,?DEFAULT_DURATION),
    PeriodSizeInFrames = 
	proplists:get_value(period_size, Params, ?PERIOD_SIZE_IN_FRAMES),
    NumBufferPeriods =
	proplists:get_value(buffer_periods, Params, ?BUFFER_PERIODS),
    BufferSizeInFrames = PeriodSizeInFrames * NumBufferPeriods,
    Format = proplists:get_value(format, Params, ?DEFAULT_FORMAT),
    Channels = proplists:get_value(channels, Params, ?DEFAULT_CHANNELS),
    Rate = proplists:get_value(rate, Params, ?DEFAULT_RATE),
    Device = proplists:get_value(device, Params, ?DEFAULT_DEVICE),
    case file:open(FilePath, [write, raw, binary]) of
        {ok, Fd} ->
            WantedHwParams = 
		[
		 {format,Format},
		 {channels,Channels},
		 {rate,Rate},
		 {period_size,PeriodSizeInFrames},
		 {buffer_size,BufferSizeInFrames}],
	    io:format("alsa_capture: file_ wanted_hw_params = ~p\n",
		      [WantedHwParams]),
            WantedSwParams =
                [{start_threshold,PeriodSizeInFrames},
		 {avail_min, PeriodSizeInFrames-1}],
            case alsa:open(Device, capture, WantedHwParams, WantedSwParams) of
                {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
		    io:format("alsa_capture: file_ actual_hw_params = ~p\n",
			      [ActualHwParams]),
		    io:format("alsa_capture: file_ actual_sw_params = ~p\n",
			      [ActualSwParams]),
		    Format1 = proplists:get_value(format, ActualHwParams),
		    Channels1 = proplists:get_value(channels, ActualHwParams),
		    Rate1 = proplists:get_value(rate, ActualHwParams),
		    PeriodSizeInFrames1 = proplists:get_value(period_size,
							      ActualHwParams),
                    ProceedUntil = erlang:monotonic_time(second) + 
			DurationInSeconds,
		    %% PeriodSizeInBytes = PeriodSizeInFrames1*Channels1,
		    %% Size1 = alsa:format_size(Format1, Channels1),
		    %% TotalLen = trunc(SampleRate1*Size1*DurationInSeconds),
		    TotalLen = 28,  %% = header with no data
		    ok = alsa_wav:write_header(Fd, TotalLen, 
					       [{format,Format1},
						{channels,Channels1},
						{rate,Rate1}]),
                    try capture(AlsaHandle, Fd, PeriodSizeInFrames1,
				ProceedUntil) of
			ok ->
			    alsa_wav:poke_file_length(Fd);
			Error ->
			    Error
		    after
			alsa:close(AlsaHandle),
			file:close(Fd)
		    end;
                {error, Reason} ->
                    file:close(Fd),
                    {error, alsa:strerror(Reason)}
            end;
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

capture(Handle, Fd, PeriodSizeInFrames, ProceedUntil) ->
    case ProceedUntil - erlang:monotonic_time(second) of
        TimeLeft when TimeLeft < 0 ->
            ok;
        _ ->
            case alsa:read(Handle, PeriodSizeInFrames) of
                {ok, Bin} when is_binary(Bin) ->
                    case file:write(Fd, Bin) of
                        ok ->
                            capture(Handle,Fd,PeriodSizeInFrames,ProceedUntil);
                        {error, Reason} ->
                            {error, file:format_error(Reason)}
                    end;
                {ok, overrun} ->
                    io:format("Recovered from overrun\n"),
                    capture(Handle, Fd, PeriodSizeInFrames,  ProceedUntil);
                {ok, suspend_event} ->
                    io:format("Recovered from suspend event\n"),
                    capture(Handle, Fd, PeriodSizeInFrames, ProceedUntil);
                {error, Reason} ->
                    {error, alsa:strerror(Reason)}
            end
    end.
