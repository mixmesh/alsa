-module(alsa_capture).
-export([file/1, file/2]).

-include("../include/alsa.hrl").

-define(DEFAULT_FORMAT, s16_le).
-define(DEFAULT_RATE_IN_HZ, 48000).
-define(DEFAULT_CHANNELS, 2).
-define(PERIOD_SIZE_IN_FRAMES, 960).
-define(BUFFER_PERIODS, 8).
-define(DEFAULT_DURATION, 5). %% in seconds
-define(DEFAULT_DEVICE, "hw:0,0").

file(FilePath) ->
    file_(FilePath, #{ duration => ?DEFAULT_DURATION}).

file(FilePath, Duration) ->
    file_(FilePath, #{ duration => Duration}).

file_(FilePath, Params) ->
    DurationInSeconds =
	maps:get(duration, Params, ?DEFAULT_DURATION),
    PeriodSizeInFrames = 
	maps:get(period_size, Params, ?PERIOD_SIZE_IN_FRAMES),
    NumBufferPeriods =
	maps:get(buffer_periods, Params, ?BUFFER_PERIODS),
    BufferSizeInFrames = PeriodSizeInFrames * NumBufferPeriods,
    Format = maps:get(format, Params, ?DEFAULT_FORMAT),
    Channels = maps:get(channels, Params, ?DEFAULT_CHANNELS),
    Rate = maps:get(rate, Params, ?DEFAULT_RATE_IN_HZ),
    Device = maps:get(device, Params, ?DEFAULT_DEVICE),
    case file:open(FilePath, [write, raw, binary]) of
        {ok, Fd} ->
            WantedHwParams =
                #{format => Format,
                  channels => Channels,
                  rate => Rate,
                  period_size => PeriodSizeInFrames,
                  buffer_size => BufferSizeInFrames},
            WantedSwParams =
                #{start_threshold => PeriodSizeInFrames },
            case alsa:open(Device, capture, WantedHwParams, WantedSwParams) of
                {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
                    io:format("Params: ~p\n", [{ActualHwParams, ActualSwParams}]),
		    Format1 = maps:get(format, ActualHwParams),
		    Channels1 = maps:get(channels, ActualHwParams),
		    Rate1 = maps:get(rate, ActualHwParams),
		    PeriodSizeInFrames1 = maps:get(period_size, ActualHwParams),		    
                    ProceedUntil = erlang:monotonic_time(second) + 
			DurationInSeconds,
		    %% PeriodSizeInBytes = PeriodSizeInFrames1*Channels1,
		    %% Size1 = alsa:format_size(Format1, Channels1),
		    %% TotalLen = trunc(Rate1*Size1*DurationInSeconds),
		    TotalLen = 0,
		    ok = alsa_wav:write_header(Fd, TotalLen, 
					       #{ format => Format1,
						  channels => Channels1,
						  rate => Rate1 }),
                    capture(AlsaHandle, Fd, PeriodSizeInFrames1, ProceedUntil);
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
	    alsa:close(Handle),
            file:close(Fd),
            ok;
        _ ->
            case alsa:read(Handle, PeriodSizeInFrames) of
                {ok, Bin} when is_binary(Bin) ->
                    case file:write(Fd, Bin) of
                        ok ->
                            capture(Handle,Fd,PeriodSizeInFrames,ProceedUntil);
                        {error, Reason} ->
			    alsa:close(Handle),
                            file:close(Fd),
                            {error, file:format_error(Reason)}
                    end;
                {ok, overrun} ->
                    io:format("Recovered from overrun\n"),
                    capture(Handle, Fd, PeriodSizeInFrames,  ProceedUntil);
                {ok, suspend_event} ->
                    io:format("Recovered from suspend event\n"),
                    capture(Handle, Fd, PeriodSizeInFrames, ProceedUntil);
                {error, Reason} ->
		    alsa:close(Handle),
                    file:close(Fd),
                    {error, alsa:strerror(Reason)}
            end
    end.
