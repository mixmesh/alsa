-module(capture).
-export([start/2]).

-include("../include/alsa.hrl").

-define(FORMAT, ?SND_PCM_FORMAT_S16_LE).
-define(RATE_IN_HZ, 48000).
-define(CHANNELS, 2).
-define(PERIOD_SIZE_IN_FRAMES, 960).
-define(SAMPLE_SIZE_IN_BYTES, 2).
-define(PERIOD_SIZE_IN_BYTES,
        (?PERIOD_SIZE_IN_FRAMES * ?CHANNELS * ?SAMPLE_SIZE_IN_BYTES)).
-define(BUFFER_MULTIPLICATOR, 8).

start(FilePath, DurationInSeconds) ->
    case file:open(FilePath, [write, raw, binary]) of
        {ok, Fd} ->
            WantedHwParams =
                #{format => ?FORMAT,
                  channels => ?CHANNELS,
                  rate => ?RATE_IN_HZ,
                  period_size => ?PERIOD_SIZE_IN_FRAMES,
                  buffer_size => ?PERIOD_SIZE_IN_FRAMES * ?BUFFER_MULTIPLICATOR},
            case alsa:open("hw:0,0", capture, WantedHwParams, #{}) of
                {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
                    io:format("Params: ~p\n", [{ActualHwParams, ActualSwParams}]),
                    ProceedUntil = erlang:monotonic_time(second) + DurationInSeconds,
                    capture(AlsaHandle, Fd, ProceedUntil);
                {error, Reason} ->
                    file:close(Fd),
                    {error, alsa:strerror(Reason)}
            end;
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

capture(AlsaHandle, Fd, ProceedUntil) ->
    case ProceedUntil - erlang:monotonic_time(second) of
        TimeLeft when TimeLeft < 0 ->
            alsa:close(AlsaHandle),
            file:close(Fd),
            ok;
        _ ->
            case alsa:read(AlsaHandle, ?PERIOD_SIZE_IN_BYTES) of
                {ok, Bin} when is_binary(Bin) ->
                    case file:write(Fd, Bin) of
                        ok ->
                            capture(AlsaHandle, Fd, ProceedUntil);
                        {error, Reason} ->
                            alsa:close(AlsaHandle),
                            file:close(Fd),
                            {error, file:format_error(Reason)}
                    end;
                {ok, overrun} ->
                    io:format("Recovered from overrun\n"),
                    capture(AlsaHandle, Fd, ProceedUntil);
                {ok, suspend_event} ->
                    io:format("Recovered from suspend event\n"),
                    capture(AlsaHandle, Fd, ProceedUntil);
                {error, Reason} ->
                    alsa:close(AlsaHandle),
                    file:close(Fd),
                    {error, alsa:strerror(Reason)}
            end
    end.
