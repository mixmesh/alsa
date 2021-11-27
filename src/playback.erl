-module(playback).
-export([start/1]).

-include("../include/alsa.hrl").

-define(FORMAT, ?SND_PCM_FORMAT_S16_LE).
-define(RATE_IN_HZ, 48000).
-define(CHANNELS, 2).
-define(PERIOD_SIZE_IN_FRAMES, 960).
-define(SAMPLE_SIZE_IN_BYTES, 2).
-define(PERIOD_SIZE_IN_BYTES,
        (?PERIOD_SIZE_IN_FRAMES * ?CHANNELS * ?SAMPLE_SIZE_IN_BYTES)).
-define(BUFFER_MULTIPLICATOR, 8).

start(FilePath) ->
    case file:open(FilePath, [read, raw, binary, read_ahead]) of
        {ok, Fd} ->
            WantedHwParams =
                #{format => ?FORMAT,
                  channels => ?CHANNELS,
                  rate => ?RATE_IN_HZ,
                  period_size => ?PERIOD_SIZE_IN_FRAMES,
                  buffer_size => ?PERIOD_SIZE_IN_FRAMES * ?BUFFER_MULTIPLICATOR},
            WantedSwParams =
                #{start_threshold =>
                      ?PERIOD_SIZE_IN_FRAMES * (?BUFFER_MULTIPLICATOR - 1)},
            case alsa:open("hw:0,0", playback, WantedHwParams,
                           WantedSwParams) of
                {ok, AlsaHandle, ActualHwParams, ActualSwParams} ->
                    io:format("Params: ~p\n", [{ActualHwParams, ActualSwParams}]),
                    playback(AlsaHandle, Fd);
                {error, Reason} ->
                    file:close(Fd),
                    {error, alsa:strerror(Reason)}
            end;
        {error, Reason} ->
            {error, file:format_error(Reason)}
    end.

playback(AlsaHandle, Fd) ->
    case file:read(Fd, ?PERIOD_SIZE_IN_BYTES) of
        {ok, Bin} ->
            case alsa:write(AlsaHandle, Bin, ?PERIOD_SIZE_IN_FRAMES) of
                {ok, ?PERIOD_SIZE_IN_FRAMES} ->
                    playback(AlsaHandle, Fd);
                {ok, underrun} ->
                    io:format("Recovered from underrun\n"),
                    playback(AlsaHandle, Fd);
                {ok, suspend_event} ->
                    io:format("Recovered from suspend event\n"),
                    playback(AlsaHandle, Fd);
                {error, Reason} ->
                    alsa:close(AlsaHandle),
                    file:close(Fd),
                    {error, alsa:strerror(Reason)}
            end;
        eof ->
            alsa:close(AlsaHandle),
            file:close(Fd),
            eof;
        {error, Reason} ->
            alsa:close(AlsaHandle),
            file:close(Fd),
            {error, alsa:strerror(Reason)}
    end.
