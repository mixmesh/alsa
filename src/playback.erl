-module(playback).
-export([start/1]).

-define(PERIOD_SIZE, 960).
-define(BUFFER_MULTIPLICATOR, 8).

-include("../include/alsa.hrl").

start(FilePath) ->
    case file:open(FilePath, [read, raw, binary, read_ahead]) of
        {ok, Fd} ->
            WantedHwParams =
                #{format => ?SND_PCM_FORMAT_S16_LE,
                  channels => 2,
                  rate => 48000,
                  period_size => ?PERIOD_SIZE,
                  buffer_size => ?PERIOD_SIZE * ?BUFFER_MULTIPLICATOR},
            WantedSwParams =
                #{start_threshold => ?PERIOD_SIZE * (?BUFFER_MULTIPLICATOR - 1)},
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
    case file:read(Fd, ?PERIOD_SIZE) of
        {ok, Bin} ->
            case alsa:write(AlsaHandle, Bin) of
                {ok, ?PERIOD_SIZE} ->
                    playback(AlsaHandle, Fd);
                {error, Reason} ->
                    io:format(standard_error, "%s\n", alsa:strerror(underrun)),
                    case alsa:snd_pcm_recover(AlsaHandle, Reason, true) of
                        ok ->
                            playback(AlsaHandle, Fd);
                        {error, Reason} ->
                            alsa:close(AlsaHandle),
                            file:close(Fd),
                            {error, Reason}
                    end
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
