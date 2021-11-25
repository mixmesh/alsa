-module(playback).
-export([start/1]).

-define(PERIOD_SIZE, 1024).
-define(BUFFER_MULTIPLICATOR, 8).

-include("../include/alsa.hrl").

start(FilePath) ->
    case file:open(FilePath, [read, raw, binary, read_ahead]) of
        {ok, Fd} ->
            case alsa:open("plughw:0,0", playback) of
                {ok, AlsaHandle, HwParams, SwParams} ->
                    case alsa:set_hw_params(
                           AlsaHandle,
                           #{format => ?SND_PCM_FORMAT_S16_LE,
                             channels => 2,
                             rate => 44100,
                             period_size => ?PERIOD_SIZE,
                             buffer_size =>
                                 ?PERIOD_SIZE * ?BUFFER_MULTIPLICATOR}) of
                        {ok, ActualSwParams} ->
                            case alsa:set_sw_params(
                                   AlsaHandle,
                                   #{start_threshold =>
                                         ?PERIOD_SIZE *
                                         (?BUFFER_MULTIPLICATOR - 1)}) of
                                {ok, ActualSwParams} ->
                                    io:format("~p\n", [HwParams, SwParams]),
                                    playback(AlsaHandle, Fd);
                                {error, Reason} ->
                                    stop(Fd, AlsaHandle),
                                    {error, Reason}
                            end;
                        {error, Reason} ->
                            stop(Fd, AlsaHandle),
                            {error, Reason}
                    end;
                {error, Reason} ->
                    stop(Fd, undefined),
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

stop(Fd, undefined) ->
    file:close(Fd);
stop(Fd, AlsaHandle) ->
    file:close(Fd),
    alsa:close(AlsaHandle).

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
                            file:close(Fd),
                            alsa:close(AlsaHandle),
                            {error, Reason}
                    end
            end;
        eof ->
            file:close(Fd),
            alsa:close(AlsaHandle),
            eof;
        {error, Reason} ->
            file:close(Fd),
            alsa:close(AlsaHandle),
            {error, Reason}
    end.
