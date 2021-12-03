-module(alsa_playback).
-export([file/1, file/2]).
-export([fd/2]).

-include("../include/alsa.hrl").

-define(DEFAULT_FORMAT, s16_le). %% ?SND_PCM_FORMAT_S16_LE).
-define(DEFAULT_RATE_IN_HZ, 48000).
-define(DEFAULT_CHANNELS, 2).
-define(PERIOD_SIZE_IN_FRAMES, 4800).  %% 100ms
-define(BUFFER_PERIODS, 8).
-define(DEFAULT_DEVICE, "hw:0,0").

file(FilePath) ->
    file(FilePath, #{}).

file(FilePath, Params) ->
    case file:open(FilePath, [read, raw, binary, read_ahead]) of
        {ok, Fd} ->
	    try fd(Fd, Params) of
		Result -> Result
	    after
		file:close(Fd)
	    end;
	{error, Reason} ->
	    {error, file:format_error(Reason)}
    end.

%% Play samples from file use [ram] option to send non-file samples...
fd(Fd, Params0) ->
    PeriodSizeInFrames = 
	maps:get(period_size, Params0, ?PERIOD_SIZE_IN_FRAMES),
    NumBufferPeriods =
	maps:get(buffer_periods, Params0, ?BUFFER_PERIODS),
    BufferSizeInFrames = PeriodSizeInFrames * NumBufferPeriods,
    Params = case alsa_wav:read_header(Fd) of
		 {ok, Header} ->
		     io:format("wav header: ~w\n", [Header]),
		     maps:merge(Params0, Header);
		 _ -> 
		     %% probably raw file
		     file:position(Fd, 0),
		     Params0
	     end,
    Format = maps:get(format, Params, ?DEFAULT_FORMAT),
    Channels0 = maps:get(channels, Params, ?DEFAULT_CHANNELS),
    Channels = if Channels0 =:= 1 -> 2;
		  true -> Channels0
	       end,
    Rate = maps:get(rate, Params, ?DEFAULT_RATE_IN_HZ),
    Device = maps:get(device, Params, ?DEFAULT_DEVICE),
    Pan = maps:get(pan, Params, 0.0), %% pan 0.0=left .. 1.0 = right
    WantedHwParams =
	#{format => Format,
	  channels => Channels,
	  rate => Rate,
	  period_size => PeriodSizeInFrames,
	  buffer_size => BufferSizeInFrames
	 },
    WantedSwParams =
	#{start_threshold =>
	      PeriodSizeInFrames * (NumBufferPeriods-1)},
    case alsa:open(Device, playback, WantedHwParams,
		   WantedSwParams) of
	{ok, Handle, ActualHwParams, ActualSwParams} ->
	    io:format("Params: ~p\n", [{ActualHwParams, ActualSwParams}]),
	    Format1 = maps:get(format, ActualHwParams),
	    PeriodSizeInFrames1 = maps:get(period_size, ActualHwParams),
	    Channels1 = maps:get(channels, ActualHwParams),
	    Size1 = alsa:format_size(Format1, Channels1),
	    PeriodSizeInBytes = PeriodSizeInFrames1*Size1,
	    Transform =
		if Channels0 =:= 1, Channels1 =:= 2 ->
			io:format("mono_to_stereo pan=~p\n", [Pan]),
			fun (X) -> mono_to_stereo(s16_le, X, Pan) end;
		   Channels0 =:= 2, Channels1 =:= 1 ->
			fun (X) -> stereo_to_mono(s16_le, X) end;
		   true ->
			fun (X) -> X end
		end,
	    %% generate silence period for all frames
	    Silence = alsa:make_silence(Format1, Channels1, 
					PeriodSizeInFrames1),
	    %% start by fill with silence
	    lists:foreach(
	      fun(_) ->
		      alsa:write(Handle, Silence)
	      end, lists:seq(1, NumBufferPeriods-1)),
	    
	    playback(Handle, Fd, PeriodSizeInBytes, Transform);
	{error, Reason} ->
	    {error, alsa:strerror(Reason)}
    end.

playback(Handle, Fd, PeriodSizeInBytes, Transform) ->
    case file:read(Fd, PeriodSizeInBytes) of
        {ok, Bin} ->
	    Bin1 = Transform(Bin),
	    %% io:format("Write: ~p\n",  [Bin1]),
            case alsa:write(Handle, Bin1) of
                {ok, N} when N =:= byte_size(Bin1) ->
                    playback(Handle, Fd, PeriodSizeInBytes, Transform);
                {ok, underrun} ->
                    io:format("Recovered from underrun\n"),
                    playback(Handle, Fd, PeriodSizeInBytes, Transform);
                {ok, suspend_event} ->
                    io:format("Recovered from suspend event\n"),
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

%% Add more stuff? or use mixer device?
mono_to_stereo(s16_le, Bin,Pan) ->
    << <<(trunc(X*(1.0-Pan))):16/signed-little, 
	 (trunc(X*Pan)):16/signed-little>> ||
	<<X:16/signed-little>> <= Bin >>.

stereo_to_mono(s16_le, Bin) ->
    << <<(max(X1,X2)):16/little-signed>> || 
	<<X1:16/little-signed,X2:16/little-signed>> <= Bin >>.
