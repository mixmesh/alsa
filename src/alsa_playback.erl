-module(alsa_playback).
-export([file/1, file/2]).
-export([open/1]).
-export([fd/2]).
-export([read_header/1, read_header/2]).

-include("../include/alsa.hrl").
-include("../include/alsa_log.hrl").

-define(dbg(F,A), ok).
%%-define(dbg(F,A), io:format((F),(A))).

-define(DEFAULT_DEVICE,         "default").
-define(DEFAULT_CHANNELS,       2).
-define(DEFAULT_RATE,           48000).
-define(DEFAULT_LATENCY_MS,     20).
-define(DEFAULT_BUFFER_PERIODS, 3).
-define(DEFAULT_FORMAT,         s16_le).

-type playback_options() ::
	# { device => string(),
	    format => alsa:format(),
	    latency => non_neg_integer(),  %% ms
	    channels => non_neg_integer(),  %% 1..N
	    rate =>  non_neg_integer(),  %% 1..N
	    pan => float(),  %% 0..1
	    buffer_periods => non_neg_integer(),
	    buffer_periods_delta => integer()
	  }.
%% mono_to_stereo parameter
-define(DEFAULT_PAN, 0.5).  %%  %% pan 0.0=left .. 1.0 = right

-spec file(Filename::string()) -> ok.
file(FilePath) ->
    file(FilePath, #{}).

-spec file(Filename::string(), Options::playback_options()) -> ok.
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
    DstChannels0 = maps:get(channels, Options0, SrcChannels),
    SrcFormat   = maps:get(format, Options, ?DEFAULT_FORMAT),
    _DstFormat   = maps:get(format, Options0, SrcFormat),
    {ok,Handle,Params} = open(Options#{ channels => DstChannels0 }),
    #{ format := DstFormat,
       channels := DstChannels,
       period_size := PeriodSize,
       buffer_size := BufferSize1,
       pan := Pan
     } = Params,
    Transform =
	if
	    SrcChannels =:= 1, DstChannels =:= 2 ->
		?dbg("mono_to_stereo pan=~p\n", [Pan]),
		fun (X) -> alsa_util:mono_to_stereo(s16_le, X, Pan) end;
	   SrcChannels =:= 2, DstChannels =:= 1 ->
		?dbg("stereo_to_mono\n", []),
		fun (X) -> alsa_util:stereo_to_mono(s16_le, X) end;
	   true ->
		fun (X) -> X end
	   %% fixme: transform SrcFormat => DstFormat 
	   %% fixme: transform SrcRate   => ActualRate
	end,
    %% generate silence period for all frames
    Silence = alsa:make_silence(DstFormat, DstChannels, PeriodSize),

    PeriodBytes = alsa:format_size(DstFormat, PeriodSize*DstChannels),
    %% number of input bytes
    Len = case maps:get(data_length, Options, undefined) of
	      undefined ->
		  undefined;
	      DataLen -> 
		  DataLen
	  end,
    %% read one period
    %% io:format("~w: read0 size=~w\n",[Len,PeriodBytes]),
    case file:read(Fd, PeriodBytes) of
        {ok, Bin} ->
	    Bin1 = Transform(Bin),
	    N = trunc(BufferSize1/PeriodSize),
	    lists:foreach(
	      fun(_) ->
		      alsa:write(Handle, Silence)
	      end, lists:seq(1, N-1)),
	    Len1 = rsub(Len, byte_size(Bin1)),
	    playback(Handle, Fd, Len1, Bin1, PeriodBytes, Transform);
	Error ->
	    alsa:close(Handle),
	    Error
    end.

%% check for wav and au file headers
read_header(Fd) ->
    read_header(Fd, #{}).
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

rsub(undefined, _) -> undefined;
rsub(Len, N) -> Len - N.

rlen(undefined, Period) -> Period;
rlen(Len, Period) when Len >= Period -> Period;
rlen(Len, _Period) ->  Len.
     
%% return eof if len=undefined  and write is ok
%% return ok  if len is defined and write is ok
%% return error otherwise
playback(Handle, _Fd, 0, Block, _PeriodBytes, _Transform) ->
    Rep = write(Handle, Block),
    alsa:drain(Handle),
    alsa:close(Handle),
    case Rep of
	{ok,_} -> ok;
	Error -> Error
    end;
playback(Handle, Fd, Len, Block, Period, Transform) ->
    Size = rlen(Len, Period),
    %% Read next output block
    case file:read(Fd, Size) of
        {ok, Bin} ->
	    %%io:format("~w: read ~w out of ~w\n", [Len,byte_size(Bin),Size]),
	    Bin1 = Transform(Bin),
	    case write(Handle, Block) of
		{ok,_N} ->
		    Len1 = rsub(Len, byte_size(Bin)),
		    %%io:format("remain: ~w : wrote ~w\n", [Len1, N]),
                    playback(Handle, Fd, Len1, Bin1, Period, Transform);
		{error, Reason} ->
		    alsa:close(Handle),
                    {error, alsa:strerror(Reason)}
            end;
	eof ->
	    %%io:format("~w: read EOF try to read ~w\n", [Len,Size]),
	    case write(Handle, Block) of
		{ok,_} ->
		    alsa:drain(Handle),
		    alsa:close(Handle),
		    eof;
		{error, Reason} ->
		    alsa:close(Handle),
                    {error, alsa:strerror(Reason)}
            end;
	{error,Reason} ->
	    alsa:drain(Handle),
	    alsa:close(Handle),
	    {error, Reason}
    end.
	    

%% write one block, retart once? on recover?
write(Handle, Data) ->
    write_(Handle, Data, true).

write_(Handle, Data, Retry) ->
    case alsa:write(Handle, Data) of
	{ok, N} when N =:= byte_size(Data) ->
	    ?dbg("wrote ~w bytes\n", [N]),
	    {ok,N};
	{ok, N} when is_integer(N) ->
	    ?dbg("error wrote short ~w out of ~w bytes\n",
		 [N, byte_size(Data)]),
	    {error, "write failed"};
	{ok, underrun} when Retry ->
	    ?info("Recovered from underrun: retry\n"),
	    write_(Handle, Data, false);
	{ok, underrun} ->
	    {error, underrun};
	{ok, suspend_event} when Retry ->
	    ?info("Recovered from suspend event: retry\n"),
	    write_(Handle, Data, false);
	{ok, suspend_event} ->
	    {error, suspend_event};
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
    BufferPeriods = maps:get(buffer_periods, Options, ?DEFAULT_BUFFER_PERIODS) +
	maps:get(buffer_periods_delta, Options, 0),
    Params0 = [{channels, Channels0},
	       {rate, Rate0},
	       {format, Format0},
	       {period_size, PeriodSize0}],
    ?dbg("request params: ~w\n", [Params0]),
    {ok, H} = alsa:open_(Device, playback),
    {ok,Range} = alsa:get_hw_params_range(H),
    ?dbg("range: ~w\n", [Range]),

    %% fixme: check non-matching format and rate and resample!
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
    BufferSize = max(BufferPeriods*PeriodSize1, PeriodSize1),
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
    {ok, _SwParams1} = alsa:set_sw_params_(H,[{start_threshold,StartThreshold}]),
    ?dbg("sw: ~w\n", [_SwParams1]),
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

