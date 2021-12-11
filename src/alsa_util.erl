%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    Collection of useful functions
%%% @end
%%% Created : 10 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_util).

-export([mono_to_stereo/2, mono_to_stereo/3]).
-export([stereo_to_mono/2]).
-export([resample/5]).
-export([reformat/5]).
-export([async_wait_ready/1]).
-export([adjust_params/2]).
-export([adjust_value/3]).
-export([encode_frame/2, decode_frame/2]).
-export([a_law_encode/1, a_law_decode/1]).
-export([mu_law_encode/1, mu_law_decode/1]).
%% testing
-export([test/0]).
-export([test_a_law/0, test_mu_law/0]).
-export([test_float/0, test_float64/0, test_integer/0]).

-include("../include/alsa_log.hrl").

-type int32() :: -16#7fffffff .. 16#7fffffff.
-type float01() :: float().  %% range -1.0 ... 1.0


mono_to_stereo(s16_le, Bin) ->
    mono_to_stereo(s16_le, Bin, 0.5).

mono_to_stereo(s16_le, Bin,Pan) ->
    << <<(trunc(X*(1.0-Pan))):16/signed-little, 
	 (trunc(X*Pan)):16/signed-little>> ||
	<<X:16/signed-little>> <= Bin >>.

stereo_to_mono(s16_le, Bin) ->
    << <<(max(X1,X2)):16/little-signed>> || 
	<<X1:16/little-signed,X2:16/little-signed>> <= Bin >>.

%%
%% simple resample replicate or drop
%%
-spec resample(SrcRate::alsa:unsigned(), DstRate::alsa:unsigned(),
	       Format::alsa:format(), Channels::alsa:unsigned(),
	       Src::binary()) -> binary().
	  
resample(SrcRate, SrcRate, _Format, _Channels, Src) -> 
    Src;
resample(SrcRate, DstRate, Format, Channels, Src) ->
    FrameSize = alsa:format_size(Format, Channels),
    Dx = DstRate/SrcRate,
    resample_(Src, Dx, Dx, FrameSize, []).

resample_(<<>>, _X, _Dx, _FrameSize, Acc) ->
    list_to_binary(lists:reverse(Acc));
resample_(Src, X, Dx, FrameSize, Acc) ->
    <<Frame:FrameSize/binary, Src1/binary>> = Src,
    if X < 1 ->
	    %% skip one frame
	    resample_(X+Dx, Dx, FrameSize, Src1, Acc);
       true ->
	    case trunc(X) of
		1 -> resample_((X-1)+Dx, Dx, FrameSize, Src1, [Frame|Acc]);
		N -> resample_((X-N)+Dx, Dx, FrameSize, Src1, 
			       [lists:duplicate(N,Frame)|Acc])
	    end
    end.

-spec reformat(SrcFormat::alsa:format(), DstFormat::alsa:format(),
	       SrcChannels::alsa:unsigned(), DstChannels::alsa:unsigned(),
	       Src::binary()) -> binary().

%% reformat frame format
reformat(SrcFormat, DstFormat, SrcChannels, DstChannels, Src) ->
    FrameSize = alsa:format_size(SrcFormat, SrcChannels),
    reformat_(Src,FrameSize,SrcFormat,DstFormat,DstChannels,[]).

reformat_(<<>>,_FrameSize,_SrcFormat,_DstFormat,_DstChannels,Acc) ->
    list_to_binary(lists:reverse(Acc));
reformat_(Bin,FrameSize,SrcFormat,DstFormat,DstChannels,Acc) ->
    <<Frame:FrameSize/binary, Bin1/binary>> = Bin,
    DstFrame = reformat_frame(Frame, SrcFormat, DstFormat, DstChannels),
    reformat_(Bin1,FrameSize,SrcFormat,DstFormat,DstChannels,
	      [DstFrame|Acc]).

%% fixme: optimize SrcFormat=DstFormat
reformat_frame(Frame, SrcFormat, DstFormat, DstChannels) ->
    SrcSamples = decode_frame(Frame, SrcFormat),
    DstSamples = map_channels(SrcSamples, DstChannels),
    encode_frame(DstSamples, DstFormat).

map_channels(Xs=[_FC], 1) -> Xs;                               %% mono
map_channels(Xs=[_FL,_FR], 2) -> Xs;                           %% stereo
map_channels(Xs=[_FL,_FR,_LFE], 3) -> Xs;                      %% 2.1
map_channels(Xs=[_FL,_FR,_RR,_RL], 4) -> Xs;                   %% 4.0
map_channels(Xs=[_FL,_FR,_LFE,_RR,_RL], 5) -> Xs;              %% 4.1
map_channels(Xs=[_FL,_FR,_FC,__LFE,_RR,_RL], 6) -> Xs;        %% 5.1
map_channels(Xs=[_FL,_FR,_FC,_LFE,_RL,_RR,_SL,_SR], 8) -> Xs;   %% 7.1

map_channels([FL,FR], 1) ->  [max(FL,FR)];
map_channels([FL,FR,_LFE], 1) ->  [max(FL,FR)];
map_channels([FL,FR,_RR,_RL], 1) ->  [max(FL,FR)];
map_channels([FL,FR,_LFE,_RR,_RL], 1) -> [max(FL,FR)];
map_channels([_FL,_FR,FC,__LFE,_RR,_RL], 1) -> [FC];
map_channels([_FL,_FR,FC,_LFE,_RL,_RR,_SL,_SR], 1) -> [FC];

map_channels([FC], 2) -> X=trunc(FC*0.5), [X,X];            %% mono->stereo
map_channels([FL,FR,_LFE], 2) -> [FR,FL];
map_channels([FL,FR,_FC,__LFE,_RR,_RL], 2) -> [FR,FL];
map_channels([FL,FR,_FC,_LFE,_RL,_RR,_SL,_SR], 2) -> [FR,FL];

map_channels([FC], 3) -> X=trunc(FC*0.5), [X,X,0];
map_channels([FL,FR], 3) -> [FR,FL,0];
map_channels([FL,FR,_FC,LFE,_RR,_RL], 3) -> [FR,FL,LFE];
map_channels([FL,FR,_FC,LFE,_RL,_RR,_SL,_SR], 3) -> [FR,FL,LFE];

map_channels([FC], 4) -> X=trunc(FC*0.5), [X,X,X,X];
map_channels([FL,FR], 4) -> [FR,FL,FL,FR];
map_channels([FL,FR,_FC,_LFE,RR,RL], 4) -> [FR,FL,RR,RL];
map_channels([FL,FR,_FC,_LFE,RL,RR,_SL,_SR], 4) -> [FR,FL,RR,RL];

map_channels([FC], 5) -> X=trunc(FC*0.5), [X,X,0,X,X];
map_channels([FL,FR], 5) -> [FR,FL,0,FL,FR];
map_channels([FL,FR,_FC,LFE,RR,RL], 5) -> [FR,FL,LFE,RR,RL];
map_channels([FL,FR,_FC,LFE,RL,RR,_SL,_SR], 5) -> [FR,FL,LFE,RR,RL];

map_channels([FC], 6) -> X=trunc(FC*0.5), [X,X,X,0,X,X];
map_channels([FL,FR], 6) -> X=max(FL,FR),[FR,FL,X,0,FL,FR];
map_channels([FL,FR,LFE,RR,RL], 6) -> FC=max(FL,FR), [FL,FR,FC,LFE,RR,RL];
map_channels([FL,FR,FC,LFE,RL,RR,_SL,_SR], 6) -> [FL,FR,FC,LFE,RL,RR].



-define(i8(X), ((X) bsl 24)).
-define(i16(X), ((X) bsl 16)).
-define(i24(X), ((X) bsl 8)).
-define(i32(X), ((X))).

-define(u8(X), (((X)-16#80) bsl 24)).
-define(u16(X), (((X)-16#8000) bsl 16)).
-define(u24(X), (((X)-16#800000) bsl 8)).
-define(u32(X), (((X)-16#80000000))).

%% decode samples into signed linear 32 bit if integer
%% and floating point if float format
decode_frame(Frame, SrcFormat) ->
    case SrcFormat of
	s8 -> [ ?i8(X) || <<X:8/signed>> <= Frame];
	u8 -> [ ?u8(X) || <<X:8/unsigned>> <= Frame];
	s16_le -> [ ?i16(X) || <<X:16/signed-little>> <= Frame];
	s16_be -> [ ?i16(X) || <<X:16/signed-big>> <= Frame];
	u16_le -> [ ?u16(X) || <<X:16/unsigned-little>> <= Frame];
	u16_be -> [ ?u16(X) || <<X:16/unsigned-big>> <= Frame];
	s24_le -> [ ?i24(X) || <<X:32/signed-little>> <= Frame];
	s24_be -> [ ?i24(X) || <<X:32/signed-big>> <= Frame];
	u24_le -> [ ?u24(X) || <<X:32/unsigned-little>> <= Frame];
	u24_be -> [ ?u24(X) || <<X:32/unsigned-big>> <= Frame];
	s32_le -> [ ?i32(X) || <<X:32/signed-little>> <= Frame];
	s32_be -> [ ?i32(X) || <<X:32/signed-big>> <= Frame];
	u32_le -> [ ?u32(X) || <<X:32/unsigned-little>> <= Frame];
	u32_be -> [ ?u32(X) || <<X:32/unsigned-big>> <= Frame];
	float_le -> [ X || <<X:32/float-little>> <= Frame];
	float_be -> [ X || <<X:32/float-big>> <= Frame];
	float64_le -> [ X || <<X:64/float-little>> <= Frame];
	float64_be -> [ X || <<X:64/float-big>> <= Frame];
	mu_law -> [ (mu_law_decode(X) bsl 16) || <<X>> <= Frame ];
	a_law -> [ (a_law_decode(X) bsl 16) || <<X>> <= Frame ]
    end.

-spec encode_frame(Xs :: [int32()]|[float01()], Format::alsa:format()) ->
	  binary().

encode_frame(Xs, Format) when is_integer(hd(Xs)) ->
    case Format of
	s8     -> << <<(X bsr 24)>> || X <- Xs>>;
	u8     -> << <<((X bsr 24)+16#80)>> || X <- Xs>>;
	s16_le -> << <<(X bsr 16):16/signed-little>> || X <- Xs >>;
	s16_be -> << <<(X bsr 16):16/signed-big>> || X <- Xs >>;
	u16_le -> << <<((X bsr 16)+16#8000):16/unsigned-little>> || X <- Xs >>;
	u16_be -> << <<((X bsr 16)+16#8000):16/unsigned-big>> || X <- Xs >>;
	s24_le -> << <<(X bsr 8):32/signed-little>> || X <- Xs >>;
	s24_be -> << <<(X bsr 8):32/signed-big>> || X <- Xs >>;
	u24_le -> << <<((X bsr 8)+16#800000):32/unsigned-little>> || X <- Xs >>;
	u24_be -> << <<((X bsr 8)+16#800000):32/unsigned-big>> || X <- Xs >>;
	s32_le -> << <<X:32/signed-little>> || X <- Xs >>;
	s32_be -> << <<X:32/signed-big>> || X <- Xs >>;
	u32_le -> << <<(X+16#80000000):32/unsigned-little>> || X <- Xs >>;
	u32_be -> << <<(X+16#80000000):32/unsigned-big>> || X <- Xs >>;
	mu_law -> << <<(mu_law_encode(X bsr 16)) >> || X <- Xs >>;
	a_law -> << <<(a_law_encode(X bsr 16)) >> || X <- Xs >>;
	float_le -> << <<(trunc(X/16#8000000)):32/float-little>> || X <- Xs >>;
	float_be -> << <<(trunc(X/16#8000000)):32/float-big>>    || X <- Xs >>;
	float64_le -> << <<(trunc(X/16#8000000)):64/float-little>> || X<-Xs >>;
	float64_be -> << <<(trunc(X/16#8000000)):64/float-big>>    || X<-Xs >>
    end;
encode_frame(Xs, DstFormat) when is_float(hd(Xs)) ->
    case DstFormat of
	s8 -> << <<(trunc(X*16#7f)):8/signed>> || X <- Xs>>;
	u8 -> << <<(trunc(X*16#7f)+16#80):8/unsigned>> || X <- Xs>>;
	s16_le -> << <<(trunc(X*16#7fff)):16/signed-little>> || X <- Xs >>;
	s16_be -> << <<(trunc(X*16#7fff)):16/signed-big>> || X <- Xs >>;
	u16_le -> << <<(trunc(X*16#7fff)+16#8000):16/unsigned-little>> || X <- Xs >>;
	u16_be -> << <<(trunc(X*16#7fff)+16#8000):16/unsigned-big>> || X <- Xs >>;
	s24_le -> << <<(trunc(X*16#7fffffff)):32/signed-little>> || X <- Xs >>;
	s24_be -> << <<(trunc(X*16#7fffffff)):32/signed-big>> || X <- Xs >>;
	u24_le -> << <<(trunc(X*16#7fffffff)+16#800000):32/unsigned-little>> || X <- Xs >>;
	u24_be -> << <<(trunc(X*16#7fffffff)+16#800000):32/unsigned-big>> || X <- Xs >>;
	s32_le -> << <<X:32/signed-little>> || X <- Xs >>;
	s32_be -> << <<X:32/signed-big>> || X <- Xs >>;
	u32_le -> << <<(X+16#80000000):32/unsigned-little>> || X <- Xs >>;
	u32_be -> << <<(X+16#80000000):32/unsigned-big>> || X <- Xs >>;
	mu_law -> << <<(mu_law_encode(trunc(X*16#7fff)))>> || X <- Xs >>;
	a_law -> << <<(a_law_encode(trunc(X*16#7fff)))>> || X <- Xs >>;
	float_le -> << <<X:32/float-little>> || X <- Xs >>;
	float_be -> << <<X:32/float-big>>    || X <- Xs >>;
	float64_le -> << <<X:64/float-little>> || X <- Xs >>;
	float64_be -> << <<X:64/float-big>>    || X <- Xs >>
    end.


aLawDecompressTable() ->
{
     -5504, -5248, -6016, -5760, -4480, -4224, -4992, -4736,
     -7552, -7296, -8064, -7808, -6528, -6272, -7040, -6784,
     -2752, -2624, -3008, -2880, -2240, -2112, -2496, -2368,
     -3776, -3648, -4032, -3904, -3264, -3136, -3520, -3392,
     -22016,-20992,-24064,-23040,-17920,-16896,-19968,-18944,
     -30208,-29184,-32256,-31232,-26112,-25088,-28160,-27136,
     -11008,-10496,-12032,-11520,-8960, -8448, -9984, -9472,
     -15104,-14592,-16128,-15616,-13056,-12544,-14080,-13568,
     -344,  -328,  -376,  -360,  -280,  -264,  -312,  -296,
     -472,  -456,  -504,  -488,  -408,  -392,  -440,  -424,
     -88,   -72,   -120,  -104,  -24,   -8,    -56,   -40,
     -216,  -200,  -248,  -232,  -152,  -136,  -184,  -168,
     -1376, -1312, -1504, -1440, -1120, -1056, -1248, -1184,
     -1888, -1824, -2016, -1952, -1632, -1568, -1760, -1696,
     -688,  -656,  -752,  -720,  -560,  -528,  -624,  -592,
     -944,  -912,  -1008, -976,  -816,  -784,  -880,  -848,
      5504,  5248,  6016,  5760,  4480,  4224,  4992,  4736,
      7552,  7296,  8064,  7808,  6528,  6272,  7040,  6784,
      2752,  2624,  3008,  2880,  2240,  2112,  2496,  2368,
      3776,  3648,  4032,  3904,  3264,  3136,  3520,  3392,
      22016, 20992, 24064, 23040, 17920, 16896, 19968, 18944,
      30208, 29184, 32256, 31232, 26112, 25088, 28160, 27136,
      11008, 10496, 12032, 11520, 8960,  8448,  9984,  9472,
      15104, 14592, 16128, 15616, 13056, 12544, 14080, 13568,
      344,   328,   376,   360,   280,   264,   312,   296,
      472,   456,   504,   488,   408,   392,   440,   424,
      88,    72,   120,   104,    24,     8,    56,    40,
      216,   200,   248,   232,   152,   136,   184,   168,
      1376,  1312,  1504,  1440,  1120,  1056,  1248,  1184,
      1888,  1824,  2016,  1952,  1632,  1568,  1760,  1696,
      688,   656,   752,   720,   560,   528,   624,   592,
      944,   912,  1008,   976,   816,   784,   880,   848
}.



a_law_decode(X) ->
    element(X+1, aLawDecompressTable()).

aLawCompressTable() ->
{
     1,1,2,2,3,3,3,3,
     4,4,4,4,4,4,4,4,
     5,5,5,5,5,5,5,5,
     5,5,5,5,5,5,5,5,
     6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7
}.

a_law_encode(X0) ->
    Sign = (((bnot X0) bsr 8) band 16#80) bxor 16#55,
    X  = min(abs(X0), 32635),
    if X >= 256 ->
	    Exponent = element(((X bsr 8) band 16#7f)+1, aLawCompressTable()),
	    Mantissa = (X bsr (Exponent+3)) band 16#f,
	    (Exponent bsl 4) bor Mantissa bxor Sign;
       true ->
	    ((X bsr 4) band 16#ff) bxor Sign
    end.

muLawDecompressTable() ->
{
     -32124,-31100,-30076,-29052,-28028,-27004,-25980,-24956,
     -23932,-22908,-21884,-20860,-19836,-18812,-17788,-16764,
     -15996,-15484,-14972,-14460,-13948,-13436,-12924,-12412,
     -11900,-11388,-10876,-10364, -9852, -9340, -8828, -8316,
      -7932, -7676, -7420, -7164, -6908, -6652, -6396, -6140,
      -5884, -5628, -5372, -5116, -4860, -4604, -4348, -4092,
      -3900, -3772, -3644, -3516, -3388, -3260, -3132, -3004,
      -2876, -2748, -2620, -2492, -2364, -2236, -2108, -1980,
      -1884, -1820, -1756, -1692, -1628, -1564, -1500, -1436,
      -1372, -1308, -1244, -1180, -1116, -1052,  -988,  -924,
       -876,  -844,  -812,  -780,  -748,  -716,  -684,  -652,
       -620,  -588,  -556,  -524,  -492,  -460,  -428,  -396,
       -372,  -356,  -340,  -324,  -308,  -292,  -276,  -260,
       -244,  -228,  -212,  -196,  -180,  -164,  -148,  -132,
       -120,  -112,  -104,   -96,   -88,   -80,   -72,   -64,
        -56,   -48,   -40,   -32,   -24,   -16,    -8,     -1,
      32124, 31100, 30076, 29052, 28028, 27004, 25980, 24956,
      23932, 22908, 21884, 20860, 19836, 18812, 17788, 16764,
      15996, 15484, 14972, 14460, 13948, 13436, 12924, 12412,
      11900, 11388, 10876, 10364,  9852,  9340,  8828,  8316,
       7932,  7676,  7420,  7164,  6908,  6652,  6396,  6140,
       5884,  5628,  5372,  5116,  4860,  4604,  4348,  4092,
       3900,  3772,  3644,  3516,  3388,  3260,  3132,  3004,
       2876,  2748,  2620,  2492,  2364,  2236,  2108,  1980,
       1884,  1820,  1756,  1692,  1628,  1564,  1500,  1436,
       1372,  1308,  1244,  1180,  1116,  1052,   988,   924,
        876,   844,   812,   780,   748,   716,   684,   652,
        620,   588,   556,   524,   492,   460,   428,   396,
        372,   356,   340,   324,   308,   292,   276,   260,
        244,   228,   212,   196,   180,   164,   148,   132,
        120,   112,   104,    96,    88,    80,    72,    64,
         56,    48,    40,    32,    24,    16,     8,     0
}.

mu_law_decode(X) ->
    element(X+1, muLawDecompressTable()).


muLawCompressTable() ->
{
     0,0,1,1,2,2,2,2,3,3,3,3,3,3,3,3,
     4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,
     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
     5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,
     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
     6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,
     7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7
}.

mu_law_encode(X0) ->
    Sign = if X0 < 0 -> 16#80; true -> 16#00 end,
    X  = min(abs(X0), 32635) + 16#84,
    Exponent = element(((X bsr 7) band 16#ff)+1, muLawCompressTable()),
    Mantissa = (X bsr (Exponent+3)) band 16#f,
    (bnot (Sign bor (Exponent bsl 4) bor Mantissa) band 16#ff).



%% add timeout? probably not...
async_wait_ready(H) ->
    alsa:select_(H),
    receive
	{select,H,undefined,_Ready} ->
	    ok
    end.

adjust_params(Params, Range) ->
    adjust_params(Params, Range, []).
adjust_params([{channels,Channels}|Params], Range, Acc) ->
    Min = proplists:get_value(channels_min, Range),
    Max = proplists:get_value(channels_max, Range),
    adjust_params(Params, Range, 
		  [{channels,adjust_value(Channels,Min,Max)}|Acc]);
adjust_params([{rate,Rate}|Params], Range, Acc) ->
    Min = proplists:get_value(rate_min, Range),
    Max = proplists:get_value(rate_max, Range),
    adjust_params(Params, Range, [{rate, adjust_value(Rate, Min, Max)}|Acc]);
adjust_params([{period_size,PeriodSize}|Params], Range, Acc) ->
    Min = proplists:get_value(period_size_min, Range),
    Max = proplists:get_value(period_size_max, Range),
    adjust_params(Params, Range, 
		  [{period_size, adjust_value(PeriodSize,Min,Max)}|Acc]);
adjust_params([{format,Format}|Params], Range, Acc) ->
    FormatList = proplists:get_value(formats, Range),
    case lists:member(Format, FormatList) of
	true ->
	    adjust_params(Params, Range, [{format, Format}|Acc]);
	false ->
	    ?warning("requested format ~s not present\n", [Format]),
	    Width = alsa:format_width(Format),
	    PhysWidth = alsa:format_physical_width(Format),
	    Linear = alsa:format_linear(Format),
	    %% look for format with same width (physical width)
	    case [F || F <- alsa:formats(),
		       PhysWidth =:= alsa:format_physical_width(F),
		       Width     =:= alsa:format_width(F),
		       Linear    =:= alsa:format_linear(F)] of
		[F|_] ->
		    ?info("selected format ~s instead\n", [F]),
		    adjust_params(Params, Range, [{format,F}|Acc])
	    end
    end;
adjust_params([], _Range, Acc) ->
    {ok, lists:reverse(Acc)}.

adjust_value(Value, Min, _Max) when Value < Min -> Min;
adjust_value(Value, _Min, Max) when Value > Max -> Max;
adjust_value(Value, _Min, _Max) -> Value.

%%
%% Testing
%%
test_mu_law() ->
    lists:foreach(
      fun(I) ->
	      I = mu_law_encode(mu_law_decode(I))
      end, lists:seq(0, 255)).

test_a_law() ->
    lists:foreach(
      fun(I) ->
	      I = a_law_encode(a_law_decode(I))
      end, lists:seq(0, 255)).

test_float() ->
    Xs  = [-1.0,-0.7,-0.5,-0.2, 0.0, 0.2, 0.5, 0.7, 1.0],
    Bin = encode_frame(Xs, float_le),
    Ys  =  decode_frame(Bin, float_le),
    true = lists:all(fun({X,Y}) -> abs(X-Y) < 0.001 end,
		     lists:zip(Xs,Ys)).

test_float64() ->
    Xs  = [-1.0,-0.7,-0.5,-0.2, 0.0, 0.2, 0.5, 0.7, 1.0],
    Bin = encode_frame(Xs, float64_le),
    Ys  =  decode_frame(Bin, float64_le),
    true = lists:all(fun({X,Y}) -> abs(X-Y) < 0.001 end,
		     lists:zip(Xs,Ys)).

test_integer() ->
    Xs = [?i8(0), ?i8(20), ?i8(30), ?i8(50), ?i8(100), ?i8(127)],
    lists:foreach(
      fun(Format) ->
	      Bin = encode_frame(Xs, Format),
	      Ys  = decode_frame(Bin, Format),
	      true = lists:all(fun({X,Y}) -> X =:= Y end,
			       lists:zip(Xs,Ys))
      end, [s8,u8,
	    s16_le,s16_be,u16_le,u16_be,
	    s24_le,s24_be,u24_le,u24_be,
	    s32_le,s32_be,u32_le,u32_be]),
    NXs = [?i8(-127), ?i8(-100), ?i8(-50), ?i8(-30), ?i8(-20)],
    lists:foreach(
      fun(Format) ->
	      Bin = encode_frame(NXs, Format),
	      Ys  = decode_frame(Bin, Format),
	      true = lists:all(fun({X,Y}) -> X =:= Y end,
			       lists:zip(NXs,Ys))
      end, [s8,
	    s16_le,s16_be,
	    s24_le,s24_be,
	    s32_le,s32_be]).
    
test() ->
    test_mu_law(),
    test_a_law(),
    test_float(),
    test_float64(),
    test_integer(),
    ok.
