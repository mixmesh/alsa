-ifndef(ALSA_WAV).
-define(ALSA_WAV, true).

-define(IFF_ID_FORM, <<"FORM">>).
-define(IFF_ID_AIFF, <<"AIFF">>).
-define(IFF_ID_AIFC, <<"AIFC">>).
-define(IFF_ID_COMM, <<"COMM">>).
-define(IFF_ID_SSND, <<"SSND">>).
-define(IFF_ID_MPEG, <<"MPEG">>).
-define(IFF_ID_NONE, <<"NONE">>).
-define(IFF_ID_2CBE, <<"twos">>).
-define(IFF_ID_2CLE, <<"sowt">>).
-define(WAV_ID_RIFF, <<"RIFF">>).
-define(WAV_ID_WAVE, <<"WAVE">>).
-define(WAV_ID_FMT , <<"fmt ">>).
-define(WAV_ID_DATA, <<"data">>).

-define(WAVE_FORMAT_PCM,        16#0001).
-define(WAVE_FORMAT_IEEE_FLOAT, 16#0003).
-define(WAVE_FORMAT_ALAW,       16#0006).
-define(WAVE_FORMAT_ULAW,       16#0007).
-define(WAVE_FORMAT_EXTENSIBLE, 16#FFFE).

%% WAV HEADER LAYOUT
%% Offs  Type                 Typical value
%% 0   <<"RIFF">>
%% 4   FileLength:32/little
%% 8   <<"WAVE">>
%% 12  <<"fmt ">>
%% 16  HeaderLen:32/little       16
%% 20  AudioFormat:16/little     1   ?WAVE_FORMAT_PCM
%% 22  NumChannels:16/little     2
%% 24  SampleRate:32/little     44100
%% 28  ByteRate:32/little,      176400 (SampleRate*BitsPerSample*Channel+7)/8
%% 32  FrameSize:16/little      4      (BitsPerSample*Channels+7)/8
%% 34  BitsPerChannel:16/little 16
%% ----- AudioFormat = ?WAVE_FORMAT_EXTENSIBLE = 16#FFFE
%% 36  CbSize:16/little
%% 38  ValidBitsPerChannel:16/little
%% 40  ChannelMask:32/little
%% 44  AudioFormat:16/little     actual audio format
%% 36/46 <<"data">>
%% 40/50 DataLen:32/little
%%     Frame1
%%     Frame2
%%     ....
%%
-define(WAV_HEADER_FIELDS(
	  AudioFormat,
	  NumChannels,
	  SamplesRate,
	  ByteRate,
	  BlockAlign,
	  BitsPerChannel),
	AudioFormat:16/little,
	NumChannels:16/little,
	SampleRate:32/little,
	ByteRate:32/little,        %% (SampleRate*BitsPerSample*Channel)/8
	FrameSize:16/little,	   %% BitsPerSample*Channels)/8
	BitsPerChannel:16/little).

-define(XWAV_HEADER_FIELDS(
	  CbSize,
	  ValidBitsPerChannel,
	  ChannelMask,
	  AudioFormat),
	CbSize:16/little,
	ValidBitsPerChannel:16/little,
	ChannelMask:32/little,
	AudioFormat:16/little).
-endif.
