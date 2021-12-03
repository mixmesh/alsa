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
	ByteRate:32/little,
	BlockAlign:16/little,	
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