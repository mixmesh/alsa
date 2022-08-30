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
-define(WAV_ID_LIST, <<"LIST">>).

-define(WAV_LIST_INFO, <<"INFO">>).
%% INFO items
-define(WAV_INFO_IARL, <<"IARL">>). %% The location where the subject of the file is archived
-define(WAV_INFO_IART, <<"IART">>). %% The artist of the original subject of the file
-define(WAV_INFO_ICMS, <<"ICMS">>). %% The name of the person or organization that commissioned the original subject of the file
-define(WAV_INFO_ICMT, <<"ICMT">>). %% General comments about the file or its subject
-define(WAV_INFO_ICOP, <<"ICOP">>). %% Copyright information about the file (e.g., "Copyright Some Company 2011")
-define(WAV_INFO_ICRD, <<"ICRD">>). %% The date the subject of the file was created (creation date)
-define(WAV_INFO_ICRP, <<"ICRP">>). %% Whether and how an image was cropped
-define(WAV_INFO_IDIM, <<"IDIM">>). %% The dimensions of the original subject of the file
-define(WAV_INFO_IDPI, <<"IDPI">>). %% Dots per inch settings used to digitize the file
-define(WAV_INFO_IENG, <<"IENG">>). %% The name of the engineer who worked on the file
-define(WAV_INFO_IGNR, <<"IGNR">>). %% The genre of the subject
-define(WAV_INFO_IKEY, <<"IKEY">>). %% A list of keywords for the file or its subject
-define(WAV_INFO_ILGT, <<"ILGT">>). %% Lightness settings used to digitize the file
-define(WAV_INFO_IMED, <<"IMED">>). %% Medium for the original subject of the file
-define(WAV_INFO_INAM, <<"INAM">>). %% Title of the subject of the file (name)
-define(WAV_INFO_IPLT, <<"IPLT">>). %% The number of colors in the color palette used to digitize the file
-define(WAV_INFO_IPRD, <<"IPRD">>). %% Name of the title the subject was originally intended for
-define(WAV_INFO_ISBJ, <<"ISBJ">>). %% Description of the contents of the file (subject)
-define(WAV_INFO_ISFT, <<"ISFT">>). %% Name of the software package used to create the file
-define(WAV_INFO_ISRC, <<"ISRC">>). %% The name of the person or organization that supplied the original subject of the file
-define(WAV_INFO_ISRF, <<"ISRF">>). %% The original form of the material that was digitized (source form)
-define(WAV_INFO_ITCH, <<"ITCH">>). %% The name of the technician who digitized the subject file

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
%% 24  Rate:32/little            44100
%% 28  ByteRate:32/little,       176400 (Rate*BitsPerSample*Channel+7)/8
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
	  Rate,
	  ByteRate,
	  BlockAlign,
	  BitsPerChannel),
	AudioFormat:16/little,
	NumChannels:16/little,
	Rate:32/little,
	ByteRate:32/little,        %% (Rate*BitsPerSample*Channel)/8
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
