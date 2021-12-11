-ifndef(__ALSA_AU__HRL__).
-define(__ALSA_AU__HRL__, true).

%% Define the magic number - audio file format
-define(AU_MAGIC, ".snd").

%% Define the encoding fields
-define(AU_ENCODING_MULAW_8,    1).   %% 8-bit ISDN u-law
-define(AU_ENCODING_LINEAR_8,    2).   %% 8-bit linear PCM 
-define(AU_ENCODING_LINEAR_16,   3).   %% 16-bit linear PCM 
-define(AU_ENCODING_LINEAR_24,   4).   %% 24-bit linear PCM 
-define(AU_ENCODING_LINEAR_32,   5).   %% 32-bit linear PCM 
-define(AU_ENCODING_FLOAT,       6).   %% 32-bit IEEE floating point 
-define(AU_ENCODING_DOUBLE,      7).   %% 64-bit IEEE floating point 
-define(AU_ENCODING_ADPCM_G721,  23).  %% 4-bit CCITT g.721 ADPCM 
-define(AU_ENCODING_ADPCM_G722,  24).  %% CCITT g.722 ADPCM 
-define(AU_ENCODING_ADPCM_G723_3, 25). %% CCITT g.723 3-bit ADPCM 
-define(AU_ENCODING_ADPCM_G723_5, 26). %% CCITT g.723 5-bit ADPCM 
-define(AU_ENCODING_ALAW_8,      27).  %% 8-bit ISDN A-law 

%% 
%%    u_int32  magic;          %% magic number 
%%    u_int32  data_offset;    %% size of this header  (16?)
%%    u_int32  data_size;      %% length of data (optional) 0xffffffff = unknown
%%    u_int32  encoding;       %% data encoding format 
%%    u_int32  sample_rate;    %% samples per second 
%%    u_int32  channels;       %% number of interleaved channels 

-define(AU_HEADER_FIELDS(
	  DataSize,
	  Encoding,
	  SampleRate,
	  Channels),
	DataSize:32,
	Encoding:32,
	SampleRate:32,
	Channels:32).

-endif.

