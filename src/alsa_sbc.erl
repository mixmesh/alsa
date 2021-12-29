%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2021, Tony Rogvall
%%% @doc
%%%    SBC codec
%%% @end
%%% Created : 19 Dec 2021 by Tony Rogvall <tony@rogvall.se>

-module(alsa_sbc).

-on_load(init/0).

-export([new/1, new/2]).
-export([reinit/2, reinit/3]).
-export([decode/2]).
-export([encode/2]).
-export([get_frame_length/1]).
-export([get_frame_duration/1]).
-export([get_codesize/1]).
-export([get_info/1]).
-export([finish/1]).

-export([new_/2, reinit_/3]).
-export([test/0]).

-type handle() :: reference().
-type reason() :: atom().
-type a2dp_conf() :: 
	#{ rate => 16000|32000|44100|48000,
	   channel_mode => mono|dual|stereo|joint_stereo,
	   block_length => 4|8|12|16,
	   subbands => 4|8,
	   allocation_method => snr|loudness,
	   minimum_bitpool_value => 0..255,
	   maximum_bitpool_value => 0..255
	 }.

-define(nif_stub,nif_stub_error(?LINE)).
nif_stub_error(Line) ->
    erlang:nif_error({nif_not_loaded,module,?MODULE,line,Line}).

init() ->
    ok = erlang:load_nif(
           filename:join(code:priv_dir(alsa), alsa_sbc_nif), none).

-spec new(Type::sbc | msbc) -> {ok,handle()} | {error, reason()}.
%% sbc defaults: #{rate=>44100, mode=>stereo, subbands=>8,
%%                 block_length=>16, endian=native}
%% msbc defaults: #{rate=>16000, mode=>mono, subbands=>8,
%%                 block_length=>15!, endian=native}


new(_Type) ->
    ?nif_stub.

-spec new(Type::a2dp, Conf::a2dp_conf()) ->
	  {ok,handle()} | {error, reason()}.
new(a2dp, Conf) ->
    new_(a2dp,make_a2dp_conf(Conf)).
-spec new_(Type::a2dp, Conf::a2dp_conf()) ->
	  {ok,handle()} | {error, reason()}.    
new_(_Type, _Conf) ->
    ?nif_stub.

-spec reinit(Handle::handle(), 
	     Type::sbc | msbc) -> {ok,handle()} | {error, reason()}.
reinit(_Handle, _Type) ->
    ?nif_stub.

-spec reinit(Handle::handle(), 
	     Type::a2dp, Conf::a2dp_conf()) ->
	  {ok,handle()} | {error, reason()}.
reinit(Handle, a2dp, Conf) when is_map(Conf) ->
    reinit_(Handle, a2pdp, make_a2dp_conf(Conf)).

-spec reinit_(Handle::handle(), 
	     Type::a2dp, Conf::a2dp_conf()) ->
	  {ok,handle()} | {error, reason()}.
reinit_(_Handle, _Type, _Conf) ->
    ?nif_stub.

%% result is a (reversed) list of data, input should normally
%% contains frames and tail binary will be null
-spec decode(Handle::handle(), Data::iolist()) ->
	  {ok, [binary()], binary()} | {error, reason()}.
decode(_Handle, _Data) ->
    ?nif_stub.

%% result is a (reversed) list of frames
-spec encode(Handle::handle(), Data::iolist()) ->
	  {ok, [binary()], binary()} | {error, reason()}.
encode(_Handle, _Data) ->
    ?nif_stub.

-spec get_frame_length(Handle::handle()) ->
	  {ok, integer()} | {error, reason()}.
get_frame_length(_Handle) ->
    ?nif_stub.

-spec get_frame_duration(Handle::handle()) ->
	  {ok, integer()} | {error, reason()}.
get_frame_duration(_Handle) ->
    ?nif_stub.

-spec get_codesize(Handle::handle()) ->
	  {ok, integer()} | {error, reason()}.
get_codesize(_Handle) ->
    ?nif_stub.

-spec get_info(Handle::handle()) ->
	  {ok, undefined|string()}.

get_info(_Handle) ->
    ?nif_stub.

%% terminate sbc state, will be terminated by garbage collection 
%% but this call can speed up resource reuse.
-spec finish(Handle::handle()) ->
	  ok.
finish(_Handle) ->
    ?nif_stub.


-define(A2DP_SAMPLING_FREQ_16000,	(1 bsl 3)).
-define(A2DP_SAMPLING_FREQ_32000,	(1 bsl 2)).
-define(A2DP_SAMPLING_FREQ_44100,	(1 bsl 1)).
-define(A2DP_SAMPLING_FREQ_48000,	(1 bsl 0)).

-define(A2DP_CHANNEL_MODE_MONO,		(1 bsl 3)).
-define(A2DP_CHANNEL_MODE_DUAL_CHANNEL,	(1 bsl 2)).
-define(A2DP_CHANNEL_MODE_STEREO,	(1 bsl 1)).
-define(A2DP_CHANNEL_MODE_JOINT_STEREO,	(1 bsl 0)).

-define(A2DP_BLOCK_LENGTH_4,		(1 bsl 3)).
-define(A2DP_BLOCK_LENGTH_8,		(1 bsl 2)).
-define(A2DP_BLOCK_LENGTH_12,		(1 bsl 1)).
-define(A2DP_BLOCK_LENGTH_16,		(1 bsl 0)).

-define(A2DP_SUBBANDS_4,		(1 bsl 1)).
-define(A2DP_SUBBANDS_8,		(1 bsl 0)).

-define(A2DP_ALLOCATION_SNR,		(1 bsl 1)).
-define(A2DP_ALLOCATION_LOUDNESS,	(1 bsl 0)).


make_a2dp_conf(Conf) when is_list(Conf) ->
    make_a2dp_conf(maps:from_list(Conf));
make_a2dp_conf(Conf) ->
    Frequency =
	case maps:get(rate, Conf, 44100) of
	    16000 -> ?A2DP_SAMPLING_FREQ_16000;
	    32000 -> ?A2DP_SAMPLING_FREQ_32000;
	    44100 -> ?A2DP_SAMPLING_FREQ_44100;
	    48000 -> ?A2DP_SAMPLING_FREQ_48000
	end,
    ChannelMode =
	case maps:get(channel_mode, Conf, stereo) of
	    mono         -> ?A2DP_CHANNEL_MODE_MONO;
	    dual         -> ?A2DP_CHANNEL_MODE_DUAL_CHANNEL;
	    stereo       -> ?A2DP_CHANNEL_MODE_STEREO;
	    joint_stereo -> ?A2DP_CHANNEL_MODE_JOINT_STEREO
	end,
    BlockLength =
	case maps:get(block_length, Conf, 16) of
	    4  -> ?A2DP_BLOCK_LENGTH_4;
	    8  -> ?A2DP_BLOCK_LENGTH_8;
	    12 -> ?A2DP_BLOCK_LENGTH_12;
	    16 -> ?A2DP_BLOCK_LENGTH_16
	end,
    SubBands = 
	case maps:get(subbands, Conf, 8) of
	    4 -> ?A2DP_SUBBANDS_4;
	    8 -> ?A2DP_SUBBANDS_8
	end,
    AllocationMethod =
	case maps:get(allocation_method, Conf, snr) of
	    snr      -> ?A2DP_ALLOCATION_SNR;
	    loudness -> ?A2DP_ALLOCATION_LOUDNESS
	end,
    MinBitPool = maps:get(minimum_bitpool_value, Conf, 0),
    MaxBitPool = maps:get(maximum_bitpool_value, Conf, 0),
    <<Frequency:4, ChannelMode:4,
      BlockLength:4, SubBands:2, AllocationMethod:2,
      MinBitPool:8, MaxBitPool:8>>.

    
%%
%% Simple encode/decode test
%%

test() ->
    {ok, H} = new(sbc),
    {ok,FrameLength} = get_frame_length(H),
    {ok,FrameDuration} = get_frame_duration(H),
    {ok,CodeSize} = get_codesize(H),
    io:format("frame size = ~w\n", [FrameLength]),
    io:format("frame duration = ~w\n", [FrameDuration]),
    io:format("code size = ~w\n", [CodeSize]),
    {ok,Info1} = get_info(H),
    io:format("info1 = ~w\n", [Info1]),
    Samples =  [{X,X} || X <- lists:seq(1,257)],
    Data = << <<L:16/little, R:16/little>> ||  {L,R} <- Samples>>,
    {ok,Fs, Tail} =  encode(H, Data),
    %% info should be available after encode?
    {ok,Info2} = get_info(H),
    io:format("info2 = ~w\n", [Info2]),
    <<257:16/little, 257:16/little>> = Tail,  %% not consumed
    {ok,Ds,<<>>} = decode(H, lists:reverse(Fs)),
    Data2 = iolist_to_binary(lists:reverse(Ds)),
    [ {L,R} || <<L:16/little, R:16/little>> <= Data2 ].
