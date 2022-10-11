%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Simple tests of alsa_play
%%% @end
%%% Created :  7 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(alsa_play_test).

-export([test1/0, test3/0,
	 test_music/0,
	 test_pong/0,
	 test_notify/0,
	 test_notify_once/0,
	 test_notify_once_one/0,
	 test_notify_music/0,
	 test_left_right/0
	]).
-export([test_wave/0]).
-export([test_mem/0]).
-export([test_repeat_music/0]).

test1() ->	 
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:start(#{rate=>16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    alsa_play:append_file(1, filename:join(Sounds, "Front_Center.wav")),
    alsa_play:mark(1, last, [{set,bof}], undefined),
    erlang:start_timer(5000, self(), stop),
    alsa_play:run(),
    alsa_play:resume(),
    test_notify_loop(),    
    alsa_play:remove(1),
    ok.

test3() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:start(#{rate=>16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    alsa_play:new(2),
    alsa_play:new(3),

    alsa_play:append_file(1, filename:join(Sounds, "Front_Center.wav")),
    alsa_play:append_file(2, filename:join(Sounds, "Front_Left.wav")),
    alsa_play:append_file(2, filename:join(Sounds, "Front_Right.wav")),
    alsa_play:append_file(3, filename:join(Sounds, "Rear_Right.wav")),

    alsa_play:mark(1, last, [{set,bof}], undefined),
    alsa_play:mark(2, last, [{set,bof}], undefined),
    alsa_play:mark(3, last, [{set,bof}], undefined),

    erlang:start_timer(5000, self(), stop),

    alsa_play:run(),
    alsa_play:resume(),
    test_notify_loop(),    
    alsa_play:remove(1),
    alsa_play:remove(2),
    alsa_play:remove(3),
    ok.

test_pong() ->
    alsa_play:start(#{}),
    alsa_play:remove(),

    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:new(1),
    alsa_play:mute(1, true),
    alsa_play:append_file(1, filename:join(Sounds, "plop.wav")),   
    alsa_play:new(2),
    alsa_play:mute(2, true),
    alsa_play:append_file(2, filename:join(Sounds, "beep.wav")),
    alsa_play:new(3),
    alsa_play:mute(3, true),
    alsa_play:append_file(3, filename:join(Sounds, "Side_Left.wav")),
    alsa_play:run(),
    alsa_play:resume(),
    timer:sleep(500),

    lists:foreach(
      fun(_) ->
	      alsa_play:mute(1, false),
	      alsa_play:restart(1),
	      timer:sleep(500), %% listen to plop
	      alsa_play:mute(1, true),

	      alsa_play:mute(2, false),
	      alsa_play:restart(2),
	      timer:sleep(500),  %% listen to beep
	      alsa_play:mute(2, true),

	      alsa_play:mute(3, false),
	      alsa_play:restart(3),
	      timer:sleep(2000),  %% listen to Side_Left
	      alsa_play:mute(3, true)
      end, lists:seq(1, 4)),

    alsa_play:mark(1, last, [{set,bof}], undefined),
    alsa_play:mark(2, last, [{set,bof}], undefined),
    alsa_play:mute(1, false),
    alsa_play:mute(2, false),
    alsa_play:mute(3, false),
    alsa_play:restart(3),
    
    timer:sleep(2000),  %% listen to all

    alsa_play:pause(),

    alsa_play:remove(1),
    alsa_play:remove(2),
    alsa_play:remove(3),
    ok.

test_notify() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:start(#{ rate => 16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    alsa_play:new(2),
    alsa_play:new(3),
    alsa_play:append_file(1, filename:join(Sounds, "Front_Center.wav")),
    alsa_play:append(1, {silence, {time,100}}),
    alsa_play:append_file(2, filename:join(Sounds, "Front_Left.wav")),
    alsa_play:append(2, {silence, {time,100}}),
    alsa_play:append_file(3, filename:join(Sounds, "Front_Right.wav")),
    alsa_play:append(3, {silence, {time,100}}),
    {ok,Ref1} = alsa_play:mark(1, last, [notify], center_done),
    {ok,Ref2} = alsa_play:mark(2, last, [notify], left_done),
    {ok,Ref3} = alsa_play:mark(3, last, [notify], right_done),
    alsa_play:run(1),
    alsa_play:resume(),
    receive
	{Ref1, 1, _Pos1, center_done} ->
	    alsa_play:run(2)
    end,
    receive
	{Ref2, 2, _Pos2, left_done} ->
	    alsa_play:run(3)
    end,
    receive
	{Ref3, 3, _Pos3, right_done} ->
	    ok
    end,
    alsa_play:pause(),
    alsa_play:unmark(1, Ref1),
    alsa_play:unmark(2, Ref2),
    alsa_play:unmark(3, Ref3),
    alsa_play:remove(1),
    alsa_play:remove(2),
    alsa_play:remove(3),
    ok.

%% play each wav file in sequence in different channels 1,2,3,
%% restart channel and remove mark after each completed play
test_notify_once() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:start(#{ rate => 16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    alsa_play:new(2),
    alsa_play:new(3),
    alsa_play:append_file(1, filename:join(Sounds, "Front_Center.wav")),
    alsa_play:append(1, {silence, {time,100}}),
    alsa_play:append_file(2, filename:join(Sounds, "Front_Left.wav")),
    alsa_play:append(2, {silence, {time,100}}),
    alsa_play:append_file(3, filename:join(Sounds, "Front_Right.wav")),
    alsa_play:append(3, {silence, {time,100}}),

    {ok,Ref1} = alsa_play:mark(1, last, [notify,once], sample_played),
    alsa_play:run(1),
    alsa_play:resume(),
    receive
	{Ref1, 1, _Pos1, sample_played} ->
	    ok
    end,

    {ok,Ref2} = alsa_play:mark(2, last, [notify,once], sample_played),
    alsa_play:run(2),
    receive
	{Ref2, 2, _Pos2, sample_played} ->
	    ok
    end,

    {ok,Ref3} = alsa_play:mark(3, last, [notify,once], sample_played),
    alsa_play:run(3),
    receive
	{Ref3, 3, _Pos3, sample_played} ->
	    ok
    end,
    alsa_play:pause(),
    alsa_play:remove(1),
    alsa_play:remove(2),
    alsa_play:remove(3),
    ok.


%% play each wav file in sequence in the same channels 1
%% remove mark after each completed play
test_notify_once_one() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:start(#{ rate => 16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    alsa_play:append_file(1, filename:join(Sounds, "Front_Center.wav")),
    %% append(1, {silence, {time,100}}),
    {ok,Ref1} = alsa_play:mark(1, last, [notify,once,stop], sample1_played),

    alsa_play:append_file(1, filename:join(Sounds, "Front_Left.wav")),
    %% append(2, {silence, {time,100}}),
    {ok,Ref2} = alsa_play:mark(1, last, [notify,once,stop], sample2_played),
    alsa_play:append_file(1, filename:join(Sounds, "Front_Right.wav")),
    %% append(3, {silence, {time,100}}),
    {ok,Ref3} = alsa_play:mark(1, last, [notify,once,stop], sample3_played),

    alsa_play:run(1),
    alsa_play:resume(),
    receive
	{Ref1, 1, _Pos1, sample1_played} ->
	    ok
    end,
    alsa_play:run(1),
    alsa_play:pause(),
    alsa_play:resume(),
    receive
	{Ref2, 1, _Pos2, sample2_played} ->
	    ok
    end,
    alsa_play:run(1),
    alsa_play:pause(),alsa_play:resume(),
    receive
	{Ref3, 1, _Pos3, sample3_played} ->
	    ok
    end,
    alsa_play:pause(),
    alsa_play:remove(1),
    ok.

test_music() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),    
    alsa_play:start(#{ rate => 16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    alsa_play:append_file(1, filename:join(Sounds, "POL-super-match-short.wav")),
    {ok,Ref1} = alsa_play:mark(1, last, [notify, stop], music_done),
    alsa_play:run(),
    alsa_play:resume(),
    receive
	{Ref1, _Chan, _Pos, What} ->
	    io:format("~s\n", [What])
    end,
    alsa_play:remove(1).
    
test_notify_music() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:start(#{ rate => 16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    alsa_play:append_file(1, filename:join(Sounds, "POL-super-match-short.wav")),
    {ok,Ref1} = alsa_play:mark(1, last, [notify,restart], "Music"),
    alsa_play:new(2),
    alsa_play:append(2, {silence, {time,500}}),
    alsa_play:append_file(2, filename:join(Sounds, "Front_Left.wav")),
    alsa_play:append(2, {silence, {time,1500}}),
    {ok,Ref2} = alsa_play:mark(2, {time,2000}, [restart], "Left"),
    alsa_play:new(3),
    alsa_play:append(3, {silence, {time,1000}}),
    alsa_play:append_file(3, filename:join(Sounds, "Front_Right.wav")),
    alsa_play:append(3, {silence, {time,1000}}),
    {ok,Ref3} = alsa_play:mark(3, {time,2000}, [restart], "Right"),

    erlang:start_timer(15000, self(), stop),
    alsa_play:run(),
    alsa_play:resume(),
    test_notify_loop(),
    alsa_play:unmark(1,Ref1),
    alsa_play:unmark(2,Ref2),
    alsa_play:unmark(3,Ref3),
    alsa_play:remove(1),
    alsa_play:remove(2),
    alsa_play:remove(3),
    ok.

test_repeat_music() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:start(#{ rate => 16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    alsa_play:append_file(1, filename:join(Sounds, "POL-super-match-short.wav")),
    {ok,_} = alsa_play:mark(1, {time,500},   [notify, {set,{time,700}}], jump_1),
    {ok,_} = alsa_play:mark(1, {time,1000},  [notify, {set,{time,1200}}], jump_2),
    {ok,_} = alsa_play:mark(1, {time,1500},  [notify, {set,{time,2300}}], jump_3),
    {ok,_} = alsa_play:mark(1, {time,2000},  [notify, {set,{time,1000}}], jump_4),
    {ok,_} = alsa_play:mark(1, {time,2400},  [notify, {set,{time,2500}}], jump_5),
    {ok,_} = alsa_play:mark(1, {time,2600},  [notify, {repeat,{time,800},3}], jump_6),

    {ok,_} = alsa_play:mark(1, last,  [notify, stop], stop),

    alsa_play:run(),
    alsa_play:resume(),

    test_notify_loop(),

    alsa_play:remove(1).
 


%% Edit Front_Left and Front_Right to say Left/Right in a loop
test_left_right() ->
    Sounds = filename:join(code:lib_dir(alsa), "sounds"),
    alsa_play:start(#{ rate => 16000 }),
    alsa_play:remove(),

    alsa_play:new(1),

    alsa_play:append(1, {silence, {time,100}}),
    alsa_play:mark(1, last, [{set,{eof,{time,500}}}], undefined),
    alsa_play:append_file(1, filename:join(Sounds, "Front_Left.wav")),

    alsa_play:append(1, {silence, {time,100}}),
    alsa_play:mark(1, last, [{set,{eof,{time,550}}}], undefined),
    alsa_play:append_file(1, filename:join(Sounds, "Front_Right.wav")),

    alsa_play:mark(1, last, [{repeat,bof,2}], undefined),
    alsa_play:append(1, {silence, 1}),
    alsa_play:mark(1, last, [notify,stop], stop),
    alsa_play:run(),
    alsa_play:resume(),
    test_notify_loop(),
    alsa_play:remove(1),
    ok.

test_wave() ->
    alsa_play:start(#{ rate => 44100, latency => 50 }),
    alsa_play:remove(),

    alsa_play:new(1),
    E = [0.2, 0.5, 0.2],
    D = trunc(1000*lists:sum(E)),
    alsa_play:set_wave(1, [{envelope,E},
			   {wave,0, [#{ form=>sine, freq=>"A3", level=>0.0},
				     #{ form=>sine, freq=>"D4", level=>0.9},
				     #{ form=>sine, freq=>"D5", level=>0.9},
				     #{ form=>sine, freq=>"A3", level=>0.0}
				    ]},
			   
			   {wave,1, [#{ form=>sine, freq=>"G3", level=>0.0},
				     #{ form=>sine, freq=>"G3", level=>0.9},
				     #{ form=>sine, freq=>"G4", level=>0.9},
				     #{ form=>sine, freq=>"G3", level=>0.0}]},
			   
			   {wave,2, [#{ form=>sine, freq=>"E3", level=>0.0},
				     #{ form=>sine, freq=>"E3", level=>0.9},
				     #{ form=>sine, freq=>"E4", level=>0.9},
				     #{ form=>sine, freq=>"E3", level=>0.0}]}
			  ]),
    alsa_play:mark(1, {time,D}, [notify,{set,bof}], restart),
    erlang:start_timer(3*D, self(), stop),
    alsa_play:run(),
    alsa_play:resume(),
    test_notify_loop(),
    alsa_play:remove(1).

test_mem() ->
    %%Sounds = filename:join(code:lib_dir(alsa), "sounds"),    
    alsa_play:start(#{ rate => 16000 }),
    alsa_play:remove(),

    alsa_play:new(1),
    %% alsa_play:append_file(1, filename:join(Sounds, "Front_Left.wav")),
    {ok,Ref1} = alsa_play:mark(1, last, [notify, stop], music_done),
    alsa_play:unmark(1, Ref1),

    %% alsa_play:run(),
    %% alsa_play:resume(),

    alsa_play:remove(1).

test_notify_loop() ->
    receive
	{timeout, _, stop} ->  %% timer stop
	    alsa_play:pause(),
	    ok;
	{_Ref, _Chan, _Pos, stop} ->  %% message stop
	    alsa_play:pause(),
	    ok;
	{_Ref, _Chan, _Pos, What} -> %% other notifications
	    io:format("~s\n", [What]),
	    test_notify_loop()
    end.
