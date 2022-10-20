%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    alsa_play plugin showing realtime samples
%%% @end
%%% Created : 15 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(alsa_play_epx).

-include_lib("epx/include/epx_menu.hrl").
-include_lib("epx/include/epx_image.hrl").

-export([start/0]).
-export([init/1,
	 configure/2,
	 key_press/2,
	 key_release/2,
	 button_press/2,
	 button_release/2,
	 enter/2,
	 leave/2,
	 focus_in/2,
	 focus_out/2,
	 close/1,
	 draw/3, draw/4,
	 command/3,
	 select/2,
	 motion/2,
	 menu/2
	]).
-export([handle_info/2]).

%% profile with default values
-record(profile,
	{
	 scheme                        = logo, %% xterm,
	 %% menu_info
	 menu_font_name                = "Arial",
	 menu_font_size                = 14,
	 menu_font_color               = grey5,   %% light
	 menu_background_color         = grey10,  %% dark
	 menu_border_color             = green
	}).

%% -define(verbose(F,A), ok).
-define(verbose(F,A), io:format((F),(A))).

-define(VIEW_WIDTH,  1024).
-define(VIEW_HEIGHT, 576).
-define(TEXT_COLOR, {0,0,0,0}).       %% black text
-define(BORDER, 2).

menu(DrawFFT) ->
    [
     {"FFT="++if DrawFFT -> "ON"; true -> "OFF" end, "F"},
     {"1Hz",  "1"},
     {"10Hz", "2"},
     {"50Hz", "3"}
    ].

start() ->
    application:ensure_all_started(epx),
    epxw:start(#{ module => ?MODULE,
		  %% demo select as fun
		  select => fun(Event={_Phase,Rect={X,Y,W,H}}, State) -> 
				    ?verbose("SELECT FUN: ~w\n", [Event]),
				    OldRect = maps:get(selection, State),
				    epxw:invalidate(OldRect),
				    epxw:invalidate({X-?BORDER-2,Y-?BORDER-2,
						     W+2*?BORDER+4,
						     H+2*?BORDER+4}),
				    State#{ selection => Rect }
			    end
		},
	       [hello,world], 
	       [{title, "AlsaPlay"},
		{scroll_horizontal, bottom},  %% none|top|bottom
		{scroll_vertical,   left},    %% none|left|right
		{scroll_bar_color,  cyan},
		{scroll_hndl_color, blue},
		{scroll_bar_size,   14},
		{scroll_hndl_size,  10},
		{left_bar, 32},
		{top_bar, 20},
		{right_bar, 8},
		{width, ?VIEW_WIDTH},
		{height, ?VIEW_HEIGHT},
		{view_width,?VIEW_WIDTH},
		{view_height,?VIEW_HEIGHT}]).

init(_Opts) ->
    ?verbose("INIT: Opts=~w\n", [_Opts]),
    {ok,Font} = epx_font:match([{size,24}]),
    Ascent = epx:font_info(Font, ascent),
    Self = self(),
    alsa_play:set_callback(
      fun(Samples, Len, Params) ->
	      %% called in alsa_play server context!
	      T = erlang:system_time(milli_seconds),
	      Self ! {samples, T, Samples, Len, Params}
      end),

    Profile = #profile{},

    MProfile = create_menu_profile(Profile),

    DrawFFT = false,
    Menu = epx_menu:create(MProfile#menu_profile{background_color=red},
			   menu(DrawFFT)),

    Search = load_icon("outline_search_black_24dp.png"),
    Copy   = load_icon("outline_content_copy_black_24dp.png"),
    Home   = load_icon("outline_home_black_24dp.png"),

    FFT    = alsa_fft:new(2048),
    alsa_fft:set_hanning(FFT),

    State = 
	#{ font => Font, 
	   ascent => Ascent,
	   mprofile => MProfile,
	   menu => Menu,
	   tools => [Search, Copy, Home],
	   selection => {0,0,0,0},
	   time => erlang:system_time(milli_seconds),
	   freq => 2, %% 1(1Hz),2(10Hz),3(50Hz),
	   draw_fft => DrawFFT,
	   fft => FFT
	 },
    {ok, State}.

load_icon(Filename) ->
    Path = filename:join(code:priv_dir(epx_demo), Filename),
    io:format("load icon [~s]\n", [Path]),
    {ok, Image} = epx_image:load(Path),
    hd(Image#epx_image.pixmaps).


configure(_Rect, State) ->
    ?verbose("CONFIGURE: ~w\n", [_Rect]),
    {Sx,_Sy} = epxw:scale(),
    Scale = float(Sx*100),
    Status = io_lib:format("~wx~w, Scale: ~.2f%",
			   [epxw:width(),epxw:height(),Scale]),
    epxw:set_status_text(Status),
    State.

key_press(_Event, State) ->
    ?verbose("KEY_PRESS: ~w\n", [_Event]),
    State.

key_release(_Event, State) ->
    ?verbose("KEY_RELEASE: ~w\n", [_Event]),
    State.

button_press(_Event, State) ->
    ?verbose("BUTTON_PRESS: ~w\n", [_Event]),
    State.

button_release(_Event, State) ->
    ?verbose("BUTTON_RELEASE: ~w\n", [_Event]),
    State.

enter(_Event, State) ->
    ?verbose("ENTER: ~w\n", [_Event]),
    State.

leave(_Event, State) ->
    ?verbose("LEAVE: ~w\n", [_Event]),
    State.

focus_in(_Event, State) ->
    ?verbose("FOCUS_IN: ~w\n", [_Event]),
    State.

focus_out(_Event, State) ->
    ?verbose("FOCUS_OUT: ~w\n", [_Event]),
    State.

close(State) ->
    ?verbose("CLOSE:\n", []),
    State.

draw(Pixels, _Dirty, State= #{ selection := Selection } ) ->
    ?verbose("DRAW: Rect = ~p\n", [_Dirty]),

    epx_gc:set_fill_style(solid),

    case State of
	#{data := Data, data_len := _Len, data_params := Params} ->
	    Format = maps:get(format, Params, s16_le),
	    Channels = maps:get(channels, Params, 1),
	    _FrameSize = alsa:format_size(Format,Channels),
	    {Frames,Range} = 
		case State of
		    #{ draw_fft := true, fft := FFT } ->
			[Data1|_] = alsa_fft:rfft(FFT, Format, Channels, Data),
			{alsa_util:decode_frame(Data1, float_le),
			 {0,100,20,1000}};
		    _ ->
			{alsa_util:decode_frame(Data, Format),
			 {-1,1,0,10000}}
		end,
	    epx_gc:set_foreground_color(black),
	    draw_frames(Pixels, Channels, ?VIEW_HEIGHT, Frames, Range);
	_ ->
	    ok
    end,
    
    case Selection of
	undefined -> empty;
	{_,_,Sw,Sh} when Sw < 2, Sh < 2 -> empty;
	_ ->
	    epx_gc:set_fill_style(blend),
	    epx_gc:set_fill_color({127,127,127,127}),
	    epx_gc:set_border_color(black),
	    epx_gc:set_border_width(?BORDER),
	    epx:draw_rectangle(Pixels, Selection)
    end,
    State.

%% draw tool bar
draw(left, Pixels, {Xa,Ya,_Wa,_Ha}, State) ->
    Tools = maps:get(tools, State, []),
    lists:foreach(
      fun({I, Png}) ->
	      [{width,W},{height,H}] = epx:pixmap_info(Png, [width, height]),
	      epx:pixmap_add_color_area(Png, Pixels, 255, {0,255,0,0},
					0, 0,
					Xa+2, Ya+I*32,
					W, H)
      end, lists:zip(lists:seq(1,length(Tools)), Tools)),
    State;
draw(_, _Pixels, _Area, State) ->
    State.
    

menu({menu,_Pos}, State=#{ mprofile := MProfile, draw_fft := DrawFFT}) ->
    Menu = epx_menu:create(MProfile#menu_profile{background_color=red},
			   menu(DrawFFT)),
    {reply, Menu, State}.
    
%% NOTE! this is overridden by the select fun above
select({_Phase,Rect}, State) ->
    ?verbose("SELECT: ~w\n", [Rect]),
    State# { selection => Rect }.

motion(_Event={motion,_Button,_Pos}, State) ->
    ?verbose("MOTION: ~w\n", [_Event]),
    State.

command($1, _Mod, State) ->
    {noreply, State#{ freq=>1 }};
command($2, _Mod, State) ->
    {noreply, State#{ freq=>2 }};
command($3, _Mod, State) ->
    {noreply, State#{ freq=>3 }};
command($f, _Mod, State=#{ draw_fft := DrawFFT}) ->
    {noreply, State#{ draw_fft=> not DrawFFT }};
command($F, Mod, State) when not Mod#keymod.shift ->
    {noreply, State#{ draw_fft=>false }};
command(Key, Mod, State) ->
    ?verbose("COMMAND: Key=~w, Mod=~w\n", [Key, Mod]),
    {reply, {Key,Mod}, State}.

handle_info({samples, TimeStamp, Data, NumBytes, Params},
	    State=#{ freq := Freq, time := CurrentTime }) ->
    Td = case Freq of
	     1 -> 1000;
	     2 -> 100;
	     3 -> 20
	 end,
    %% io:format("samples: ~p\n", [{TimeStamp,NumBytes}]),
    if TimeStamp - CurrentTime > Td ->
	    epxw:invalidate(),
	    %% io:format("invalidate curr=~w\n", [CurrentTime]),
	    {noreply, State# { data => Data,
			       data_len => NumBytes,
			       data_params => Params,
			       time => TimeStamp } };
       true ->
	    {noreply, State}
    end;
handle_info(_Info, State) ->
    io:format("handle_info: ~p\n", [_Info]),
    {noreply, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

draw_frames(Pixels, Channels, Height, Frames, Range) ->
    draw_frame_(Pixels, 0, Channels, Height, Frames, Range).

draw_frame_(Pixels, X, Channels, Height, Frames, 
	    _Range={Min,Max,SkipFrames,MaxFrames}) ->
    Chan = center(Channels),
    Frames1 = lists:nthtail(SkipFrames*Channels,Frames),
    Frames2 = lists:sublist(Frames1, MaxFrames*Channels),
    draw_chan(X, Chan, Channels, Frames2,
	      fun(Xi, Yi) ->
		      Yc = min(max(Yi,Min),Max),  %% clip
		      Ys = Yc/(Max-Min),          %% scale
		      Hp = if Min < 0 -> Height div 2;
			      true -> Height
			   end,
		      Y0 = Hp,
		      Yp = Hp*(1-Ys),
		      %% epx:draw_point(Pixels, Xi, H*(1+Yi))
		      epx:draw_line(Pixels, Xi, Y0, Xi, Yp)
	      end).

draw_chan(X, Chan, Channels, Frames, Fun) ->
    draw_chan_(X, Chan, 1, Channels, Channels, Frames, Fun).

draw_chan_(X, I, Chan, N, Channels, [Y|Frames], Fun) ->
    if I < Chan ->
	    draw_chan_(X, I+1, Chan, N-1, Channels, Frames, Fun); 
       true ->
	    Fun(X, Y),
	    draw_chan(X+1, Chan, Channels, lists:nthtail(N, Frames), Fun)
    end;
draw_chan_(_X, _I, _Chan, _N, _Channels, [], _Fun) ->
    ok.

%% select center from num_channels
center(1) -> 1;
center(2) -> 1;
center(3) -> 1;
center(4) -> 1;
center(5) -> 3;
center(6) -> 3;
center(8) -> 3.


create_menu_profile(Profile) ->
    #menu_profile {
       scheme           = Profile#profile.scheme,
       font_name        = Profile#profile.menu_font_name,
       font_size        = Profile#profile.menu_font_size,
       font_color       = Profile#profile.menu_font_color,
       background_color = Profile#profile.menu_background_color,
       border_color     = Profile#profile.menu_border_color
      }.

