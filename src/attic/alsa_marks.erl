%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2022, Tony Rogvall
%%% @doc
%%%    Time marks 
%%% @end
%%% Created :  6 Oct 2022 by Tony Rogvall <tony@rogvall.se>

-module(alsa_marks).

-export([new/0]).
-export([mark/5, mark/6]).
-export([unmark/2]).
-export([set_position/3]).
-export([set_flags/3]).
-export([set_user_data/3]).
-export([range/3]).

-type unsigned() :: non_neg_integer().
-type position() :: unsigned().

-type event_flag() :: 
	notify  | %% send event to mark creator
	once    | %% remove mark when executed
	stop    | %% stop channel after mark is executed
	restart | %% set cur=0 when mark is executed = {set,0} = {set,bof}
	{set,Pos::position()} .

-type event() :: {pid(),Pos::unsigned(),[event_flag()],UserData::term()}.

-record(marks, 
	{
	 tree  :: gb_trees:tree(position(),reference()),
	 events :: #{ reference() =>  event()}
	}).
-type marks() :: #marks{}.


-spec new() -> marks().

new() ->
    #marks { tree = gb_trees:empty(),
	     events = #{}
	   }.

-spec mark(AM::marks(), Pid::pid(), Pos::position(), Flags::[event_flag()],
	   UserData::term()) ->
	  {reference(), AM1::marks()}.
	   
mark(AM, Pid, Pos, Flags, UserData) when is_pid(Pid), is_list(Flags) ->
    mark(AM, Pid, make_ref(), Pos, Flags, UserData).

mark(AM=#marks{tree=Gb,events=Marks}, Pid, Ref, Pos, Flags, UserData)
  when is_pid(Pid), is_reference(Ref), is_list(Flags) ->
    Gb1 = case gb_trees:lookup(Pos, Gb) of
	      none -> 
		  gb_trees:insert(Pos, [Ref], Gb);
	      {value,List0} -> 
		  gb_trees:update(Pos, [Ref|List0], Gb)
	  end,
    Event = {Pid,Pos,Flags,UserData},
    Marks1 = Marks#{ Ref => Event },
    %% io:format("add event ~p to gb=~p\n", [Event, Gb1]),
    {Ref, AM#marks{tree=Gb1, events=Marks1}}.

-spec unmark(AM::marks(), Ref::reference()) -> AM1::marks().
%% remove a mark 
unmark(AM=#marks{tree=Gb,events=Marks}, Ref) 
  when is_reference(Ref) ->
    case maps:get(Ref, Marks, undefined) of
	{_Pid,Pos,_Flags,_EventData} ->
	    Marks1 = maps:remove(Ref, Marks),
	    case gb_trees:lookup(Pos, Gb) of
		none ->
		    AM#marks{events=Marks1};
		{value,List0} ->
		    Gb1 = case lists:delete(Ref,List0) of
			      [] ->
				  gb_trees:delete(Pos, Gb);
			      List1 ->
				  gb_trees:update(Pos, List1, Gb)
			  end,
		    AM#marks{tree=Gb1, events=Marks1}
	    end;
	undefined ->
	    AM
    end.

%%
%% move mark to a new position
%%
-spec set_position(AM::marks(), Ref::reference(), NewPos::position()) ->
	  AM1::marks().

set_position(AM=#marks{tree=Gb,events=Marks},Ref,NewPos) 
  when is_reference(Ref) ->
    case maps:get(Ref, Marks, undefined) of
	{Pid,Pos,Flags,UserData} ->
	    case gb_trees:lookup(Pos, Gb) of
		none ->
		    AM;
		{value,List0} ->
		    Gb1 = case lists:delete(Ref,List0) of
			      [] ->
				  gb_trees:delete(Pos, Gb);
			      List1 ->
				  gb_trees:update(Pos, List1, Gb)
			  end,
		    Gb2 = case gb_trees:lookup(NewPos, Gb1) of
			      none -> 
				  gb_trees:insert(NewPos, [Ref], Gb1);
			      {value,List2} -> 
				  gb_trees:update(NewPos, [Ref|List2], Gb1)
			  end,
		    Event = {Pid,NewPos,Flags,UserData},
		    Marks1 = Marks#{ Ref => Event },
		    AM#marks{tree=Gb2, events=Marks1}
	    end;
	undefined ->
	    AM
    end.

%% set mark flags
-spec set_flags(AM::marks(), Ref::reference(), NewFlags::[event_flag()]) ->
	  AM1::marks().

set_flags(AM=#marks{events=Marks}, Ref, NewFlags) when
      is_reference(Ref), is_list(NewFlags) ->
    case maps:get(Ref, Marks, undefined) of
	{Pid,Pos,_Flags,UserData} ->
	    Event = {Pid,Pos,NewFlags,UserData},
	    Marks1 = Marks#{ Ref => Event },
	    AM#marks{events=Marks1};
	undefined ->
	    AM
    end.

%% set mark user data
-spec set_user_data(AM::marks(), Ref::reference(), UserData::term()) ->
	  AM1::marks().
set_user_data(AM=#marks{events=Marks}, Ref, NewUserData) when
      is_reference(Ref) ->
    case maps:get(Ref, Marks, undefined) of
	{Pid,Pos,Flags,_UserData} ->
	    Event = {Pid,Pos,Flags,NewUserData},
	    Marks1 = Marks#{ Ref => Event },
	    AM#marks{events=Marks1};
	undefined ->
	    AM
    end.

%% return list of marks from and including position From
%% until but NOT including To (To is the location after the region scanned)
-spec range(AM::marks(), From::unsigned(), To::unsigned()) ->
	  [{reference(),event()}].
range(#marks{tree=Gb, events=Marks},
	   From, To) when is_integer(From), is_integer(To), From >= 0,
			  To >= From ->
    Iter = gb_trees:iterator_from(From, Gb),
    case range_(Iter, Marks, To, []) of
	[] -> [];
	RefList -> [{Ref,maps:get(Ref,Marks)} || Ref <- RefList]
    end.

range_(Iter, Marks, To, Acc) ->
    case gb_trees:next(Iter) of
	{Pos, RefList, Iter1} ->
	    if Pos < To ->
		    range_(Iter1, Marks, To, Acc++RefList);
	       true -> Acc
	    end;
	none -> Acc
    end.
