-module(paxy).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).
-define(RB, {255,0,255}).
-define(BG, {0,255,255}).
-define(RG, {255,255,0}).
-define(RGB, {255,255,255}).
-define(N, {0,0,0}).
-define(RED2, {128,0,0}).
-define(BLUE2, {0,0,128}).
-define(GREEN2, {0,128,0}).

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  %AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e", "Acceptor f", "Acceptor g", "Acceptor h", "Acceptor i", "Acceptor j", "Acceptor k", "Acceptor l", "Acceptor m"],
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", "Acceptor e"],
  %AccRegister = [a, b, c, d, e, f, g, h, i, j, k, l, m],
  AccRegister = [a, b, c, d, e],
  %ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?RG}, {"Proposer willard", ?BLUE}, {"Proposer x1", ?RB}, {"Proposer x2", ?BG}, {"Proposer x3", ?RG}, {"Proposer x4", ?RGB}, {"Proposer x5", ?N}, {"Proposer x6", ?RED2}, {"Proposer x7", ?BLUE2}, {"Proposer x8", ?GREEN2}],
  ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, {"Proposer willard", ?BLUE}],
  %PropInfo = [{kurtz, ?RED}, {kilgore, ?RG}, {willard, ?BLUE}, {x1, ?RB}, {x2, ?BG}, {x3, ?RG}, {x4, ?RGB}, {x5, ?N}, {x6, ?RED2}, {x7, ?BLUE2}, {x8, ?GREEN2}],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() -> 
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRegister, Sleep, self()),
        wait_proposers(length(PropIds)),
        End = erlang:monotonic_time(),
        Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
        io:format("[Paxy] Total elapsed time: ~w ms~n", [Elapsed])
      end)
  end.
    
start_acceptors(AccIds, AccReg) ->
  case AccIds of
    [] ->
      ok;
    [AccId|Rest] ->
      [RegName|RegNameRest] = AccReg,
      register(RegName, acceptor:start(RegName, AccId)),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main),	
      start_proposers(Rest, RestInfo, Acceptors, RestSleep, Main)
  end.

wait_proposers(0) ->
  ok;
wait_proposers(N) ->
  receive
    done ->
      wait_proposers(N-1)
  end.

stop() ->
  stop(a),
  stop(b),
  stop(c),
  stop(d),
  stop(e),
  %stop(f),
  %stop(g),
  %stop(h),
  %stop(i),
  %stop(j),
  %stop(k),
  %stop(l),
  %stop(m),
  stop(gui).

stop(Name) ->
  case whereis(Name) of
    undefined ->
      ok;
    Pid ->
      Pid ! stop
  end.

 
