-module(paxy).
-export([start/1, stop/0, stop/1]).

-define(RED, {255,0,0}).
-define(BLUE, {0,0,255}).
-define(GREEN, {0,255,0}).

-define(paxy_acc_node, 'acc@philipp-hpprobook430g4').
-define(paxy_pro_node, 'pro@philipp-hpprobook430g4').

get_remote_info(RegNames, HostName) ->
  case RegNames of
    [] ->
      [];
    [RegName|Rest] ->
      [{RegName, HostName} | get_remote_info(Rest, HostName)]
  end.

% Sleep is a list with the initial sleep time for each proposer
start(Sleep) ->
  AcceptorNames = ["Acceptor a", "Acceptor b", "Acceptor c", "Acceptor d", 
                   "Acceptor e"],
  AccRegister = [a, b, c, d, e],
  AccRemoteInfos = get_remote_info(AccRegister, ?paxy_acc_node),
  ProposerNames = [{"Proposer kurtz", ?RED}, {"Proposer kilgore", ?GREEN}, 
                   {"Proposer willard", ?BLUE}],
  PropInfo = [{kurtz, ?RED}, {kilgore, ?GREEN}, {willard, ?BLUE}],
  register(gui, spawn(fun() -> gui:start(AcceptorNames, ProposerNames) end)),
  gui ! {reqState, self()},
  receive
    {reqState, State} ->
      {AccIds, PropIds} = State,
      start_acceptors(AccIds, AccRegister),
      spawn(fun() -> 
        Begin = erlang:monotonic_time(),
        start_proposers(PropIds, PropInfo, AccRemoteInfos, Sleep, self()),
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
      spawn(?paxy_acc_node, fun() -> register(RegName, acceptor:start(RegName, AccId)) end),
      start_acceptors(Rest, RegNameRest)
  end.

start_proposers(PropIds, PropInfo, Acceptors, Sleep, Main) ->
  case PropIds of
    [] ->
      ok;
    [PropId|Rest] ->
      [{RegName, Colour}|RestInfo] = PropInfo,
      [FirstSleep|RestSleep] = Sleep,
      spawn(?paxy_pro_node, fun() -> register(RegName, proposer:start(RegName, Colour, Acceptors, FirstSleep, PropId, Main)) end),
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
  stop({a, ?paxy_acc_node}),
  stop({b, ?paxy_acc_node}),
  stop({c, ?paxy_acc_node}),
  stop({d, ?paxy_acc_node}),
  stop({e, ?paxy_acc_node}),
  stop(gui).

stop(Name) ->
  case Name of
    {RegName, Node} -> 
      {RegName, Node} ! stop;
    _ ->
      case whereis(Name) of
        undefined ->
          ok;
        Pid ->
          Pid ! stop
      end
  end.

