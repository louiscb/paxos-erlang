-module(proposer).
-export([start/6]).

-define(timeout, 2000).
-define(backoff, 10).

start(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  spawn(fun() -> init(Name, Proposal, Acceptors, Sleep, PanelId, Main) end).

init(Name, Proposal, Acceptors, Sleep, PanelId, Main) ->
  timer:sleep(Sleep),
  Begin = erlang:monotonic_time(),
  Round = order:first(Name),
  {Decision, LastRound} = round(Name, ?backoff, Round, Proposal, Acceptors, PanelId),
  End = erlang:monotonic_time(),
  Elapsed = erlang:convert_time_unit(End-Begin, native, millisecond),
  io:format("[Proposer ~w] DECIDED ~w in round ~w after ~w ms~n",
    [Name, Decision, LastRound, Elapsed]),
  Main ! done,
  PanelId ! stop.

round(Name, Backoff, Round, Proposal, Acceptors, PanelId) ->
  io:format("[Proposer ~w] Phase 1: round ~w proposal ~w~n",
    [Name, Round, Proposal]),
  % Update gui
  PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Proposal},
  case ballot(Name, Round, Proposal, Acceptors, PanelId) of
    {ok, Value} ->
      {Value, Round};
    abort ->
      timer:sleep(rand:uniform(Backoff)),
      Next = order:inc(Round),
      round(Name, (2*Backoff), Next, Proposal, Acceptors, PanelId)
  end.

ballot(Name, Round, Proposal, Acceptors, PanelId) ->
  prepare(Round, Acceptors),
  Quorum = (length(Acceptors) div 2) + 1,
  MaxVoted = order:null(),
  %Maybe len/2+1: yes, should be better. Otherwise we might waste time
  case collect(Quorum, Round, MaxVoted, Proposal, 0) of
    {accepted, Value} ->
      io:format("[Proposer ~w] Phase 2: round ~w proposal ~w (was ~w)~n",
        [Name, Round, Value, Proposal]),
      % update gui
      PanelId ! {updateProp, "Round: " ++ io_lib:format("~p", [Round]), Value},
      accept(Round, Value, Acceptors),
      case vote(Quorum, Round, 0) of
        ok ->
          {ok, Value};
        abort ->
          abort
      end;
    abort ->
      abort
  end.

collect(0, _, _, Proposal, _) ->
  {accepted, Proposal};
collect(_, _, _, _, 3) ->
  abort;
collect(N, Round, MaxVoted, Proposal, RejectedPromises) ->
  receive
    {promise, Round, _, na} ->
      collect(N-1, Round, MaxVoted, Proposal, RejectedPromises);
    {promise, Round, Voted, Value} ->
      case order:gr(Voted, MaxVoted) of
        true ->
          collect(N-1, Round, Voted, Value, RejectedPromises);
        false ->
          collect(N-1, Round, MaxVoted, Proposal, RejectedPromises)
      end;
    {promise, _, _,  _} ->
      collect(N, Round, MaxVoted, Proposal, RejectedPromises);
    {sorry, {prepare, Round}} ->
      collect(N, Round, MaxVoted, Proposal, RejectedPromises+1);
    {sorry, _} ->
      collect(N, Round, MaxVoted, Proposal, RejectedPromises)
  after ?timeout ->
    abort
  end.

vote(0, _, _) ->
  ok;
vote(_, _, 3) ->
  abort;
vote(N, Round, RejectedVotes) ->
  receive
    {vote, Round} ->
      vote(N-1, Round, RejectedVotes);
    {vote, _} ->
      vote(N, Round, RejectedVotes);
    {sorry, {accept, Round}} ->
      vote(N, Round, RejectedVotes+1);
    {sorry, _} ->
      vote(N, Round, RejectedVotes)
  after ?timeout ->
    abort
  end.

prepare(Round, Acceptors) ->
  Fun = fun(Acceptor) ->
    send(Acceptor, {prepare, self(), Round})
        end,
  lists:foreach(Fun, Acceptors).

accept(Round, Proposal, Acceptors) ->
  Fun = fun(Acceptor) ->
    send(Acceptor, {accept, self(), Round, Proposal})
        end,
  lists:foreach(Fun, Acceptors).

send(Name, Message) ->
  Name ! Message.
