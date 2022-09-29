-module(acceptor).
-export([start/2]).

-define(response_delay, 2000).
-define(response_delay_enabled, false).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  Promised = order:null(),
  Voted = order:null(),
  Value = na,
  acceptor(Name, Promised, Voted, Value, PanelId).

send_with_delay(Pid, Message) ->
  T = rand:uniform(?response_delay),
  timer:send_after(T, Pid, Message).

send_message_to_proposer(Pid, Message) ->
  case ?response_delay_enabled of
    true ->
      send_with_delay(Pid, Message);
    false ->
      Pid ! Message
  end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
	  send_message_to_proposer(Proposer, {promise, Round, Voted, Value}),
          io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
            [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
              "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
	  send_message_to_proposer(Proposer, {sorry, {prepare, Round}}),
	  % It is wrong to pass Round here as it is smaller than Promised
	  % acceptor(Name, Round, Voted, Value, PanelId)
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
	  send_message_to_proposer(Proposer, {vote, Round}),
          case order:goe(Round, Voted) of
            true ->
              io:format("[Acceptor ~w] Phase 2: promised ~w voted ~w colour ~w~n",
                [Name, Promised, Round, Proposal]),
              % Update gui
              PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Round]),
                  "Promised: " ++ io_lib:format("~p", [Promised]), Proposal},
              acceptor(Name, Promised, Round, Proposal, PanelId);
            false ->
              acceptor(Name, Promised, Voted, Value, PanelId)
          end;
        false ->
	  send_message_to_proposer(Proposer, {sorry, {accept, Round}}),
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      ok
  end.
