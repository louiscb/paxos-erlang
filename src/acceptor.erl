-module(acceptor).
-export([start/2]).

start(Name, PanelId) ->
  spawn(fun() -> init(Name, PanelId) end).

init(Name, PanelId) ->
  case filelib:is_regular(Name) of
    true ->
      pers:open(Name),
      {Promised, Voted, Value, PanelIdOriginal} = pers:read(Name),
      pers:close(Name),
      Colour = case Value of na -> {0,0,0}; _ -> Value end,
      PanelIdOriginal !
        {updateAcc, "Voted: " ++ io_lib:format("~p", [Promised]), "Promised: " ++ io_lib:format("~p", [Promised]), Colour},
      acceptor(Name, Promised, Voted, Value, PanelIdOriginal);
    false ->
      Promised = order:null(),
      Voted = order:null(),
      Value = na,
      acceptor(Name, Promised, Voted, Value, PanelId)
  end.

acceptor(Name, Promised, Voted, Value, PanelId) ->
  receive
    {prepare, Proposer, Round} ->
      case order:gr(Round, Promised) of
        true ->
          Proposer ! {promise, Round, Voted, Value},

          % Persist the state in case of crashes
          save_state(Name, Round, Voted, Value, PanelId),

          io:format("[Acceptor ~w] Phase 1: promised ~w voted ~w colour ~w~n",
            [Name, Round, Voted, Value]),
          % Update gui
          Colour = case Value of na -> {0,0,0}; _ -> Value end,
          PanelId ! {updateAcc, "Voted: " ++ io_lib:format("~p", [Voted]),
              "Promised: " ++ io_lib:format("~p", [Round]), Colour},
          acceptor(Name, Round, Voted, Value, PanelId);
        false ->
          Proposer ! {sorry, {prepare, Round}},
          % It is wrong to pass Round here as it is smaller than Promised
          % acceptor(Name, Round, Voted, Value, PanelId)
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    {accept, Proposer, Round, Proposal} ->
      case order:goe(Round, Promised) of
        true ->
          Proposer ! {vote, Round},
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
          Proposer ! {sorry, {accept, Round}},
          acceptor(Name, Promised, Voted, Value, PanelId)
      end;
    stop ->
      PanelId ! stop,
      delete_state(Name),
      ok
  end.

save_state(Name, Round, Voted, Value, PanelId) ->
  pers:open(Name),
  pers:store(Name, Round, Voted, Value, PanelId),
  pers:close(Name).

delete_state(Name) ->
  pers:delete(Name).
