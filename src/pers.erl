-module(pers).
-export([open/1, read/1, store/5, close/1, delete/1]).

%% dets module provides term storage on file

open(Name) ->
    dets:open_file(Name, []).

%% returns the object with the key 'perm' stored in the table 'Name'
read(Name) ->
    case dets:lookup(Name, perm) of
        [{perm, Promised, Voted, Value, PanelId}] ->
            {Promised, Voted, Value, PanelId};
        [] ->
            {order:null(), order:null(), na, na}
    end.

%% inserts one object {Pr, Vt, Ac, Pn} into the table 'Name'
store(Name, Promised, Voted, Value, PanelId)->
    dets:insert(Name, {perm, Promised, Voted, Value, PanelId}).

close(Name) ->
    dets:close(Name).

delete(Name) ->
    file:delete(Name).


