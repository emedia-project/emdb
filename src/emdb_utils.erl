-module(emdb_utils).

-export([
  to_atom/1,
  to_list/1,
  keylist_to_params_string/1,
  distance/2,
  levenshtein/2,
  key_add_or_replace/3,
  is_keylist/1
]).

to_atom(X) when is_atom(X) ->
  X;
to_atom(X) when is_binary(X); is_bitstring(X) ->
  binary_to_atom(X, utf8);
to_atom(X) when is_list(X) ->
  list_to_atom(X).

to_list(V) when is_atom(V) ->
  atom_to_list(V);
to_list(V) when is_list(V) ->
  V;
to_list(V) when is_integer(V) ->
  integer_to_list(V);
to_list(true) ->
  "true";
to_list(false) ->
  "false".

keylist_to_params_string(Dict) ->
  join_string(
    lists:foldl(
      fun({Key, Value}, Result) -> 
          Result ++ [to_list(Key) ++ "=" ++ to_list(Value)] 
      end, 
      [], 
      Dict
    ),
    "&"
  ).

join_string(Items, Sep) ->
  lists:flatten(lists:reverse(join_string1(Items, Sep, []))).
join_string1([Head | []], _Sep, Acc) ->
  [Head | Acc];
join_string1([Head | Tail], Sep, Acc) ->
  join_string1(Tail, Sep, [Sep, Head | Acc]).

distance(A, B) ->
  LA = string:to_lower(A),
  LB = string:to_lower(B),
  SLA = re:replace(LA, "[^0-9a-z]", "", [global, {return, list}]),
  SLB = re:replace(LB, "[^0-9a-z]", "", [global, {return, list}]),
  levenshtein(SLA, SLB).

levenshtein(Samestring, Samestring) -> 0;
levenshtein(String, []) -> length(String);
levenshtein([], String) -> length(String);
levenshtein(Source, Target) ->
  levenshtein_rec(Source, Target, lists:seq(0, length(Target)), 1).

%% Recurses over every character in the source string and calculates a list of distances
levenshtein_rec([SrcHead|SrcTail], Target, DistList, Step) ->
  levenshtein_rec(SrcTail, Target, levenshtein_distlist(Target, DistList, SrcHead, [Step], Step), Step + 1);
levenshtein_rec([], _, DistList, _) ->
  lists:last(DistList).

%% Generates a distance list with distance values for every character in the target string
levenshtein_distlist([TargetHead|TargetTail], [DLH|DLT], SourceChar, NewDistList, LastDist) when length(DLT) > 0 ->
  Min = lists:min([LastDist + 1, hd(DLT) + 1, DLH + dif(TargetHead, SourceChar)]),
  levenshtein_distlist(TargetTail, DLT, SourceChar, NewDistList ++ [Min], Min);
levenshtein_distlist([], _, _, NewDistList, _) ->
  NewDistList.

% Calculates the difference between two characters or other values
dif(C, C) -> 0;
dif(_, _) -> 1.

key_add_or_replace(_, [], TupleList2) ->
  TupleList2;
key_add_or_replace(N, [Tuple|Rest], TupleList2) ->
  Key = element(N, Tuple),
  TupleList3 = case lists:keysearch(Key, N, TupleList2) of
    {value, _} -> lists:keydelete(Key, N, TupleList2);
    false -> TupleList2
  end,
  key_add_or_replace(N, Rest, TupleList3 ++ [Tuple]).

is_keylist(L) when is_list(L) ->
  lists:all(fun(E) -> is_tuple(E) end, L).
