-module(emdbd_srv).

-include("../include/emdb.hrl").

-export([
  start_link/0,
  interfaces/0,
  interface_infos/1,
  set_language/2,
  set_key/2,
  search/3,
  add_interface/1,
  add_interface/2,
  is_loaded/1
]).

start_link() ->
  R = gen_event:start({local, emdbd_manager}),
  case application:get_env(emdb, emdbi) of
    {ok, LH} -> [start_interface(H) || H <- LH];
    _ -> lager:info("[emdbd] No interface started!")
  end,
  R.

interfaces() ->
  gen_event:which_handlers(emdbd_manager).

interface_infos(Interface) ->
  case is_loaded(Interface) of
    true -> gen_event:call(emdbd_manager, Interface, info);
    false -> not_loaded
  end.

set_language(Interface, Lang) ->
  gen_event:call(emdbd_manager, Interface, {language, Lang}).

set_key(Interface, Key) ->
  gen_event:call(emdbd_manager, Interface, {key, Key}).

% Options :
%   * {only, [Interfaces]} : use ontly interfaces in list (will be used in the given order)
%   * {distance, lt, N} | {distance, gt, M} | {distance, in, {N, M}} : Keep only distances
%   * {language, Lang} : usgae language Lang
search(Type, Data, Options) ->
  {FetchDistance, Options1} = case lists:keytake(distance, 1, Options) of
    {value, Tuple, List} -> {Tuple, List};
    false -> {false, Options}
  end,
  {Interfaces1, Options2} = case lists:keytake(only, 1, Options1) of
    {value, {only, Interfaces}, Rest} -> {Interfaces, Rest};
    false -> {interfaces(), Options1}
  end,
  Results = do_search(Interfaces1, Type, Data, Options2, []),
  filter_distance(FetchDistance, Results).

add_interface(Interface) ->
  add_interface(Interface, []).

add_interface(Interface, Params) ->
  case is_loaded(Interface) of
    true -> 
      lager:info("Interface ~p already loaded", [Interface]),
      already_loaded;
    false ->
      lager:debug("Start interface ~p", [Interface]),
      gen_event:add_handler(emdbd_manager, Interface, Params)
  end.

start_interface({Module, Params}) ->
  add_interface(Module, Params).

do_search([], _, _, _, Results) ->
  Results;
do_search([Interface|Rest], Type, Data, Options, Results) ->
  Results1 = case is_loaded(Interface) of
    true -> Results ++ gen_event:call(emdbd_manager, Interface, {search, Type, Data, Options});
    false -> Results
  end,
  do_search(Rest, Type, Data, Options, Results1).

is_loaded(Interface) ->
  lists:any(fun(E) -> E =:= Interface end, interfaces()).

% Private

filter_distance(false, Data) ->
  Data;
filter_distance({distance, Op, Value}, Data) ->
  lists:foldl(fun(Movie, AccIn) ->
        #movie{rank = Rank} = Movie,
        Keep = case Op of
          eq -> Rank =:= Value;
          lt -> Rank < Value;
          le -> Rank =< Value;
          gt -> Rank > Value;
          ge -> Rank >= Value;
          in -> 
            {Min, Max} = Value,
            (Rank >= Min) and (Rank =< Max)
        end,
        case Keep of
          true -> AccIn ++ [Movie];
          false -> AccIn
        end
    end, [], Data).
