-module(emdbd_srv).

-export([
  start_link/0,
  interfaces/0,
  interface_infos/1,
  set_language/2,
  set_key/2,
  search/2,
  add_interface/1,
  add_interface/2,
  is_loaded/1
]).

start_link() ->
  R = gen_event:start({local, emdbd_manager}),
  {ok, LH} = application:get_env(emdbd, emdbi),
  [start_interface(H) || H <- LH],
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
%   * {language, Lang} : usgae language Lang
search(Name, Options) ->
  {Interfaces1, Options1} = case lists:keytake(only, 1, Options) of
    {value, {only, Interfaces}, Rest} -> {Interfaces, Rest};
    false -> {interfaces(), Options}
  end,
  do_search(Interfaces1, Name, Options1, []).

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

do_search([], _, _, Results) ->
  Results;
do_search([Interface|Rest], Name, Options, Results) ->
  Results1 = case is_loaded(Interface) of
    true -> Results ++ gen_event:call(emdbd_manager, Interface, {search, Name, Options});
    flase -> Results
  end,
  do_search(Rest, Name, Options, Results1).

is_loaded(Interface) ->
  lists:any(fun(E) -> E =:= Interface end, interfaces()).
