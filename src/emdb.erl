-module(emdb).

-export([
  start/0,
  interfaces/0,
  interface_infos/1,
  add_interface/1,
  add_interface/2,
  is_loaded/1,
  set_language/2,
  set_key/2,
  search/1,
  search/2,
  download_image/2
  ]).

-type error() :: bad_module | {'EXIT', reason()} | term().
-type reason() :: term().
-type name() :: term().
-type id() :: term().
-type num() :: integer().
-type search_term() :: {movie, {name, name()}} | 
                       {movie, {id, id()}} |
                       {tv, {name, name()}} |
                       {tv, {id, id()}} |
                       {season, {tv, id()}, {season, num()}} |
                       {cast, {movie, id()}} |
                       {cast, {tv, id()}} |
                       {cast, {tv, id()}, {season, num()}} |
                       {cast, {tv, id()}, {season, num()}, {episode, num()}} |
                       {person, {name, name()}} |
                       {person, {id, id()}} |
                       {album, {name, name()}} |
                       {album, {id, id()}} |
                       {song, {name, name()}} |
                       {song, {id, id()}}.

% Start emdb
start() ->
  application:start(inets),
  application:ensure_all_started(lager),
  application:start(emdb).

% Return the list of loaded interface
-spec interfaces() -> [atom()].
interfaces() ->
  emdbd_srv:interfaces().

% Return informations for the given interface
-spec interface_infos(atom()) -> not_loaded | list().
interface_infos(Interface) ->
  emdbd_srv:interface_infos(Interface).

% Load a new interface
-spec add_interface(atom()) -> already_loaded | ok | {'EXIT', term()} | term().
add_interface(Interface) ->
  emdbd_srv:add_interface(Interface).

% Load a new interface with the given parameters
-spec add_interface(atom(), list()) -> already_loaded | ok | {'EXIT', reason()} | term().
add_interface(Interface, Params) ->
  emdbd_srv:add_interface(Interface, Params).

% Return true if the given interface is loaded
-spec is_loaded(atom()) -> true | false.
is_loaded(Interface) ->
  emdbd_srv:is_loaded(Interface).

% Set the default language for the given interface
-spec set_language(atom(), atom()) -> term() | {error, error()}.
set_language(Interface, Lang) ->
  emdbd_srv:set_language(Interface, Lang).

% Set the API key for the given interface
-spec set_key(atom(), list()) -> term() | {error, error()}.
set_key(Interface, Key) ->
  emdbd_srv:set_key(Interface, Key).

% Search by name
% Tuple = {movie, {name, Name}} | 
%         {movie, {id, ID}} |
%         {tv, {name, Name}}
%         {tv, {id, ID}}
%         {season, {tv, ID}, {season, Num}}
%         {cast, {movie, ID}}
%         {cast, {tv, ID}}
%         {cast, {tv, ID}, {season, Num}}
%         {cast, {tv, ID}, {season, Num}, {episode, Ep}}
%         {person, {name, Name}}
%         {person, {id, ID}}
%         {album, {name, Name}}
%         {album, {id, ID}}
%         {song, {name, Name}}
%         {song, {id, ID}}
-spec search(search_term()) -> list().
search(Tuple) when is_tuple(Tuple) ->
  search(Tuple, []).

% Search by name with options
-spec search(search_term(), list()) -> list().
search(Tuple, Options) when is_tuple(Tuple), is_list(Options) ->
  [Type|Data] = [element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))],
  emdbd_srv:search(Type, Data, Options).

% Download the URL in File
-spec download_image(list(), list()) -> {ok, saved_to_file} | {error, reason()}.
download_image(Url, File) ->
  lager:debug("Start download ~p to ~p~n",[Url, File]),
  case httpc:request(get, {Url, []}, [{timeout, infinity}], [{stream, File}]) of
    {ok, saved_to_file} -> {ok, self()};
    _ -> {error, self()}
  end.
