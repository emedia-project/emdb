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

% Start emdb
start() ->
  application:start(inets),
  application:ensure_all_started(lager),
  application:start(emdb).

% Return the list of loaded interface
interfaces() ->
  emdbd_srv:interfaces().

% Return informations for the given interface
interface_infos(Interface) ->
  emdbd_srv:interface_infos(Interface).

% Load a new interface
add_interface(Interface) ->
  emdbd_srv:add_interface(Interface).

% Load a new interface with the given parameters
add_interface(Interface, Params) ->
  emdbd_srv:add_interface(Interface, Params).

% Return true if the given interface is loaded
is_loaded(Interface) ->
  emdbd_srv:is_loaded(Interface).

% Set the default language for the given interface
set_language(Interface, Lang) ->
  emdbd_srv:set_language(Interface, Lang).

% Set the API key for the given interface
set_key(Interface, Lang) ->
  emdbd_srv:set_key(Interface, Lang).

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
search(Tuple) when is_tuple(Tuple) ->
  search(Tuple, []).

% Search by name with options
search(Tuple, Options) when is_tuple(Tuple), is_list(Options) ->
  [Type|Data] = [element(I,Tuple) || I <- lists:seq(1,tuple_size(Tuple))],
  emdbd_srv:search(Type, Data, Options).

% Download the URL in File
download_image(Url, File) ->
  lager:debug("Start download ~p to ~p~n",[Url, File]),
  case httpc:request(get, {Url, []}, [{timeout, infinity}], [{stream, File}]) of
    {ok, saved_to_file} -> {ok, self()};
    _ -> {error, self()}
  end.
