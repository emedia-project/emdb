-module(omdb).
-behaviour(gen_event).

-include("../include/emdb.hrl").

-define(DEFAULT_ARGS, [
  {base_url, "http://www.omdbapi.com"},
  {r, json},
  {plot, full}
]).

-export([
  init/1,
  handle_event/2,
  handle_call/2,
  handle_info/2,
  code_change/3,
  terminate/2
]).

init(Args) ->
  Args1 = emdb_utils:key_add_or_replace(1, Args, ?DEFAULT_ARGS),
  {ok, Args1}.

handle_event(_Request, State) ->
  {ok, State}.

handle_call(info, State) ->
  {ok, State, State};
handle_call({language, Lang}, State) ->
  {ok, ok, lists:keystore(language, 1, State, {language, Lang})};
handle_call({key, Key}, State) ->
  {ok, ok, lists:keystore(key, 1, State, {key, Key})};
handle_call({search, movie, Data, Options}, State) ->
  lager:info("[OMDB] search movie ~p with ~p", [Data, Options]),
  {ok, search_movie(Data, Options, State), State};
handle_call({search, tv, Data, Options}, State) ->
  lager:info("[omdb] search tv ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, season, Data, Options}, State) ->
  lager:info("[omdb] search season ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, cast, Data, Options}, State) ->
  lager:info("[omdb] search cast ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, person, Data, Options}, State) ->
  lager:info("[omdb] search person ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, album, Data, Options}, State) ->
  lager:info("[omdb] search album ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, song, Data, Options}, State) ->
  lager:info("[omdb] search song ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call(_Request, State) ->
  {ok, not_available, State}.

handle_info(_Info, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Args, State) ->
  file:close(State).

% Private

search_movie([{name, Name}], Options, State) ->
  {BaseURL, RequestParams} = lists:foldl(fun({Key, Value}, {BaseURL1, RequestParams1}) ->
          case emdb_utils:to_atom(Key) of
            base_url -> {Value, RequestParams1};
            X -> {BaseURL1, [{X, Value}] ++ RequestParams1}
          end
      end, {undefined, []}, State ++ Options ++ [{s, http_uri:encode(Name)}]),
  URL = BaseURL ++ "?" ++ emdb_utils:keylist_to_params_string(RequestParams),
  lager:info("[OMDB] GET ~p", [URL]),
  case httpc:request(URL) of
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
      [{<<"Search">>, ResultList}] = jsx:decode(list_to_binary(Body)),
      to_movie(Name, ResultList);
    _ -> []
  end;
search_movie([{id, ID}], Options, State) ->
  {BaseURL, RequestParams} = lists:foldl(fun({Key, Value}, {BaseURL1, RequestParams1}) ->
          case emdb_utils:to_atom(Key) of
            base_url -> {Value, RequestParams1};
            X -> {BaseURL1, [{X, Value}] ++ RequestParams1}
          end
      end, {undefined, []}, State ++ Options ++ [{i, ID}]),
  URL = BaseURL ++ "?" ++ emdb_utils:keylist_to_params_string(RequestParams),
  lager:info("[OMDB] GET ~p", [URL]),
  case httpc:request(URL) of
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
      to_movie(undefined, [jsx:decode(list_to_binary(Body))]);
    _ -> []
  end.

to_movie(SearchTerm, List) ->
  lists:foldl(fun(Data, AccIn) ->
        Title = proplists:get_value(<<"Title">>, Data),
        Distance = case SearchTerm of
          undefined -> 0;
          X -> emdb_utils:distance(X, binary_to_list(Title))
        end,
        Genres = case proplists:get_value(<<"Genre">>, Data) of
          undefined -> [];
          G -> [G]
        end,
        Poster = case proplists:get_value(<<"Poster">>, Data) of
          P when is_binary(P) -> binary_to_list(P);
          _ -> undefined
        end,
        case proplists:get_value(<<"Type">>, Data) of
          <<"movie">> ->
            AccIn ++ [#movie {
              id = proplists:get_value(<<"imdbID">>, Data),
              source = omdb,
              title = Title,
              original_title = Title,
              adult = undefined,
              date = proplists:get_value(<<"Released">>, Data),
              poster = Poster,
              genres = Genres,
              tagline = proplists:get_value(<<"Plot">>, Data),
              overview = proplists:get_value(<<"Plot">>, Data),
              distance = Distance
            }];
          _ -> AccIn
        end
    end, [], List).
