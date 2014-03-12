-module(themoviedb).
-behaviour(gen_event).

-define(DEFAULT_ARGS, [
  {base_url, "http://api.themoviedb.org/3"},
  {image_url, "http://cf2.imgobject.com/t/p"}
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
  Args1 = lists:keysort(1, Args),
  Args2 = lists:keymerge(1, Args1, ?DEFAULT_ARGS),
  {ok, Args2}.

handle_event(_Request, State) ->
  {ok, State}.

handle_call(info, State) ->
  {ok, State, State};
handle_call({language, Lang}, State) ->
  {ok, ok, lists:keystore(language, 1, State, {language, Lang})};
handle_call({key, Key}, State) ->
  {ok, ok, lists:keystore(key, 1, State, {key, Key})};
handle_call({search, movie, Data, Options}, State) ->
  lager:info("[themoviedb] search movie ~p with ~p", [Data, Options]),
  {ok, search_movie(Data, Options, State), State};
handle_call({search, tv, Data, Options}, State) ->
  lager:info("[themoviedb] search tv ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, season, Data, Options}, State) ->
  lager:info("[themoviedb] search season ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, cast, Data, Options}, State) ->
  lager:info("[themoviedb] search cast ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, person, Data, Options}, State) ->
  lager:info("[themoviedb] search person ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, album, Data, Options}, State) ->
  lager:info("[themoviedb] search album ~p with ~p", [Data, Options]),
  {ok, not_available, State};
handle_call({search, song, Data, Options}, State) ->
  lager:info("[themoviedb] search song ~p with ~p", [Data, Options]),
  {ok, not_available, State};
handle_call(_Request, State) ->
  {ok, not_available, State}.

handle_info(_Info, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Args, State) ->
  file:close(State).

% Private

search_movie({name, Name}, Options, State) ->
  {BaseURL, RequestParams} = lists:foldl(fun({Key, Value}, {BaseURL1, RequestParams1}) ->
          case emdbd_utils:to_atom(Key) of
            base_url -> {Value, RequestParams1};
            image_url -> {BaseURL1, RequestParams1};
            key -> {BaseURL1, [{api_key, Value}] ++ RequestParams1};
            X -> {BaseURL1, [{X, Value}] ++ RequestParams1}
          end
      end, {undefined, []}, State ++ Options ++ [{query, http_uri:encode(Name)}]),
  URL = BaseURL ++ "/search/movie?" ++ emdbd_utils:keylist_to_params_string(RequestParams),
  lager:info("[TMDB] Request = ~p", [URL]),
  case httpc:request(URL) of 
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
      Body;
    _ -> []
  end;
search_movie({id, _ID}, _Options, _State) ->
  [].
