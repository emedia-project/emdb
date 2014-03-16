-module(themoviedb).
-behaviour(gen_event).

-include("../include/emdb.hrl").

-define(DEFAULT_ARGS, [
  {base_url, "http://api.themoviedb.org/3"},
  {image_url, "http://cf2.imgobject.com/t/p/original"}
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
  lager:debug("[TMDB] search movie ~p with ~p", [Data, Options]),
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

search_movie([{name, Name}], Options, State) ->
  {BaseURL, ImageURL, RequestParams} = lists:foldl(fun({Key, Value}, {BaseURL1, ImageURL1, RequestParams1}) ->
          case emdb_utils:to_atom(Key) of
            base_url -> {Value, ImageURL1, RequestParams1};
            image_url -> {BaseURL1, Value, RequestParams1};
            key -> {BaseURL1, ImageURL1, [{api_key, Value}] ++ RequestParams1};
            X -> {BaseURL1, ImageURL1, [{X, Value}] ++ RequestParams1}
          end
      end, {undefined, undefined, []}, State ++ emdb_utils:key_add_or_replace(1, Options, [{page, 1}]) ++ [{query, http_uri:encode(Name)}]),
  URL = BaseURL ++ "/search/movie?" ++ emdb_utils:keylist_to_params_string(RequestParams),
  lager:info("[TMDB] GET ~p", [URL]),
  case httpc:request(URL) of 
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
      [
        {<<"page">>, Page},
        {<<"results">>, ResultList},
        {<<"total_pages">>, TotalPages},
        {<<"total_results">>, _TotalResults}
      ] = jsx:decode(list_to_binary(Body)),
      if
        Page < TotalPages -> to_movie(Name, ImageURL, ResultList) ++ search_movie([{name, Name}], emdb_utils:key_add_or_replace(1, [{page, Page + 1}], Options), State);
        true -> to_movie(Name, ImageURL, ResultList)
      end;
    _ -> []
  end;
search_movie([{id, ID}], Options, State) ->
  {BaseURL, ImageURL, RequestParams} = lists:foldl(fun({Key, Value}, {BaseURL1, ImageURL1, RequestParams1}) ->
          case emdb_utils:to_atom(Key) of
            base_url -> {Value, ImageURL1, RequestParams1};
            image_url -> {BaseURL1, Value, RequestParams1};
            key -> {BaseURL1, ImageURL1, [{api_key, Value}] ++ RequestParams1};
            X -> {BaseURL1, ImageURL1, [{X, Value}] ++ RequestParams1}
          end
      end, {undefined, undefined, []}, State ++ Options),
  URL = BaseURL ++ "/movie/" ++ emdb_utils:to_list(ID) ++ "?" ++ emdb_utils:keylist_to_params_string(RequestParams),
  lager:debug("[TMDB] GET ~p", [URL]),
  case httpc:request(URL) of 
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
      to_movie(undefined, ImageURL, [jsx:decode(list_to_binary(Body))]);
    _ -> []
  end.

to_movie(SearchTerm, ImageURL, List) ->
  lists:foldl(fun(Data, AccIn) ->
      Title = proplists:get_value(<<"title">>, Data),
      OriginalTitle = proplists:get_value(<<"original_title">>, Data),
      Distance = case SearchTerm of
        undefined -> 0;
        X -> emdb_utils:distance(X, binary_to_list(Title))
      end,
      OriginalDistance = case SearchTerm of
        undefined -> 0;
        Y -> emdb_utils:distance(Y, binary_to_list(OriginalTitle))
      end,
      PosterURL = case proplists:get_value(<<"poster_path">>, Data) of
        P when is_binary(P) -> ImageURL ++ binary_to_list(P);
        _ -> undefined
      end,
      BackgropURL = case proplists:get_value(<<"backdrop_path">>, Data) of
        B when is_binary(B) -> ImageURL ++ binary_to_list(B);
        _ -> undefined
      end,
      Genres = case proplists:get_value(<<"genres">>, Data) of
        undefined -> [];
        Genres1 -> 
          lists:foldl(fun(Genre, AccIn1) ->
              case lists:keyfind(<<"name">>, 1, Genre) of
                {<<"name">>, GenreName} -> AccIn1 ++ [GenreName];
                _ -> AccIn1
              end
            end, [], Genres1)
      end,
      AccIn ++ [#movie {
        id = proplists:get_value(<<"id">>, Data),
        source = themoviedb,
        title = proplists:get_value(<<"title">>, Data),
        original_title = proplists:get_value(<<"original_title">>, Data),
        adult = proplists:get_value(<<"adult">>, Data),
        date = proplists:get_value(<<"release_date">>, Data),
        poster = PosterURL,
        backdrop = BackgropURL,
        genres = Genres,
        tagline = proplists:get_value(<<"tagline">>, Data),
        overview = proplists:get_value(<<"overview">>, Data),
        distance = min(Distance, OriginalDistance)
      }]
    end, [], List).
