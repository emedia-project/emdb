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
  lager:debug("[TMDB] search tv ~p with ~p", [Data, Options]),
  {ok, search_tv(Data, Options, State), State};
handle_call({search, season, Data, Options}, State) ->
  lager:debug("[TMDB] search season ~p with ~p", [Data, Options]),
  {ok, search_season(Data, Options, State), State};
handle_call({search, episode, Data, Options}, State) ->
  lager:info("[TMDB] search episode ~p with ~p", [Data, Options]),
  {ok, search_episode(Data, Options, State), State};
handle_call({search, cast, Data, Options}, State) ->
  lager:info("[themoviedb] search cast ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, person, Data, Options}, State) ->
  lager:info("[themoviedb] search person ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, album, Data, Options}, State) ->
  lager:debug("[TMDB] search album ~p with ~p", [Data, Options]),
  {ok, [], State};
handle_call({search, song, Data, Options}, State) ->
  lager:debug("[TMDB] search song ~p with ~p", [Data, Options]),
  {ok, [], State};
handle_call(_Request, State) ->
  {ok, [], State}.

handle_info(_Info, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Args, State) ->
  file:close(State).

% Private

% -- Movie --

search_movie([{name, Name}], Options, State) ->
  {BaseURL, ImageURL, RequestParams} = parse_options(
    State ++ 
    emdb_utils:key_add_or_replace(1, Options, [{page, 1}]) ++ 
    [{query, http_uri:encode(Name)}]
  ),
  URL = BaseURL ++ "/search/movie?" ++ emdb_utils:keylist_to_params_string(RequestParams),
  lager:debug("[TMDB] GET ~p", [URL]),
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
  {BaseURL, ImageURL, RequestParams} = parse_options(
    State ++ Options
  ),
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

% -- TV --

search_tv([{name, Name}], Options, State) ->
  {BaseURL, ImageURL, RequestParams} = parse_options(
    State ++ 
    emdb_utils:key_add_or_replace(1, Options, [{page, 1}]) ++ 
    [{query, http_uri:encode(Name)}]
  ),
  URL = BaseURL ++ "/search/tv?" ++ emdb_utils:keylist_to_params_string(RequestParams),
  lager:debug("[TMDB] GET ~p", [URL]),
  case httpc:request(URL) of 
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
      [
        {<<"page">>, Page},
        {<<"results">>, ResultList},
        {<<"total_pages">>, TotalPages},
        {<<"total_results">>, _TotalResults}
      ] = jsx:decode(list_to_binary(Body)),
      if
        Page < TotalPages -> to_tv(Name, ImageURL, ResultList) ++ search_tv([{name, Name}], emdb_utils:key_add_or_replace(1, [{page, Page + 1}], Options), State);
        true -> to_tv(Name, ImageURL, ResultList)
      end;
    _ -> []
  end;
search_tv([{id, ID}], Options, State) ->
  {BaseURL, ImageURL, RequestParams} = parse_options(
    State ++ Options
  ),
  URL = BaseURL ++ "/tv/" ++ emdb_utils:to_list(ID) ++ "?" ++ emdb_utils:keylist_to_params_string(RequestParams),
  lager:debug("[TMDB] GET ~p", [URL]),
  case httpc:request(URL) of 
    {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
      to_tv(undefined, ImageURL, [jsx:decode(list_to_binary(Body))]);
    _ -> []
  end.

to_tv(SearchTerm, ImageURL, Result) ->
  lists:foldl(fun(TV, AccIn) ->
      Title = proplists:get_value(<<"name">>, TV),
      OriginalTitle = proplists:get_value(<<"original_name">>, TV),
      Distance = case SearchTerm of
        undefined -> 0;
        X -> emdb_utils:distance(X, binary_to_list(Title))
      end,
      OriginalDistance = case SearchTerm of
        undefined -> 0;
        Y -> emdb_utils:distance(Y, binary_to_list(OriginalTitle))
      end,
      PosterURL = case proplists:get_value(<<"poster_path">>, TV) of
        P when is_binary(P) -> ImageURL ++ binary_to_list(P);
        _ -> undefined
      end,
      BackgropURL = case proplists:get_value(<<"backdrop_path">>, TV) of
        B when is_binary(B) -> ImageURL ++ binary_to_list(B);
        _ -> undefined
      end,
      Genres = case proplists:get_value(<<"genres">>, TV) of
        undefined -> [];
        Genres1 -> 
          lists:foldl(fun(Genre, AccIn1) ->
              case lists:keyfind(<<"name">>, 1, Genre) of
                {<<"name">>, GenreName} -> AccIn1 ++ [GenreName];
                _ -> AccIn1
              end
            end, [], Genres1)
      end,
      Networks = case proplists:get_value(<<"networks">>, TV) of
        undefined -> [];
        Networks1 -> 
          lists:foldl(fun(Network, AccIn2) ->
              case lists:keyfind(<<"name">>, 1, Network) of
                {<<"name">>, NetworkName} -> AccIn2 ++ [NetworkName];
                _ -> AccIn2
              end
            end, [], Networks1)
      end,
      AccIn ++ [#tv {
          id = proplists:get_value(<<"id">>, TV),
          source = themoviedb,
          title = Title,
          original_title = OriginalTitle,
          adult = undefined, %% NA
          date = proplists:get_value(<<"first_air_date">>, TV),
          poster = PosterURL,
          backdrop = BackgropURL,
          genres = Genres,
          tagline = undefined, %% NA
          overview = proplists:get_value(<<"overview">>, TV),
          networks = Networks,
          seasons = proplists:get_value(<<"number_of_seasons">>, TV),
          episodes = proplists:get_value(<<"number_of_episodes">>, TV),
          distance = min(Distance, OriginalDistance)
        }]
    end, [], Result).

% -- Season --

search_season(Data, Options, State) ->
  case lists:keyfind(tv, 1, Data) of
    {tv, TVID} -> 
      case lists:keyfind(season, 1, Data) of
        {season, SeasonNum} -> 
          {BaseURL, ImageURL, RequestParams} = parse_options(
            State ++ Options
          ),
          URL = BaseURL ++ "/tv/" ++ emdb_utils:to_list(TVID) ++ "/season/" ++ emdb_utils:to_list(SeasonNum) ++ "?" ++ emdb_utils:keylist_to_params_string(RequestParams),
          lager:debug("[TMDB] GET ~p", [URL]),
          case httpc:request(URL) of 
            {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
              to_season(TVID, SeasonNum, ImageURL, [jsx:decode(list_to_binary(Body))]);
            _ -> []
          end;
        _ -> []
      end;
    _ -> []
  end.

to_season(TVID, Num, ImageURL, Results) ->
  lists:foldl(fun(Season, AccIn) ->
        PosterURL = case proplists:get_value(<<"poster_path">>, Season) of
          P when is_binary(P) -> ImageURL ++ binary_to_list(P);
          _ -> undefined
        end,
        Episodes = case proplists:get_value(<<"episodes">>, Season) of
          E when is_list(E) -> length(E);
          _ -> 0
        end,
        AccIn ++ [#season{
            id = proplists:get_value(<<"id">>, Season),
            tv_id = TVID,
            source = themoviedb,
            title = proplists:get_value(<<"name">>, Season),
            original_title = undefined,
            date = proplists:get_value(<<"air_date">>, Season),
            poster = PosterURL,
            backdrop = undefined,
            overview = proplists:get_value(<<"overview">>, Season),
            season = Num,
            episodes = Episodes
          }]
    end, [], Results).

% -- Episode --

search_episode(Data, Options, State) ->
  case lists:keyfind(tv, 1, Data) of
    {tv, TVID} -> 
      case lists:keyfind(season, 1, Data) of
        {season, SeasonNum} -> 
          case lists:keyfind(episode, 1, Data) of
            {episode, EpisodeNum} ->
              {BaseURL, ImageURL, RequestParams} = parse_options(
                State ++ Options
              ),
              URL = BaseURL ++ "/tv/" ++ emdb_utils:to_list(TVID) ++ "/season/" ++ emdb_utils:to_list(SeasonNum) ++ "/episode/" ++ emdb_utils:to_list(EpisodeNum) ++ "?" ++ emdb_utils:keylist_to_params_string(RequestParams),
              lager:debug("[TMDB] GET ~p", [URL]),
              case httpc:request(URL) of 
                {ok, {{_Version, 200, _ReasonPhrase}, _Headers, Body}} -> 
                  to_episode(TVID, SeasonNum, EpisodeNum, ImageURL, [jsx:decode(list_to_binary(Body))]);
                _ -> []
              end;
            _ -> []
          end;
        _ -> []
      end;
    _ -> []
  end.

to_episode(TVID, SeasonNum, EpisodeNum, ImageURL, Results) ->
  lists:foldl(fun(Episode, AccIn) ->
        PosterURL = case proplists:get_value(<<"still_path">>, Episode) of
          P when is_binary(P) -> ImageURL ++ binary_to_list(P);
          _ -> undefined
        end,
        AccIn ++ [#episode{
            id = proplists:get_value(<<"id">>, Episode),
            tv_id = TVID,
            season = SeasonNum,
            episode = EpisodeNum,
            source = themoviedb,
            title = proplists:get_value(<<"name">>, Episode),
            original_title = undefined,
            date = proplists:get_value(<<"air_date">>, Episode),
            poster = PosterURL,
            backdrop = undefined,
            overview = proplists:get_value(<<"overview">>, Episode)
          }]
    end, [], Results).

% -- Common --

parse_options(Options) ->
  lists:foldl(fun({Key, Value}, {BaseURL1, ImageURL1, RequestParams1}) ->
          case emdb_utils:to_atom(Key) of
            base_url -> {Value, ImageURL1, RequestParams1};
            image_url -> {BaseURL1, Value, RequestParams1};
            key -> {BaseURL1, ImageURL1, [{api_key, Value}] ++ RequestParams1};
            X -> {BaseURL1, ImageURL1, [{X, Value}] ++ RequestParams1}
          end
      end, {undefined, undefined, []}, Options).
