-module(tvrage).
-behaviour(gen_event).

-define(DEFAULT_ARGS, [
    % TODO
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
  lager:info("[tvrage] search movie ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, tv, Data, Options}, State) ->
  lager:info("[tvrage] search tv ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, season, Data, Options}, State) ->
  lager:info("[tvrage] search season ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, episode, Data, Options}, State) ->
  lager:info("[tvrage] search season ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, cast, Data, Options}, State) ->
  lager:info("[tvrage] search cast ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, person, Data, Options}, State) ->
  lager:info("[tvrage] search person ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, album, Data, Options}, State) ->
  lager:info("[tvrage] search album ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call({search, song, Data, Options}, State) ->
  lager:info("[tvrage] search song ~p with ~p", [Data, Options]),
  {ok, [], State}; % TODO
handle_call(_Request, State) ->
  {ok, not_available, State}.

handle_info(_Info, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Args, State) ->
  file:close(State).

