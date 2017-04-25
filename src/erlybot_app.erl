%% ------------------------------------------------------------------
%% @doc Erlybot - Telegram bot scaffold
%% @end
%% ------------------------------------------------------------------
-module(erlybot_app).
-behaviour(application).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start/2, stop/1]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start(_, _) -> {ok, pid()}.
start(_StartType, _StartArgs) ->
  application:ensure_all_started(?MODULE),
  lager:info("Erlybot: start."),
  erlybot_sup:start_link().

-spec stop(_) -> ok.
stop(_State) ->
  lager:info("Erlybot: stop."),
  ok.

