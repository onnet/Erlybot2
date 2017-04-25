%% ------------------------------------------------------------------
%% @doc Erlybot top level supervisor.
%% @end
%% ------------------------------------------------------------------
-module(erlybot_sup).
-behaviour(supervisor).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).

%% ------------------------------------------------------------------
%% Supervisor Function Exports
%% ------------------------------------------------------------------
-export([init/1]).

%% ------------------------------------------------------------------
%% API Function definitions 
%% ------------------------------------------------------------------

-spec start_link() ->  {ok, pid()} | ignore | {error, supervisor:startlink_err()}.
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ------------------------------------------------------------------
%% Supervisor Function Definitions
%% ------------------------------------------------------------------
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}

-spec init(_) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} | ignore.
init(_) ->
  lager:info("Erlybot: starting main supervisor..."),
  {ok, { {one_for_one, 20, 10}, [
                                  {erlybot_parser, 
                                  {erlybot_parser, start_link, []},
                                  permanent,
                                  2000,
                                  worker,
                                  [erlybot_parser]},

                                  {erlybot_processor, 
                                  {erlybot_processor, start_link, []},
                                  permanent,
                                  2000,
                                  worker,
                                  [erlybot_processor]}

                                ]}}.