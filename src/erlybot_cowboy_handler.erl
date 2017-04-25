%% ------------------------------------------------------------------
%% @doc Erlybot Cowboy handler
%% @end
%% ------------------------------------------------------------------
-module(erlybot_cowboy_handler).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([init/2]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec init(cowboy_req:req(), term()) -> {ok, cowboy_req:req(), term()}.
init(Req0, State) ->
  {ok, Data, _} = cowboy_req:read_body(Req0),
  Req = cowboy_req:reply(200, #{},"" , Req0),

  erlybot_parser:parse_message(Data),

  {ok, Req, State}.


