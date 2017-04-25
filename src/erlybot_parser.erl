%% ------------------------------------------------------------------
%% @doc Erlybot incoming messages parser
%% @end
%% ------------------------------------------------------------------
-module(erlybot_parser).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0, parse_message/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------
-export([init/1, 
          handle_call/3, 
          handle_cast/2, 
          handle_info/2,
          terminate/2, 
          code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, Error :: term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec parse_message(binary()) -> ok.
parse_message(Data) ->
  gen_server:cast(?SERVER, Data).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-spec init(_) -> {ok, []}.
init(_Args) ->
  lager:info("Erlybot: starting inbound messages parser..."),
  {ok, []}.

-spec handle_call(_, _, _) -> {noreply, term()}.
handle_call(_Request, _From, _State) -> 
  {noreply, _State}.

-spec handle_cast(binary(), term()) -> {noreply, term()}.
handle_cast(Msg, _State) ->

  UpdateBody = jsx:decode(Msg),

  Message = get_value(<<"message">>, UpdateBody),
  UserId = get_value(<<"id">>, get_value(<<"from">>, Message)),
  Username = get_value(<<"username">>, get_value(<<"from">>, Message)),
  ChatId = get_value(<<"id">>, get_value(<<"chat">>, Message)),
  MessageText = get_value(<<"text">>, Message),

  Reply = {UserId, Username, ChatId, MessageText},

  validate_message(lists:member(undefined, tuple_to_list(Reply)), Reply),

  {noreply, []}.

-spec handle_info(_, term()) -> {noreply, term()}.
handle_info(_Info, State) ->
  {noreply, State}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_, term(), _) -> {ok, term()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
-spec get_value (term(), undefined) -> undefined;
                (binary(), [term()]) -> term().
get_value(_, undefined) -> undefined;
get_value(Key, Data) -> proplists:get_value(Key, Data).

-spec validate_message(false, tuple()) -> ok;
                      (true, _) -> ok.
validate_message(false, {UserId, Username, ChatId, MessageText}) -> 
  erlybot_processor:process_message({UserId, binary:bin_to_list(Username), ChatId, binary:bin_to_list(MessageText)});

validate_message(true, _) -> 
  lager:info("Erlybot parser error!"), ok.