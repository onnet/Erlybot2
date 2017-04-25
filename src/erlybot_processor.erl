%% ------------------------------------------------------------------
%% @doc Erlybot messages processor
%% @end
%% ------------------------------------------------------------------
-module(erlybot_processor).
-behaviour(gen_server).
-include("erlybot.hrl").
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/0]).
-export([process_message/1]).

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
-spec start_link() -> {ok,pid()} | ignore | {error, Error :: term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-type message() :: {integer(), [byte()], integer(), [byte()]}.
-spec process_message(message()) -> {reply, ok, []}.
process_message(Data) ->
  gen_server:call(?SERVER, Data).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
%% init with deferred initialization
-spec init(_) -> {ok, []}.
init(_Args) ->
  lager:info("Erlybot: starting messages processor..."),
  self() ! do_init,  
  {ok, []}.

-spec handle_call(message(), _, any()) -> {reply, ok, []}.
handle_call(Request, _From, _State) ->
  {UserId, Username, ChatId, MessageText} = Request,
  Userstate = check_user_state(UserId),

  case Userstate of

    "unauthorized" -> 
      reply_to_unauthorized(UserId, Username, ChatId, normalize_command(MessageText));

    "challenge_sent" ->
      wait_for_password(UserId, Username, ChatId, normalize_command(MessageText));

    "authorized" ->
      handle_command(UserId, Username, ChatId, normalize_command(MessageText))

  end,
      
  {reply, ok, []}.

-spec handle_cast(_, any()) -> {noreply, any()}.
handle_cast(_Msg, State) ->
  {noreply, State}.

-spec handle_info(do_init, _) -> {noreply, []}.
handle_info(do_init, _State) ->
  init_cowboy(),
  init_usertable(),  
  {noreply, []}.

-spec terminate(_, _) -> ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(_, any(), _) -> {ok, any()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

-spec init_cowboy() -> ok.
init_cowboy() ->

  {ok, Token} = application:get_env(?APPLICATION, token),
  {ok, IP} = application:get_env(?APPLICATION, ip),
  {ok, Port} = application:get_env(?APPLICATION, port),
  BotPath = binary:list_to_bin("/" ++ lists:last(string:tokens(Token, ":"))),

  Dispatch = cowboy_router:compile([
    {'_', [{BotPath, erlybot_cowboy_handler, #{}}]}]),

  {ok, _} = cowboy:start_clear(main_bot_listener, 100,
    [
      {port, Port},
      {ip, IP}
    ],
    #{env => #{dispatch => Dispatch}}),
  
  lager:info("Erlybot: Cowboy initialization complete."),
  ok.

%% @doc creates new ETS for user states
-spec init_usertable() ->  atom().
init_usertable() ->
  ets:new(usertable, [named_table, public, set]).

%% @doc return current user state from ETS
-spec check_user_state(integer()) -> string().
check_user_state(UserId) ->

  Userstate = case ets:member(usertable, UserId) of 

	false ->
	    ets:insert(usertable, {UserId, "unauthorized"}),
        "unauthorized";

	true -> 
        [{_, User_state}] = ets:lookup(usertable, UserId),
        User_state

    end,

  Userstate.

%% @doc Sends an authorization request to user
-spec send_authorization_request(integer(), [byte()], integer()) -> true.
send_authorization_request(UserId, Username, ChatId) ->
  send_reply(ChatId, Username ++ ", input your password, please!"),
  ets:insert(usertable, {UserId, "challenge_sent"}).

%% @doc Sends message to certain chat id
-spec send_reply(integer(), string()) -> ok.
send_reply(ChatId, Reply) ->
  {ok, Path} = application:get_env(?APPLICATION, token),
  httpc:request(post, {"https://api.telegram.org/bot"++Path++"/sendMessage", 
	[], 
	"application/x-www-form-urlencoded", "chat_id=" ++ integer_to_list(ChatId) ++ "&text=" ++ Reply}, 
  [], []),
  ok.

%% @doc Authorize user - change state to "authorized"
-spec do_authorization(integer()) -> true.
do_authorization(UserId) ->
  ets:insert(usertable, {UserId, "authorized"}).


%% @doc Cut @<bot_name> from message
-spec normalize_command([byte()]) -> [byte()].
normalize_command(Command) ->
  lists:nth(1, re:split(Command, "[@]", [{return, list}])).

%% @doc Reply for unauthorized user
reply_to_unauthorized(UserId, Username, ChatId, "/start") ->
  send_authorization_request(UserId, Username, ChatId);

reply_to_unauthorized(_UserId, _Username, ChatId, _) ->
  send_reply(ChatId, "Begin with /start").

%% @doc Requesting password
wait_for_password(UserId, Username, ChatId, "/password23084") ->
  do_authorization(UserId),
  Reply = Username ++ ", authorization complete, waiting for commands...",
  send_reply(ChatId, Reply);
  
wait_for_password(_UserId, Username, ChatId, _) ->
  Reply = Username ++ ", password incorrect",
  send_reply(ChatId, Reply).

%% ------------------------------------------------------------------
%% Command Handlers
%% ------------------------------------------------------------------
-spec handle_command(integer(), string(), integer(), string()) -> any().
handle_command(_UserId, _Username, ChatId, "/start") -> 
  send_reply(ChatId, "Session already started.");

handle_command(_UserId, _Username, ChatId, "/password23084") ->
  send_reply(ChatId, "Already authorized.");

handle_command(_UserId, _Username, ChatId, "/help") ->
  send_reply(ChatId, "No real goals, just for fun.");

handle_command(UserId, Username, ChatId, "/exit") ->
  ets:insert(usertable, {UserId, "unauthorized"}),
  send_reply(ChatId, Username ++ ", logout complete.");

handle_command(_UserId, _Username, ChatId, _) ->
  send_reply(ChatId, "Not implemented.").