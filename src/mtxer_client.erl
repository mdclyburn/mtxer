-module(mtxer_client).
-behavior(gen_server).

-include_lib("kernel/include/logger.hrl").

-export([start_link/1]).
-export([init/1,
         terminate/2,
         handle_call/3,
         handle_cast/2,
         handle_info/2]).
-export([users/0,
         rooms/1]).

-record(state, {users = [] :: [mtxer_user:user()]}).

-type state() :: #state{}.

%%%%% Interface %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

users() -> gen_server:call(?MODULE, users).

rooms(Username) -> gen_server:call(?MODULE, {rooms, Username}).

%%%%% gen_server %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Accounts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Accounts], []).

init([Accounts]) ->
    erlang:process_flag(trap_exit, true),
    Users = lists:map(fun ({Username, Password}) -> mtxer_user:new(Username, Password) end, Accounts),
    {ok, #state{users = login(Users)}}.

terminate(_Reason, State) ->
    logout(State#state.users).

handle_call(users, _From, State) ->
    {reply,
     [mtxer_user:username(User) || User <- State#state.users],
     State};
handle_call({rooms, Username}, _From, State) ->
    RoomsResult =
        with_user(
          State#state.users,
          Username,
          fun (User) -> mtxer_api_room:list(User) end),
    {reply, RoomsResult, State};
handle_call(_Request, _From, State) ->
    {reply, nil, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%%%%% Private %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

login(Users) -> [mtxer_api_account:login(U) || U <- Users].

logout([]) -> ok;
logout([User|RestUsers]) ->
    mtxer_api_account:logout(User),
    logout(RestUsers).

with_user(Users, Username, Action) ->
    Result = lists:search(fun (User) -> mtxer_user:username(User) == Username end, Users),
    case Result of
        {value, User} -> Action(User);
        false -> {error, nouser}
    end.
