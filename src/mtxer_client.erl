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
         membership/1]).

-record(state, {users = [] :: [mtxer_user:user()]}).

%%%%% Interface %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

users() -> gen_server:call(?MODULE, users).

membership(Username) -> gen_server:call(?MODULE, {membership, Username}).

%%%%% gen_server %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

start_link(Accounts) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Accounts], []).

init([Accounts]) ->
    erlang:process_flag(trap_exit, true),
    Users = lists:map(fun ({Username, Password}) -> mtxer_user:login(Username, Password) end, Accounts),
    {ok, #state{users = Users}}.

terminate(_Reason, State) ->
    logout(State#state.users).

handle_call(users, _From, State) ->
    {reply,
     [mtxer_user:username(User) || User <- State#state.users],
     State};
handle_call({membership, Username}, _From, State)->
    {reply,
     with_user(
       State#state.users,
       Username,
       fun (User) -> mtxer_user:membership(User) end),
     State};
handle_call(_Request, _From, State) ->
    {reply, nil, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%%%%% Private %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

logout([]) -> ok;
logout([User|RestUsers]) ->
    mtxer_user:logout(User),
    logout(RestUsers).

with_user(Users, Username, Action) ->
    Result = lists:search(fun (User) -> mtxer_user:username(User) == Username end, Users),
    case Result of
        {value, User} -> Action(User);
        false -> {error, nouser}
    end.
