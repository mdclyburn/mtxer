-module(mtxer_user).

-export([login/2,
         logout/1,
         username/1,
         access_token/1,
         membership/1,
         is_member/2,
         get_transaction_id/1]).

-record(user, {username = "" :: string(),
               access_token = "" :: string(),
               tx_id = 1 :: non_neg_integer()}).

-type user() :: #user{}.

-export_type([user/0]).

-spec login(Username, Password) -> User when
      Username :: string(),
      Password :: string(),
      User :: user().

login(Username, Password) ->
    #user{
       username = Username,
       access_token = mtxer_api_account:login(Username, Password)}.

-spec logout(User1) -> User2 when
      User1 :: user(),
      User2 :: user().

logout(User) ->
    mtxer_api_account:logout(User#user.access_token),
    User#user{access_token = ""}.

-spec username(User) -> Username when
      User :: user(),
      Username :: string().

username(User) -> User#user.username.

-spec access_token(User) -> Token when
      User :: user(),
      Token :: nonempty_string().

access_token(User) -> User#user.access_token.

-spec membership(User) -> RoomList when
      User :: user(),
      RoomList :: [string()].

membership(User) -> mtxer_api_room:joined(User#user.access_token).

-spec is_member(User, Room) -> IsMember when
      User :: user(),
      Room :: string(),
      IsMember :: boolean().

is_member(User, Room) -> lists:member(Room, membership(User)).

-spec get_transaction_id(User1) -> User2 when
      User1 :: user(),
      User2 :: user().

get_transaction_id(User) ->
    {User#user.tx_id,
     User#user{tx_id = User#user.tx_id + 1}}.
