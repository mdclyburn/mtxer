-module(mtxer_user).

-export([new/2,
         username/1,
         password/1,
         access_token/1,
         set_access_token/2]).

-record(user, {username = "" :: string(),
               password = "" :: string(),
               access_token = "" :: string()}).

-type user() :: #user{}.

-export_type([user/0]).

-spec new(Username, Password) -> User when
      Username :: string(),
      Password :: string(),
      User :: user().

new(Username, Password) ->
    #user{
       username = Username,
       password = Password}.

-spec username(User) -> Username when
      User :: user(),
      Username :: string().

username(User) -> User#user.username.

-spec password(User) -> Password when
      User :: user(),
      Password :: string().

password(User) -> User#user.password.

-spec access_token(User) -> Token when
      User :: user(),
      Token :: nonempty_string().

access_token(User) -> User#user.access_token.

-spec set_access_token(User1, Token) -> User2 when
      User1 :: user(),
      Token :: nonempty_string(),
      User2 :: user().

set_access_token(User1, Token) ->
    User1#user{access_token = Token}.
