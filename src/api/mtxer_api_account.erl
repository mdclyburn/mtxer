-module(mtxer_api_account).

-include_lib("kernel/include/logger.hrl").

-export([register/2,
         login/2,
         logout/1]).

-type register_result() :: {ok, map()} | error.

-spec register(Username, Password) -> Result when
      Username :: nonempty_string(),
      Password :: nonempty_string(),
      Result :: register_result().

register(Username, Password) ->
    Nonce = nonce(),
    Mac = erlang:list_to_binary(string:to_lower(calculate_mac(Username, Password, Nonce))),
    Payload = #{nonce => Nonce,
                username => unicode:characters_to_binary(Username),
                password => unicode:characters_to_binary(Password),
                mac => Mac},
    io:format("~s~n", [Mac]),
    mtxer_api_common:post(
      "/_synapse/admin/v1/register",
      [],
      Payload).

login(Username, Password) ->
    Payload = #{type => <<"m.login.password">>,
                identifier => #{type => <<"m.id.user">>,
                                user => unicode:characters_to_binary(Username)},
                password => unicode:characters_to_binary(Password),
                device_id => <<"MTXER">>},
    ?LOG_INFO("Logging into '~s'.", [Username]),
    #{<<"access_token">> := Token} = mtxer_api_common:post("/_matrix/client/r0/login", [], Payload),
    erlang:binary_to_list(Token).

logout(Token) ->
    ?LOG_INFO("Logging out.", [Token]),
    mtxer_api_common:post(
      "/_matrix/client/r0/logout",
      [mtxer_api_common:authorization(Token)],
      nil).

%%%%% Private %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Calculate MAC for registration authorization token header.
calculate_mac(Username, Password, Nonce) ->
    EncUsername = unicode:characters_to_binary(Username, utf8),
    EncPassword = unicode:characters_to_binary(Password, utf8),
    Data = <<Nonce/binary, <<0>>/binary,
             EncUsername/binary, <<0>>/binary,
             EncPassword/binary, <<0>>/binary,
             <<"notadmin">>/binary>>,
    Mac = crypto:mac(hmac, sha, shared_secret(), Data),
    mac_to_hex(Mac).

mac_to_hex(Mac) ->
    Padded = [string:pad(erlang:integer_to_list(X, 16), 2, leading, "0")
              || X <- erlang:binary_to_list(Mac)],
    lists:flatten(Padded).

% Retrieve the nonce for registration.
nonce() ->
    Obj = mtxer_api_common:get("/_synapse/admin/v1/register", []),
    #{<<"nonce">> := Nonce} = Obj,
    Nonce.

shared_secret() ->
    {ok, Secret} = application:get_env(shared_secret),
    unicode:characters_to_binary(Secret).
