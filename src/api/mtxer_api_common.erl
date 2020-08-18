-module(mtxer_api_common).

-compile(export_all).

-export([authorization/1,
         get/2,
         post/3]).

-spec authorization(User) -> Header when
      User :: mtxer_user:user(),
      Header :: {string(), string()}.

authorization(User) ->
    {"Authorization", "Bearer " ++ mtxer_user:access_token(User)}.

-spec get(Path, Headers) -> Body when
      Path :: nonempty_string(),
      Headers :: [{nonempty_string(), nonempty_string()}],
      Body :: any().

get(Path, Headers) ->
    {ok, {ResponseInfo, _ResponseHeaders, ResponseContent}} =
        httpc:request(
          get,
          {url(Path), Headers},
          [],
          [{body_format, binary}]),
    case ResponseInfo of
        {_Method, 200, _Status} -> jsone:decode(ResponseContent);
        {_Method, 203, _Status} -> ok;
        {_Method, StatusCode, _Status} -> {error, StatusCode, jsone:decode(ResponseContent)}
    end.

post(Path, Headers, Content) ->
    {ok, {ResponseInfo, _ResponseHeaders, ResponseContent}} =
        httpc:request(
          post,
          {url(Path), Headers, "application/json", jsone:encode(Content)},
          [],
          [{body_format, binary}]),
    case ResponseInfo of
        {_Http, 200, _Status} -> jsone:decode(ResponseContent);
        {_Http, 203, _Status} -> ok;
        {_Method, StatusCode, _Status} -> {error, StatusCode, jsone:decode(ResponseContent)}
    end.

%%%%% Private %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Build URL.
homeserver() ->
    {ok, HomeserverUrl} = application:get_env(url),
    HomeserverUrl.

url(Path) -> homeserver() ++ Path.
