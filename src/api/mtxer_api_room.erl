-module(mtxer_api_room).

-include_lib("kernel/include/logger.hrl").

-export([list/1,
         joined/1,
         join/2,
         resolve/1,
         id_of/1,
         servers_of/1]).

-record(room, {name = "" :: string(),
               alias = "" :: string(),
               id = "" :: string(),
               member_count = 0 :: non_neg_integer()}).

-type room() :: #room{}.
-export_type([room/0]).

list(Token) ->
    #{<<"chunk">> := RoomsData} =
        mtxer_api_common:get(
          "/_matrix/client/r0/publicRooms",
          [mtxer_api_common:authorization(Token)]),
    lists:map(
      fun (RoomData) ->
              #{<<"name">> := Name,
                <<"canonical_alias">> := Alias,
                <<"room_id">> := Id,
                <<"num_joined_members">> := MemberCount} = RoomData,
              #room{name = Name,
                    alias = Alias,
                    id = Id,
                    member_count = MemberCount}
      end,
      RoomsData).

joined(Token) ->
    #{<<"joined_rooms">> := Rooms} =
        mtxer_api_common:get(
          "/_matrix/client/r0/joined_rooms",
          [mtxer_api_common:authorization(Token)]),
    Rooms.

join(Token, Room) ->
    #{<<"room_id">> := RoomId} =
        mtxer_api_common:post(
          "/_matrix/client/r0/join/" ++ Room,
          [mtxer_api_common:authorization(Token)],
          #{}),
    RoomId.

resolve(Alias) ->
    mtxer_api_common:get(
      "/_matrix/client/r0/directory/room/" ++ Alias,
      []).

id_of(Alias) ->
    #{<<"room_id">> := Id} = resolve(Alias),
    Id.

servers_of(Alias) ->
    #{<<"servers">> := Servers} = resolve(Alias),
    Servers.
