-module(mtxer_api_room).

-include_lib("kernel/include/logger.hrl").

-export([list/1]).

-record(room, {name = "" :: string(),
               alias = "" :: string(),
               id = "" :: string(),
               member_count = 0 :: non_neg_integer()}).

-type room() :: #room{}.
-export_type([room/0]).

list(User) ->
    ?LOG_INFO("Listing rooms as '~s'.", [mtxer_user:username(User)]),
    #{<<"chunk">> := RoomsData} =
        mtxer_api_common:get(
          "/_matrix/client/r0/publicRooms",
          [mtxer_api_common:authorization(User)]),
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
