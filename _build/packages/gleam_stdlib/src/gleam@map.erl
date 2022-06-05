-module(gleam@map).
-compile(no_auto_import).

-export([size/1, to_list/1, from_list/1, has_key/2, new/0, get/2, insert/3, map_values/2, keys/1, values/1, filter/2, take/2, merge/2, delete/2, drop/2, update/3, fold/3]).
-export_type([map_/2]).

-type map_(Key, Value) :: any() | {gleam_phantom, Key, Value}.

-spec size(map_(any(), any())) -> integer().
size(Map) ->
    maps:size(Map).

-spec to_list(map_(AQS, AQT)) -> list({AQS, AQT}).
to_list(Map) ->
    maps:to_list(Map).

-spec from_list(list({ARC, ARD})) -> map_(ARC, ARD).
from_list(List) ->
    maps:from_list(List).

-spec has_key(map_(ARM, any()), ARM) -> boolean().
has_key(Map, Key) ->
    maps:is_key(Key, Map).

-spec new() -> map_(any(), any()).
new() ->
    maps:new().

-spec get(map_(ASC, ASD), ASC) -> {ok, ASD} | {error, nil}.
get(From, Get) ->
    gleam_stdlib:map_get(From, Get).

-spec insert(map_(ASO, ASP), ASO, ASP) -> map_(ASO, ASP).
insert(Map, Key, Value) ->
    maps:put(Key, Value, Map).

-spec map_values(map_(ATA, ATB), fun((ATA, ATB) -> ATE)) -> map_(ATA, ATE).
map_values(Map, Fun) ->
    maps:map(Fun, Map).

-spec keys(map_(ATO, any())) -> list(ATO).
keys(Map) ->
    maps:keys(Map).

-spec values(map_(any(), ATZ)) -> list(ATZ).
values(Map) ->
    maps:values(Map).

-spec filter(map_(AUI, AUJ), fun((AUI, AUJ) -> boolean())) -> map_(AUI, AUJ).
filter(Map, Property) ->
    maps:filter(Property, Map).

-spec take(map_(AUU, AUV), list(AUU)) -> map_(AUU, AUV).
take(Map, Desired_keys) ->
    maps:with(Desired_keys, Map).

-spec merge(map_(AVI, AVJ), map_(AVI, AVJ)) -> map_(AVI, AVJ).
merge(Map, New_entries) ->
    maps:merge(Map, New_entries).

-spec delete(map_(AVY, AVZ), AVY) -> map_(AVY, AVZ).
delete(Map, Key) ->
    maps:remove(Key, Map).

-spec drop(map_(AWK, AWL), list(AWK)) -> map_(AWK, AWL).
drop(Map, Disallowed_keys) ->
    gleam@list:fold(Disallowed_keys, Map, fun delete/2).

-spec update(map_(AWR, AWS), AWR, fun((gleam@option:option(AWS)) -> AWS)) -> map_(AWR, AWS).
update(Map, Key, Fun) ->
    _pipe = Map,
    _pipe@1 = get(_pipe, Key),
    _pipe@2 = gleam@option:from_result(_pipe@1),
    _pipe@3 = Fun(_pipe@2),
    insert(Map, Key, _pipe@3).

-spec do_fold(list({AWY, AWZ}), AXB, fun((AXB, AWY, AWZ) -> AXB)) -> AXB.
do_fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [{K, V} | Tail] ->
            do_fold(Tail, Fun(Initial, K, V), Fun)
    end.

-spec fold(map_(AXC, AXD), AXG, fun((AXG, AXC, AXD) -> AXG)) -> AXG.
fold(Map, Initial, Fun) ->
    _pipe = Map,
    _pipe@1 = to_list(_pipe),
    do_fold(_pipe@1, Initial, Fun).
