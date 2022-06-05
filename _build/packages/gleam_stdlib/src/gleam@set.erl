-module(gleam@set).
-compile(no_auto_import).

-export([new/0, size/1, insert/2, contains/2, delete/2, to_list/1, from_list/1, fold/3, filter/2, take/2, union/2, intersection/2]).
-export_type([set/1]).

-opaque set(DVG) :: {set, gleam@map:map_(DVG, list(nil))}.

-spec new() -> set(any()).
new() ->
    {set, gleam@map:new()}.

-spec size(set(any())) -> integer().
size(Set) ->
    gleam@map:size(erlang:element(2, Set)).

-spec insert(set(DVP), DVP) -> set(DVP).
insert(Set, Member) ->
    {set, gleam@map:insert(erlang:element(2, Set), Member, [])}.

-spec contains(set(DVS), DVS) -> boolean().
contains(Set, Member) ->
    _pipe = erlang:element(2, Set),
    _pipe@1 = gleam@map:get(_pipe, Member),
    gleam@result:is_ok(_pipe@1).

-spec delete(set(DVU), DVU) -> set(DVU).
delete(Set, Member) ->
    {set, gleam@map:delete(erlang:element(2, Set), Member)}.

-spec to_list(set(DVX)) -> list(DVX).
to_list(Set) ->
    gleam@map:keys(erlang:element(2, Set)).

-spec from_list(list(DWA)) -> set(DWA).
from_list(Members) ->
    Map = gleam@list:fold(
        Members,
        gleam@map:new(),
        fun(M, K) -> gleam@map:insert(M, K, []) end
    ),
    {set, Map}.

-spec fold(set(DWD), DWF, fun((DWF, DWD) -> DWF)) -> DWF.
fold(Set, Initial, Reducer) ->
    gleam@map:fold(
        erlang:element(2, Set),
        Initial,
        fun(A, K, _) -> Reducer(A, K) end
    ).

-spec filter(set(DWG), fun((DWG) -> boolean())) -> set(DWG).
filter(Set, Property) ->
    {set,
     gleam@map:filter(erlang:element(2, Set), fun(M, _) -> Property(M) end)}.

-spec take(set(DWJ), list(DWJ)) -> set(DWJ).
take(Set, Desired) ->
    {set, gleam@map:take(erlang:element(2, Set), Desired)}.

-spec order(set(DWN), set(DWN)) -> {set(DWN), set(DWN)}.
order(First, Second) ->
    case gleam@map:size(erlang:element(2, First))
    > gleam@map:size(erlang:element(2, Second)) of
        true ->
            {First, Second};

        false ->
            {Second, First}
    end.

-spec union(set(DWS), set(DWS)) -> set(DWS).
union(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    fold(Smaller, Larger, fun insert/2).

-spec intersection(set(DWW), set(DWW)) -> set(DWW).
intersection(First, Second) ->
    {Larger, Smaller} = order(First, Second),
    take(Larger, to_list(Smaller)).
