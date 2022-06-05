-module(gleam@pair).
-compile(no_auto_import).

-export([first/1, second/1, swap/1, map_first/2, map_second/2]).

-spec first({HG, any()}) -> HG.
first(Pair) ->
    {A, _@1} = Pair,
    A.

-spec second({any(), HJ}) -> HJ.
second(Pair) ->
    {_@1, A} = Pair,
    A.

-spec swap({HK, HL}) -> {HL, HK}.
swap(Pair) ->
    {A, B} = Pair,
    {B, A}.

-spec map_first({HM, HN}, fun((HM) -> HO)) -> {HO, HN}.
map_first(Pair, Fun) ->
    {A, B} = Pair,
    {Fun(A), B}.

-spec map_second({HP, HQ}, fun((HQ) -> HR)) -> {HP, HR}.
map_second(Pair, Fun) ->
    {A, B} = Pair,
    {A, Fun(B)}.
