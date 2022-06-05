-module(gleam@float).
-compile(no_auto_import).

-export([parse/1, to_string/1, clamp/3, compare/2, loosely_compare/3, min/2, max/2, ceiling/1, floor/1, round/1, truncate/1, absolute_value/1, power/2, square_root/1, negate/1, sum/1, product/1, random/2]).

-spec parse(binary()) -> {ok, float()} | {error, nil}.
parse(String) ->
    gleam_stdlib:parse_float(String).

-spec to_string(float()) -> binary().
to_string(F) ->
    _pipe = F,
    _pipe@1 = gleam@string_builder:from_float(_pipe),
    gleam@string_builder:to_string(_pipe@1).

-spec clamp(float(), float(), float()) -> float().
clamp(N, Min_bound, Max_bound) ->
    _pipe = N,
    _pipe@1 = min(_pipe, Max_bound),
    max(_pipe@1, Min_bound).

-spec compare(float(), float()) -> gleam@order:order().
compare(A, B) ->
    case A =:= B of
        true ->
            eq;

        false ->
            case A < B of
                true ->
                    lt;

                false ->
                    gt
            end
    end.

-spec loosely_compare(float(), float(), float()) -> gleam@order:order().
loosely_compare(A, B, Tolerance) ->
    Diff = absolute_value(A - B),
    case Diff =< Tolerance of
        true ->
            eq;

        false ->
            compare(A, B)
    end.

-spec min(float(), float()) -> float().
min(A, B) ->
    case A < B of
        true ->
            A;

        false ->
            B
    end.

-spec max(float(), float()) -> float().
max(A, B) ->
    case A > B of
        true ->
            A;

        false ->
            B
    end.

-spec ceiling(float()) -> float().
ceiling(Float) ->
    math:ceil(Float).

-spec floor(float()) -> float().
floor(Float) ->
    math:floor(Float).

-spec round(float()) -> integer().
round(Float) ->
    erlang:round(Float).

-spec truncate(float()) -> integer().
truncate(Float) ->
    erlang:trunc(Float).

-spec absolute_value(float()) -> float().
absolute_value(Float) ->
    case Float >= 0.0 of
        true ->
            Float;

        _@1 ->
            0.0 - Float
    end.

-spec power(float(), float()) -> float().
power(Base, Exponent) ->
    math:pow(Base, Exponent).

-spec square_root(float()) -> {ok, float()} | {error, nil}.
square_root(Number) ->
    case Number < 0.0 of
        true ->
            {error, nil};

        false ->
            {ok, power(Number, 0.5)}
    end.

-spec negate(float()) -> float().
negate(X) ->
    -1.0 * X.

-spec sum(list(float())) -> float().
sum(Numbers) ->
    _pipe = Numbers,
    do_sum(_pipe, 0.0).

-spec do_sum(list(float()), float()) -> float().
do_sum(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_sum(Rest, X + Initial)
    end.

-spec product(list(float())) -> float().
product(Numbers) ->
    case Numbers of
        [] ->
            0.0;

        _@1 ->
            do_product(Numbers, 1.0)
    end.

-spec do_product(list(float()), float()) -> float().
do_product(Numbers, Initial) ->
    case Numbers of
        [] ->
            Initial;

        [X | Rest] ->
            do_product(Rest, X * Initial)
    end.

-spec random(float(), float()) -> float().
random(Boundary_a, Boundary_b) ->
    {Min, Max} = case {Boundary_a, Boundary_b} of
        {A, B} when A =< B ->
            {A, B};

        {A@1, B@1} when A@1 > B@1 ->
            {B@1, A@1}
    end,
    case {Min, Max} of
        {Min@1, _@1} when Min@1 =:= Max ->
            Min@1;

        {Min@2, Max@1} ->
            (rand:uniform() * (Max@1 - Min@2)) + Min@2
    end.
