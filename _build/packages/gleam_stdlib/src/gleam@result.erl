-module(gleam@result).
-compile(no_auto_import).

-export([is_ok/1, is_error/1, map/2, map_error/2, flatten/1, then/2, unwrap/2, lazy_unwrap/2, unwrap_error/2, unwrap_both/1, nil_error/1, 'or'/2, lazy_or/2, all/1, replace/2, replace_error/2, values/1]).

-spec is_ok({ok, any()} | {error, any()}) -> boolean().
is_ok(Result) ->
    case Result of
        {error, _@1} ->
            false;

        {ok, _@2} ->
            true
    end.

-spec is_error({ok, any()} | {error, any()}) -> boolean().
is_error(Result) ->
    case Result of
        {ok, _@1} ->
            false;

        {error, _@2} ->
            true
    end.

-spec map({ok, BYA} | {error, BYB}, fun((BYA) -> BYE)) -> {ok, BYE} |
    {error, BYB}.
map(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, Fun(X)};

        {error, E} ->
            {error, E}
    end.

-spec map_error({ok, BYH} | {error, BYI}, fun((BYI) -> BYL)) -> {ok, BYH} |
    {error, BYL}.
map_error(Result, Fun) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, Error} ->
            {error, Fun(Error)}
    end.

-spec flatten({ok, {ok, BYO} | {error, BYP}} | {error, BYP}) -> {ok, BYO} |
    {error, BYP}.
flatten(Result) ->
    case Result of
        {ok, X} ->
            X;

        {error, Error} ->
            {error, Error}
    end.

-spec then({ok, BYW} | {error, BYX}, fun((BYW) -> {ok, BZA} | {error, BYX})) -> {ok,
                                                                                 BZA} |
    {error, BYX}.
then(Result, Fun) ->
    case Result of
        {ok, X} ->
            Fun(X);

        {error, E} ->
            {error, E}
    end.

-spec unwrap({ok, BZF} | {error, any()}, BZF) -> BZF.
unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default
    end.

-spec lazy_unwrap({ok, BZJ} | {error, any()}, fun(() -> BZJ)) -> BZJ.
lazy_unwrap(Result, Default) ->
    case Result of
        {ok, V} ->
            V;

        {error, _@1} ->
            Default()
    end.

-spec unwrap_error({ok, any()} | {error, BZO}, BZO) -> BZO.
unwrap_error(Result, Default) ->
    case Result of
        {ok, _@1} ->
            Default;

        {error, E} ->
            E
    end.

-spec unwrap_both({ok, BZR} | {error, BZR}) -> BZR.
unwrap_both(Result) ->
    case Result of
        {ok, A} ->
            A;

        {error, A@1} ->
            A@1
    end.

-spec nil_error({ok, BZU} | {error, any()}) -> {ok, BZU} | {error, nil}.
nil_error(Result) ->
    map_error(Result, fun(_) -> nil end).

-spec 'or'({ok, CAA} | {error, CAB}, {ok, CAA} | {error, CAB}) -> {ok, CAA} |
    {error, CAB}.
'or'(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second
    end.

-spec lazy_or({ok, CAI} | {error, CAJ}, fun(() -> {ok, CAI} | {error, CAJ})) -> {ok,
                                                                                 CAI} |
    {error, CAJ}.
lazy_or(First, Second) ->
    case First of
        {ok, _@1} ->
            First;

        {error, _@2} ->
            Second()
    end.

-spec all(list({ok, CAQ} | {error, CAR})) -> {ok, list(CAQ)} | {error, CAR}.
all(Results) ->
    gleam@list:try_map(Results, fun(X) -> X end).

-spec replace({ok, any()} | {error, CAZ}, CBC) -> {ok, CBC} | {error, CAZ}.
replace(Result, Value) ->
    case Result of
        {ok, _@1} ->
            {ok, Value};

        {error, Error} ->
            {error, Error}
    end.

-spec replace_error({ok, CBF} | {error, any()}, CBJ) -> {ok, CBF} | {error, CBJ}.
replace_error(Result, Error) ->
    case Result of
        {ok, X} ->
            {ok, X};

        {error, _@1} ->
            {error, Error}
    end.

-spec values(list({ok, CBM} | {error, any()})) -> list(CBM).
values(Results) ->
    gleam@list:filter_map(Results, fun(R) -> R end).
