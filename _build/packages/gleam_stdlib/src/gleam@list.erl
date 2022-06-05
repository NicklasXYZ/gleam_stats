-module(gleam@list).
-compile(no_auto_import).

-export([length/1, reverse/1, is_empty/1, contains/2, first/1, rest/1, filter/2, filter_map/2, map/2, map_fold/3, index_map/2, try_map/2, drop/2, take/2, new/0, append/2, prepend/2, flatten/1, flat_map/2, fold/3, fold_right/3, index_fold/3, try_fold/3, fold_until/3, find/2, find_map/2, all/2, any/2, zip/2, strict_zip/2, unzip/1, intersperse/2, at/2, unique/1, sort/2, range/2, repeat/2, split/2, split_while/2, key_find/2, pop/2, pop_map/2, key_pop/2, key_set/3, each/2, partition/2, permutations/1, window/2, window_by_2/1, drop_while/2, take_while/2, chunk/2, sized_chunk/2, reduce/2, scan/3, last/1, combinations/2, combination_pairs/1, interleave/1, transpose/1]).
-export_type([length_mismatch/0, continue_or_stop/1]).

-type length_mismatch() :: length_mismatch.

-type continue_or_stop(HT) :: {continue, HT} | {stop, HT}.

-spec length(list(any())) -> integer().
length(List) ->
    erlang:length(List).

-spec reverse(list(HY)) -> list(HY).
reverse(Xs) ->
    lists:reverse(Xs).

-spec is_empty(list(any())) -> boolean().
is_empty(List) ->
    List =:= [].

-spec contains(list(IG), IG) -> boolean().
contains(List, Elem) ->
    case List of
        [] ->
            false;

        [Head | Rest] ->
            (Head =:= Elem) orelse contains(Rest, Elem)
    end.

-spec first(list(II)) -> {ok, II} | {error, nil}.
first(List) ->
    case List of
        [] ->
            {error, nil};

        [X | _@1] ->
            {ok, X}
    end.

-spec rest(list(IM)) -> {ok, list(IM)} | {error, nil}.
rest(List) ->
    case List of
        [] ->
            {error, nil};

        [_@1 | Xs] ->
            {ok, Xs}
    end.

-spec do_filter(list(IR), fun((IR) -> boolean()), list(IR)) -> list(IR).
do_filter(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                true ->
                    [X | Acc];

                false ->
                    Acc
            end,
            do_filter(Xs, Fun, New_acc)
    end.

-spec filter(list(IV), fun((IV) -> boolean())) -> list(IV).
filter(List, Predicate) ->
    do_filter(List, Predicate, []).

-spec do_filter_map(list(IY), fun((IY) -> {ok, JA} | {error, any()}), list(JA)) -> list(JA).
do_filter_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            New_acc = case Fun(X) of
                {ok, X@1} ->
                    [X@1 | Acc];

                {error, _@1} ->
                    Acc
            end,
            do_filter_map(Xs, Fun, New_acc)
    end.

-spec filter_map(list(JG), fun((JG) -> {ok, JI} | {error, any()})) -> list(JI).
filter_map(List, Fun) ->
    do_filter_map(List, Fun, []).

-spec do_map(list(JN), fun((JN) -> JP), list(JP)) -> list(JP).
do_map(List, Fun, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            do_map(Xs, Fun, [Fun(X) | Acc])
    end.

-spec map(list(JS), fun((JS) -> JU)) -> list(JU).
map(List, Fun) ->
    do_map(List, Fun, []).

-spec map_fold(list(JW), JY, fun((JY, JW) -> {JY, JZ})) -> {JY, list(JZ)}.
map_fold(List, Acc, Fun) ->
    _pipe = fold(
        List,
        {Acc, []},
        fun(Acc@1, Item) ->
            {Current_acc, Items} = Acc@1,
            {Next_acc, Next_item} = Fun(Current_acc, Item),
            {Next_acc, [Next_item | Items]}
        end
    ),
    gleam@pair:map_second(_pipe, fun reverse/1).

-spec do_index_map(list(KB), fun((integer(), KB) -> KD), integer(), list(KD)) -> list(KD).
do_index_map(List, Fun, Index, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Xs] ->
            Acc@1 = [Fun(Index, X) | Acc],
            do_index_map(Xs, Fun, Index + 1, Acc@1)
    end.

-spec index_map(list(KG), fun((integer(), KG) -> KI)) -> list(KI).
index_map(List, Fun) ->
    do_index_map(List, Fun, 0, []).

-spec do_try_map(list(KK), fun((KK) -> {ok, KM} | {error, KN}), list(KM)) -> {ok,
                                                                              list(KM)} |
    {error, KN}.
do_try_map(List, Fun, Acc) ->
    case List of
        [] ->
            {ok, reverse(Acc)};

        [X | Xs] ->
            case Fun(X) of
                {ok, Y} ->
                    do_try_map(Xs, Fun, [Y | Acc]);

                {error, Error} ->
                    {error, Error}
            end
    end.

-spec try_map(list(KU), fun((KU) -> {ok, KW} | {error, KX})) -> {ok, list(KW)} |
    {error, KX}.
try_map(List, Fun) ->
    do_try_map(List, Fun, []).

-spec drop(list(LD), integer()) -> list(LD).
drop(List, N) ->
    case N =< 0 of
        true ->
            List;

        false ->
            case List of
                [] ->
                    [];

                [_@1 | Xs] ->
                    drop(Xs, N - 1)
            end
    end.

-spec do_take(list(LG), integer(), list(LG)) -> list(LG).
do_take(List, N, Acc) ->
    case N =< 0 of
        true ->
            reverse(Acc);

        false ->
            case List of
                [] ->
                    reverse(Acc);

                [X | Xs] ->
                    do_take(Xs, N - 1, [X | Acc])
            end
    end.

-spec take(list(LK), integer()) -> list(LK).
take(List, N) ->
    do_take(List, N, []).

-spec new() -> list(any()).
new() ->
    [].

-spec append(list(LP), list(LP)) -> list(LP).
append(First, Second) ->
    lists:append(First, Second).

-spec prepend(list(LX), LX) -> list(LX).
prepend(List, Item) ->
    [Item | List].

-spec do_flatten(list(list(MA)), list(MA)) -> list(MA).
do_flatten(Lists, Acc) ->
    case Lists of
        [] ->
            Acc;

        [L | Rest] ->
            do_flatten(Rest, append(Acc, L))
    end.

-spec flatten(list(list(MF))) -> list(MF).
flatten(Lists) ->
    do_flatten(Lists, []).

-spec flat_map(list(MJ), fun((MJ) -> list(ML))) -> list(ML).
flat_map(List, Fun) ->
    _pipe = map(List, Fun),
    flatten(_pipe).

-spec fold(list(MO), MQ, fun((MQ, MO) -> MQ)) -> MQ.
fold(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            fold(Rest, Fun(Initial, X), Fun)
    end.

-spec fold_right(list(MR), MT, fun((MT, MR) -> MT)) -> MT.
fold_right(List, Initial, Fun) ->
    case List of
        [] ->
            Initial;

        [X | Rest] ->
            Fun(fold_right(Rest, Initial, Fun), X)
    end.

-spec do_index_fold(list(MU), MW, fun((MW, MU, integer()) -> MW), integer()) -> MW.
do_index_fold(Over, Acc, With, Index) ->
    case Over of
        [] ->
            Acc;

        [First | Rest] ->
            do_index_fold(Rest, With(Acc, First, Index), With, Index + 1)
    end.

-spec index_fold(list(MX), MZ, fun((MZ, MX, integer()) -> MZ)) -> MZ.
index_fold(Over, Initial, Fun) ->
    do_index_fold(Over, Initial, Fun, 0).

-spec try_fold(list(NA), NC, fun((NC, NA) -> {ok, NC} | {error, ND})) -> {ok,
                                                                          NC} |
    {error, ND}.
try_fold(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            {ok, Accumulator};

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {error, _try} -> {error, _try};
                {ok, Accumulator@1} ->
                    try_fold(Rest, Accumulator@1, Fun)
            end
    end.

-spec fold_until(list(NI), NK, fun((NK, NI) -> continue_or_stop(NK))) -> NK.
fold_until(Collection, Accumulator, Fun) ->
    case Collection of
        [] ->
            Accumulator;

        [First | Rest] ->
            case Fun(Accumulator, First) of
                {continue, Next_accumulator} ->
                    fold_until(Rest, Next_accumulator, Fun);

                {stop, B} ->
                    B
            end
    end.

-spec find(list(NM), fun((NM) -> boolean())) -> {ok, NM} | {error, nil}.
find(Haystack, Is_desired) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Is_desired(X) of
                true ->
                    {ok, X};

                _@1 ->
                    find(Rest, Is_desired)
            end
    end.

-spec find_map(list(NQ), fun((NQ) -> {ok, NS} | {error, any()})) -> {ok, NS} |
    {error, nil}.
find_map(Haystack, Fun) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Fun(X) of
                {ok, X@1} ->
                    {ok, X@1};

                _@1 ->
                    find_map(Rest, Fun)
            end
    end.

-spec all(list(NY), fun((NY) -> boolean())) -> boolean().
all(List, Predicate) ->
    case List of
        [] ->
            true;

        [X | Rest] ->
            Predicate(X) andalso all(Rest, Predicate)
    end.

-spec any(list(OA), fun((OA) -> boolean())) -> boolean().
any(List, Predicate) ->
    case List of
        [] ->
            false;

        [X | Rest] ->
            Predicate(X) orelse any(Rest, Predicate)
    end.

-spec do_zip(list(OC), list(OE), list({OC, OE})) -> list({OC, OE}).
do_zip(Xs, Ys, Acc) ->
    case {Xs, Ys} of
        {[X | Xs@1], [Y | Ys@1]} ->
            do_zip(Xs@1, Ys@1, [{X, Y} | Acc]);

        {_@1, _@2} ->
            reverse(Acc)
    end.

-spec zip(list(OI), list(OK)) -> list({OI, OK}).
zip(Xs, Ys) ->
    do_zip(Xs, Ys, []).

-spec strict_zip(list(ON), list(OP)) -> {ok, list({ON, OP})} |
    {error, length_mismatch()}.
strict_zip(L1, L2) ->
    case length(L1) =:= length(L2) of
        true ->
            {ok, zip(L1, L2)};

        false ->
            {error, length_mismatch}
    end.

-spec do_unzip(list({OY, OZ}), list(OY), list(OZ)) -> {list(OY), list(OZ)}.
do_unzip(Input, Xs, Ys) ->
    case Input of
        [] ->
            {reverse(Xs), reverse(Ys)};

        [{X, Y} | Rest] ->
            do_unzip(Rest, [X | Xs], [Y | Ys])
    end.

-spec unzip(list({OY, OZ})) -> {list(OY), list(OZ)}.
unzip(Input) ->
    do_unzip(Input, [], []).

-spec do_intersperse(list(PD), PD, list(PD)) -> list(PD).
do_intersperse(List, Separator, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [X | Rest] ->
            do_intersperse(Rest, Separator, [X, Separator | Acc])
    end.

-spec intersperse(list(PH), PH) -> list(PH).
intersperse(List, Elem) ->
    case List of
        [] ->
            List;

        [_@1] ->
            List;

        [X | Rest] ->
            do_intersperse(Rest, Elem, [X])
    end.

-spec at(list(PK), integer()) -> {ok, PK} | {error, nil}.
at(List, Index) ->
    _pipe = List,
    _pipe@1 = drop(_pipe, Index),
    first(_pipe@1).

-spec unique(list(PO)) -> list(PO).
unique(List) ->
    case List of
        [] ->
            [];

        [X | Rest] ->
            [X | unique(filter(Rest, fun(Y) -> Y /= X end))]
    end.

-spec merge_sort(list(PR), list(PR), fun((PR, PR) -> gleam@order:order())) -> list(PR).
merge_sort(A, B, Compare) ->
    case {A, B} of
        {[], _@1} ->
            B;

        {_@2, []} ->
            A;

        {[Ax | Ar], [Bx | Br]} ->
            case Compare(Ax, Bx) of
                lt ->
                    [Ax | merge_sort(Ar, B, Compare)];

                _@3 ->
                    [Bx | merge_sort(A, Br, Compare)]
            end
    end.

-spec do_sort(list(PV), fun((PV, PV) -> gleam@order:order()), integer()) -> list(PV).
do_sort(List, Compare, List_length) ->
    case List_length < 2 of
        true ->
            List;

        false ->
            Split_length = List_length div 2,
            A_list = take(List, Split_length),
            B_list = drop(List, Split_length),
            merge_sort(
                do_sort(A_list, Compare, Split_length),
                do_sort(B_list, Compare, List_length - Split_length),
                Compare
            )
    end.

-spec sort(list(PY), fun((PY, PY) -> gleam@order:order())) -> list(PY).
sort(List, Compare) ->
    do_sort(List, Compare, length(List)).

-spec range(integer(), integer()) -> list(integer()).
range(Start, Stop) ->
    case gleam@int:compare(Start, Stop) of
        eq ->
            [];

        gt ->
            [Start | range(Start - 1, Stop)];

        lt ->
            [Start | range(Start + 1, Stop)]
    end.

-spec do_repeat(QC, integer(), list(QC)) -> list(QC).
do_repeat(A, Times, Acc) ->
    case Times =< 0 of
        true ->
            Acc;

        false ->
            do_repeat(A, Times - 1, [A | Acc])
    end.

-spec repeat(QF, integer()) -> list(QF).
repeat(A, Times) ->
    do_repeat(A, Times, []).

-spec do_split(list(QH), integer(), list(QH)) -> {list(QH), list(QH)}.
do_split(List, N, Taken) ->
    case N =< 0 of
        true ->
            {reverse(Taken), List};

        false ->
            case List of
                [] ->
                    {reverse(Taken), []};

                [X | Xs] ->
                    do_split(Xs, N - 1, [X | Taken])
            end
    end.

-spec split(list(QM), integer()) -> {list(QM), list(QM)}.
split(List, Index) ->
    do_split(List, Index, []).

-spec do_split_while(list(QQ), fun((QQ) -> boolean()), list(QQ)) -> {list(QQ),
                                                                     list(QQ)}.
do_split_while(List, F, Acc) ->
    case List of
        [] ->
            {reverse(Acc), []};

        [X | Xs] ->
            case F(X) of
                false ->
                    {reverse(Acc), List};

                _@1 ->
                    do_split_while(Xs, F, [X | Acc])
            end
    end.

-spec split_while(list(QV), fun((QV) -> boolean())) -> {list(QV), list(QV)}.
split_while(List, Predicate) ->
    do_split_while(List, Predicate, []).

-spec key_find(list({QZ, RA}), QZ) -> {ok, RA} | {error, nil}.
key_find(Keyword_list, Desired_key) ->
    find_map(
        Keyword_list,
        fun(Keyword) ->
            {Key, Value} = Keyword,
            case Key =:= Desired_key of
                true ->
                    {ok, Value};

                false ->
                    {error, nil}
            end
        end
    ).

-spec do_pop(list(RI), fun((RI) -> boolean()), list(RI)) -> {ok, {RI, list(RI)}} |
    {error, nil}.
do_pop(Haystack, Predicate, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Predicate(X) of
                true ->
                    {ok, {X, append(reverse(Checked), Rest)}};

                false ->
                    do_pop(Rest, Predicate, [X | Checked])
            end
    end.

-spec pop(list(RI), fun((RI) -> boolean())) -> {ok, {RI, list(RI)}} |
    {error, nil}.
pop(Haystack, Is_desired) ->
    do_pop(Haystack, Is_desired, []).

-spec do_pop_map(list(RR), fun((RR) -> {ok, RT} | {error, any()}), list(RR)) -> {ok,
                                                                                 {RT,
                                                                                  list(RR)}} |
    {error, nil}.
do_pop_map(Haystack, Mapper, Checked) ->
    case Haystack of
        [] ->
            {error, nil};

        [X | Rest] ->
            case Mapper(X) of
                {ok, Y} ->
                    {ok, {Y, append(reverse(Checked), Rest)}};

                {error, _@1} ->
                    do_pop_map(Rest, Mapper, [X | Checked])
            end
    end.

-spec pop_map(list(RR), fun((RR) -> {ok, RT} | {error, any()})) -> {ok,
                                                                    {RT,
                                                                     list(RR)}} |
    {error, nil}.
pop_map(Haystack, Is_desired) ->
    do_pop_map(Haystack, Is_desired, []).

-spec key_pop(list({SA, SB}), SA) -> {ok, {SB, list({SA, SB})}} | {error, nil}.
key_pop(Haystack, Key) ->
    pop_map(
        Haystack,
        fun(Entry) ->
            {K, V} = Entry,
            case K of
                K@1 when K@1 =:= Key ->
                    {ok, V};

                _@1 ->
                    {error, nil}
            end
        end
    ).

-spec key_set(list({SG, SH}), SG, SH) -> list({SG, SH}).
key_set(List, Key, Value) ->
    case List of
        [] ->
            [{Key, Value}];

        [{K, _@1} | Rest] when K =:= Key ->
            [{Key, Value} | Rest];

        [First | Rest@1] ->
            [First | key_set(Rest@1, Key, Value)]
    end.

-spec each(list(SK), fun((SK) -> any())) -> nil.
each(List, F) ->
    case List of
        [] ->
            nil;

        [X | Xs] ->
            F(X),
            each(Xs, F)
    end.

-spec do_partition(list(SS), fun((SS) -> boolean()), list(SS), list(SS)) -> {list(SS),
                                                                             list(SS)}.
do_partition(List, Categorise, Trues, Falses) ->
    case List of
        [] ->
            {reverse(Trues), reverse(Falses)};

        [X | Xs] ->
            case Categorise(X) of
                true ->
                    do_partition(Xs, Categorise, [X | Trues], Falses);

                false ->
                    do_partition(Xs, Categorise, Trues, [X | Falses])
            end
    end.

-spec partition(list(SS), fun((SS) -> boolean())) -> {list(SS), list(SS)}.
partition(List, Categorise) ->
    do_partition(List, Categorise, [], []).

-spec permutations(list(SW)) -> list(list(SW)).
permutations(L) ->
    case L of
        [] ->
            [[]];

        _@1 ->
            _pipe@2 = map(
                L,
                fun(X) ->
                    _pipe = filter(L, fun(Y) -> Y /= X end),
                    _pipe@1 = permutations(_pipe),
                    map(_pipe@1, fun(_capture) -> append([X], _capture) end)
                end
            ),
            flatten(_pipe@2)
    end.

-spec do_window(list(list(TA)), list(TA), integer()) -> list(list(TA)).
do_window(Acc, L, N) ->
    Window = take(L, N),
    case length(Window) =:= N of
        true ->
            do_window([Window | Acc], drop(L, 1), N);

        false ->
            Acc
    end.

-spec window(list(TG), integer()) -> list(list(TG)).
window(L, N) ->
    _pipe = do_window([], L, N),
    reverse(_pipe).

-spec window_by_2(list(TK)) -> list({TK, TK}).
window_by_2(L) ->
    zip(L, drop(L, 1)).

-spec drop_while(list(TN), fun((TN) -> boolean())) -> list(TN).
drop_while(List, Predicate) ->
    case List of
        [] ->
            [];

        [X | Xs] ->
            case Predicate(X) of
                true ->
                    drop_while(Xs, Predicate);

                false ->
                    [X | Xs]
            end
    end.

-spec do_take_while(list(TQ), fun((TQ) -> boolean()), list(TQ)) -> list(TQ).
do_take_while(List, Predicate, Acc) ->
    case List of
        [] ->
            reverse(Acc);

        [Head | Tail] ->
            case Predicate(Head) of
                true ->
                    do_take_while(Tail, Predicate, [Head | Acc]);

                false ->
                    reverse(Acc)
            end
    end.

-spec take_while(list(TU), fun((TU) -> boolean())) -> list(TU).
take_while(List, Predicate) ->
    do_take_while(List, Predicate, []).

-spec do_chunk(list(TX), fun((TX) -> TZ), TZ, list(TX), list(list(TX))) -> list(list(TX)).
do_chunk(List, F, Previous_key, Current_chunk, Acc) ->
    case List of
        [Head | Tail] ->
            Key = F(Head),
            case Key =:= Previous_key of
                false ->
                    New_acc = [reverse(Current_chunk) | Acc],
                    do_chunk(Tail, F, Key, [Head], New_acc);

                _@1 ->
                    do_chunk(Tail, F, Key, [Head | Current_chunk], Acc)
            end;

        _@2 ->
            reverse([reverse(Current_chunk) | Acc])
    end.

-spec chunk(list(UF), fun((UF) -> any())) -> list(list(UF)).
chunk(List, F) ->
    case List of
        [] ->
            [];

        [Head | Tail] ->
            do_chunk(Tail, F, F(Head), [Head], [])
    end.

-spec do_sized_chunk(list(UK), integer(), integer(), list(UK), list(list(UK))) -> list(list(UK)).
do_sized_chunk(List, Count, Left, Current_chunk, Acc) ->
    case List of
        [] ->
            case Current_chunk of
                [] ->
                    reverse(Acc);

                Remaining ->
                    reverse([reverse(Remaining) | Acc])
            end;

        [Head | Tail] ->
            Chunk = [Head | Current_chunk],
            case Left > 1 of
                false ->
                    do_sized_chunk(
                        Tail,
                        Count,
                        Count,
                        [],
                        [reverse(Chunk) | Acc]
                    );

                true ->
                    do_sized_chunk(Tail, Count, Left - 1, Chunk, Acc)
            end
    end.

-spec sized_chunk(list(UR), integer()) -> list(list(UR)).
sized_chunk(List, Count) ->
    do_sized_chunk(List, Count, Count, [], []).

-spec reduce(list(UV), fun((UV, UV) -> UV)) -> {ok, UV} | {error, nil}.
reduce(List, Fun) ->
    case List of
        [] ->
            {error, nil};

        [Head | Tail] ->
            {ok, fold(Tail, Head, Fun)}
    end.

-spec do_scan(list(UZ), VB, list(VB), fun((VB, UZ) -> VB)) -> list(VB).
do_scan(List, Accumulator, Accumulated, Fun) ->
    case List of
        [] ->
            reverse(Accumulated);

        [X | Xs] ->
            Next = Fun(Accumulator, X),
            do_scan(Xs, Next, [Next | Accumulated], Fun)
    end.

-spec scan(list(VE), VG, fun((VG, VE) -> VG)) -> list(VG).
scan(List, Initial, Fun) ->
    do_scan(List, Initial, [], Fun).

-spec last(list(VI)) -> {ok, VI} | {error, nil}.
last(List) ->
    _pipe = List,
    reduce(_pipe, fun(_, Elem) -> Elem end).

-spec combinations(list(VM), integer()) -> list(list(VM)).
combinations(Items, N) ->
    case N of
        0 ->
            [[]];

        _@1 ->
            case Items of
                [] ->
                    [];

                [X | Xs] ->
                    First_combinations = begin
                        _pipe = map(
                            combinations(Xs, N - 1),
                            fun(Com) -> [X | Com] end
                        ),
                        reverse(_pipe)
                    end,
                    fold(
                        First_combinations,
                        combinations(Xs, N),
                        fun(Acc, C) -> [C | Acc] end
                    )
            end
    end.

-spec do_combination_pairs(list(VQ)) -> list(list({VQ, VQ})).
do_combination_pairs(Items) ->
    case Items of
        [] ->
            [];

        [X | Xs] ->
            First_combinations = map(Xs, fun(Other) -> {X, Other} end),
            [First_combinations | do_combination_pairs(Xs)]
    end.

-spec combination_pairs(list(VU)) -> list({VU, VU}).
combination_pairs(Items) ->
    _pipe = do_combination_pairs(Items),
    flatten(_pipe).

-spec interleave(list(list(VX))) -> list(VX).
interleave(List) ->
    _pipe = transpose(List),
    flatten(_pipe).

-spec transpose(list(list(WB))) -> list(list(WB)).
transpose(List_of_list) ->
    Take_first = fun(List) -> case List of
            [] ->
                [];

            [F] ->
                [F];

            [F@1 | _@1] ->
                [F@1]
        end end,
    case List_of_list of
        [] ->
            [];

        [[] | Xss] ->
            transpose(Xss);

        Rows ->
            Firsts = begin
                _pipe = Rows,
                _pipe@1 = map(_pipe, Take_first),
                flatten(_pipe@1)
            end,
            Rest = transpose(map(Rows, fun(_capture) -> drop(_capture, 1) end)),
            [Firsts | Rest]
    end.
