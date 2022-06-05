-module(gleam@function).
-compile(no_auto_import).

-export([compose/2, curry2/1, curry3/1, curry4/1, curry5/1, curry6/1, flip/1, identity/1, constant/1, tap/2]).

-spec compose(fun((Z) -> AA), fun((AA) -> AB)) -> fun((Z) -> AB).
compose(Fun1, Fun2) ->
    fun(A) -> Fun2(Fun1(A)) end.

-spec curry2(fun((AC, AD) -> AE)) -> fun((AC) -> fun((AD) -> AE)).
curry2(Fun) ->
    fun(A) -> fun(B) -> Fun(A, B) end end.

-spec curry3(fun((AG, AH, AI) -> AJ)) -> fun((AG) -> fun((AH) -> fun((AI) -> AJ))).
curry3(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> Fun(A, B, C) end end end.

-spec curry4(fun((AL, AM, AN, AO) -> AP)) -> fun((AL) -> fun((AM) -> fun((AN) -> fun((AO) -> AP)))).
curry4(Fun) ->
    fun(A) -> fun(B) -> fun(C) -> fun(D) -> Fun(A, B, C, D) end end end end.

-spec curry5(fun((AR, AS, AT, AU, AV) -> AW)) -> fun((AR) -> fun((AS) -> fun((AT) -> fun((AU) -> fun((AV) -> AW))))).
curry5(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) -> fun(D) -> fun(E) -> Fun(A, B, C, D, E) end end end
        end
    end.

-spec curry6(fun((AY, AZ, BA, BB, BC, BD) -> BE)) -> fun((AY) -> fun((AZ) -> fun((BA) -> fun((BB) -> fun((BC) -> fun((BD) -> BE)))))).
curry6(Fun) ->
    fun(A) ->
        fun(B) ->
            fun(C) ->
                fun(D) -> fun(E) -> fun(F) -> Fun(A, B, C, D, E, F) end end end
            end
        end
    end.

-spec flip(fun((BG, BH) -> BI)) -> fun((BH, BG) -> BI).
flip(Fun) ->
    fun(B, A) -> Fun(A, B) end.

-spec identity(BJ) -> BJ.
identity(X) ->
    X.

-spec constant(BK) -> fun((any()) -> BK).
constant(Value) ->
    fun(_) -> Value end.

-spec tap(BM, fun((BM) -> any())) -> BM.
tap(Arg, Effect) ->
    Effect(Arg),
    Arg.
