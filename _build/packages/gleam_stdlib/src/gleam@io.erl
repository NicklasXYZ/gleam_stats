-module(gleam@io).
-compile(no_auto_import).

-export([print/1, println/1, debug/1]).
-export_type([do_not_leak/0]).

-type do_not_leak() :: any().

-spec print(binary()) -> nil.
print(String) ->
    gleam_stdlib:print(String).

-spec println(binary()) -> nil.
println(String) ->
    gleam_stdlib:println(String).

-spec debug(EEU) -> EEU.
debug(Term) ->
    debug_print(Term),
    Term.

-spec debug_print(any()) -> do_not_leak().
debug_print(Term) ->
    io:fwrite(<<"~tp\n"/utf8>>, [Term]).
