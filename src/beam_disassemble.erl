-module(beam_disassemble).

-export([main/1]).
-export([do/1]).

-define(EXT, ".S").

-include("x_beam_disasm.hrl").

main(L) ->
    HandleResult = maybe_delete_s(os:getenv("HUNT_REGRESSIONS")),
    lists:foreach(
      fun(BeamFile) ->
	      HandleResult(do(BeamFile))
      end,
      L).

maybe_delete_s(false) -> fun(X) -> io:format("~p~n",[X]) end;
maybe_delete_s(_) ->
    fun({ok, Mod, _}) ->file:delete(atom_to_list(Mod) ++ ?EXT);
       (X) -> io:format("~n~n~p~n~n",[X]) end.


-spec do(BeamFile :: file:filename()) ->
    {ok, Module :: atom(), [_]} | {error, Module :: atom(), Reason :: term()}.
do(BeamFile) -> do1(x_beam_disasm:file(BeamFile)).

do1({error, _, _} = E) -> E;
do1({beam_file, Mod, Exp0, Attr0, _Info, NumLabels, Code0}) ->
    Exp = lists:map(fun fix_exp/1, Exp0),
    {Attr, Code} = fix_on_load(Attr0, Code0),
    MaybeOk = file:open(atom_to_list(Mod) ++ ?EXT,
			[write]),
    do2(MaybeOk, {Mod, Exp, Attr, Code, NumLabels}).

do2({error, E}, _) -> {error, file, E};
do2({ok, Out}, {Mod, _, _, _, _} = Args) ->
    ok = io:setopts(Out, [{encoding, utf8}]),
    ok = x_beam_listing:module(Out, Args),
    file:close(Out),
    compile:file(Mod, [from_asm, strong_validation, verbose, return]).

fix_exp({F, A,_L}) -> {F,A}.

fix_on_load(Attr, Code) ->
    MaybeEmpty =
	lists:filter(
	  fun(#function{code = C}) -> lists:member(on_load, C) end, Code),
    fix_on_load(MaybeEmpty, Attr, Code, []).

fix_on_load([], Attr, Code, []) -> {Attr, Code};
fix_on_load([], Attr0, Code, L) -> {[{on_load, L}| Attr0], Code};
fix_on_load([#function{name = Name, arity = Arity, code = C} = F | T],
	    Attr, Code0, Acc) ->
    Code = lists:keyreplace(Name, #function.name, Code0,
			    F#function{code = lists:delete(on_load, C)}),
    fix_on_load(T, Attr, Code, [{Name, Arity}|Acc]).
