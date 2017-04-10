-module(beam_disassemble).

-export([main/1]).
-export([do/1]).

-define(EXT, ".S").

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
do1({beam_file, Mod, Exp0, Attr, _Info, NumLabels, Code}) ->
    Exp = lists:map(fun fix_exp/1, Exp0),
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
