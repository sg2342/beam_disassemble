-module(beam_disassemble).

-export([do/1]).


-spec do(BeamFile :: file:filename()) ->
    ok | {error, Module :: atom(), Reason :: term()}.
do(BeamFile) -> do1(x_beam_disasm:file(BeamFile)).

do1({error, _, _} = E) -> E;
do1({beam_file, Mod, Exp0, Attr, _Info, Code}) ->
    NumLabels = count_labels(Code),  %% do this in x_beam_disasm !
    Exp = lists:map(fun fix_exp/1, Exp0), %% do this in x_beam_disasm !
    MaybeOk = file:open(atom_to_list(Mod) ++ ".S", %% encoding ?
			[write]),
    do2(MaybeOk, {Mod, Exp, Attr, Code, NumLabels}).

do2({error, E}, _) -> {error, file, E};
do2({ok, Out}, Args) ->
    ok = x_beam_listing:module(Out, Args),
    file:close(Out).

fix_exp({F, A,_L}) -> {F,A}.

count_labels(Code) -> count_labels(Code, 0).

count_labels([], Acc) -> Acc;
count_labels(label, Acc) -> Acc + 1;
count_labels([H|T], Acc) -> count_labels(T, count_labels(H, Acc));
count_labels(Tuple, Acc) when is_tuple(Tuple) ->
    lists:foldl(fun count_labels/2, Acc, tuple_to_list(Tuple));
count_labels(_, Acc) -> Acc.
