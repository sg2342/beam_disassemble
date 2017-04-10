-module(beam_disassemble).

-export([main/1]).
-export([do/1]).

-define(EXT, ".S").

-include("x_beam_disasm.hrl").

main(["--regression-test"]) ->
    AllBeamsInPath = lists:append(
		       lists:map(fun(Dir) ->
				        filelib:wildcard(
				          filename:join(Dir, "*.beam")) end,
			        code:get_path())),
    regression_test(AllBeamsInPath);

main(L) ->
    lists:foreach(
      fun(BeamFile) ->
	      R  = do(BeamFile), io:format("~p~n",[R])
      end,
      L).

regression_test(BeamFiles) ->
    Self = self(),
    Pids = [spawn(fun() -> test_one(Self, B) end) || B <- BeamFiles],
    regression_test1(lists:zip(Pids, BeamFiles), {[],[]}).

regression_test1([], {Ok, Error}) -> report(Ok, Error, []);
regression_test1(Pids0, {Ok, Error}) ->
   receive
       {P, ok} ->
	   {value, {P, Beam}, Pids} = lists:keytake(P,1,Pids0),
	   regression_test1(Pids, {[Beam | Ok], Error});
       {P, {failed, E}} ->
	   {value, {P, Beam}, Pids} = lists:keytake(P,1,Pids0),
	   regression_test1(Pids,{Ok, [{Beam, E}| Error]})
   after timer:minutes(3) ->
	   {_, TimedOut} = lists:unzip(Pids0),
	   report(Ok, Error, TimedOut),
	   halt(1)
   end.

report(Ok, Error, TimedOut) ->
    {ok, F} = file:open("regression_test.log", [write]),
    io:fwrite(F, "Timed out: ~p~nError: ~p~nOk: ~p~n~n",
	      [length(TimedOut), length(Error), length(Ok)]),
    io:fwrite(F, "~n============ Timed out =========================~n",[]),
    lists:foreach(fun(B) -> io:fwrite(F,"~s~n",[B]) end, TimedOut),
    io:fwrite(F, "~n============ Error =============================~n",[]),
    lists:foreach(fun({B, E}) -> io:fwrite(F,"~s~n~p~n~n", [B, E]) end, Error),
    io:fwrite(F, "~n============ Ok ================================~n",[]),
    lists:foreach(fun(B) -> io:fwrite(F,"~s~n",[B]) end, Ok),
    file:close(F).

test_one(Collector, Beam) ->
    try
	case do(Beam) of
	    {ok, Mod, []} ->
		ok = file:delete(atom_to_list(Mod) ++ ?EXT),
		Collector ! {self(), ok};
	    {_, _,_} = E ->
		Collector ! {self(), {failed, E}}
	end
    catch C:Ex ->
	    Collector ! {self(), {failed, {exception, {C,Ex}}}}
    end.

-spec do(BeamFile :: file:filename()) ->
    {ok, Module :: atom(), [_]} | {error, Module :: atom(), Reason :: term()}.
do(BeamFile) -> do1(x_beam_disasm:file(BeamFile)).

do1({beam_file, Mod, Exp0, Attr0, _Info, NumLabels, Code0}) ->
    Exp = lists:map(fun fix_exp/1, Exp0),
    {Attr, Code} = fix_on_load(Attr0, Code0),
    MaybeOk = file:open(atom_to_list(Mod) ++ ?EXT,
			[write]),
    do2(MaybeOk, {Mod, Exp, Attr, Code, NumLabels});
do1(E) -> E.

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
