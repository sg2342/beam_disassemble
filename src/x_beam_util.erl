-module(x_beam_util).

-export([lines_of_bin/2]).
-compile(export_all).
-include_lib("compiler/src/beam_opcodes.hrl").

lines_of_bin(Bin, Mod) ->
    try lines_of_bin1(Bin, Mod)
    catch _C:_E ->
	    io:format("could not decode line table: ~p~n",[Mod]),
	    undefined
    end.

lines_of_bin1(<<0:32,  %% Version
	       0:32,  %% Flags
	       _NumLineInstrs:32,
	       NumLines:32,
	       NumFnames:32,
	       Bin0/binary>>, Mod) ->
    {Ls0, Bin} = lines(NumLines, Bin0, [], 0),
    Fns = fnames(NumFnames, Bin, []),
    Ls1 = lists:map(fun({I, 0}) -> [{location, atom_to_list(Mod) ++ ".erl", I}];
		       ({I, A}) -> [{location, lists:nth(A, Fns), I}]
		    end, Ls0),
    Ls2 = lists:zip(lists:seq(1, length(Ls1)), Ls1),
    gb_trees:from_orddict([{0,[]} | Ls2]).


fnames(0, <<>>, Fns) -> lists:reverse(Fns);
fnames(NumFnames, <<S:16, Fn:S/binary, Bin/binary>>, Fns) ->
    fnames(NumFnames - 1, Bin, [binary_to_list(Fn)|Fns]).


lines(0, Bin, Is, _) -> {lists:reverse(Is), Bin};
lines(NumLines, <<B:8, Bs0/binary>>, Is, _) when (B band 2#111) =:= ?tag_a ->
    {A, Bs} = decode_int(B, Bs0),
    lines(NumLines, Bs, Is, A);
lines(NumLines, <<B:8, Bs0/binary>>, Is, A) when (B band 2#111) =:= ?tag_i ->
    {I, Bs} = decode_int(B, Bs0),
    lines(NumLines - 1, Bs, [{I, A} | Is], A).


%% negative or extreme large line numbers are not handled here
decode_int(B, Bs)                  when (B band 16#08) =:= 0 ->
    {B bsr 4, Bs};
decode_int(B, <<B1:8, Bs/binary>>) when (B band 16#10) =:= 0 ->
    {((B band 2#11100000) bsl 3) bor B1, Bs};
decode_int(B, Bs0)                 when (B bsr 5) < 7 ->
    S = ((B bsr 5) +2) * 8,
    <<N:S/integer, Bs/binary>> = Bs0,
    {N, Bs}.
