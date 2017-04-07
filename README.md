beam_disassemble
=====

BEAM file disassembler


Disassemble `a_module.beam` in order to get `a_module.S` which
can be (modified and then) compiled with `erlc a_module.S`.

Based on `compiler/src/beam_disasm.erl` (special purpose BEAM disassembler
in the Erlang Compiler application. used by HiPE).

Build
-----

    $ rebar3 compile
