beam_disassemble
=====

BEAM file dis-assembler

Based on `compiler/src/beam_disasm.erl` (special purpose BEAM dis-assembler
in the OTP Compiler application. used by HiPE).

Disassemble `a_module.beam` in order to get `a_module.S` which
can be (modified and then) compiled with `erlc a_module.S`.


Build
-----

```
    $ rebar3 escriptize
```

results in `_build/default/bin/beam_disassemble`

Usage
----

```
    beam_disassemble path/to/beam_file1.beam path/to/beam_file2.beam ...
```

disassemble the BEAM(s) and create `.S` file(s) in the current directory.
`beam_disassemble` will try to validate the results (with`compile:file/2`)

```
   beam_disassemble --regression-test
```

find all BEAM files in code path, disassemble and validate.

write `regression_test.log` and `.S` files of modules that failed
validation to the current directory.


Limitations
----

**"scratch an itch"-software: will not be maintained/supported**

The format of assembler files is not documented, and can change between
OTP releases.

When running the regression test on an OTP 19.3.1 installation, it was found
that:


* `beam_type.S` from `lib/compiler-7.0.4/ebin/beam_type.beam` does not
validate, but if a `beam_type.beam` is created with
`erlc -I lib/compiler-7.0.4/src lib/compiler-7.0.4` and then
disassembled, the result validates.

* `sys_core_fold.S` from `lib/compiler-7.0.4/ebin/sys_core_fold.beam`
does not validate, but an `sys_core_fold.S` created by
`erlc -S lib/compiler-7.0.4/src/sys_core_fold.beam` does also fail to
 compile (identical error).

* `public_key.S` from `lib/public_key-1.4/ebin/public_key.beam` does
not validate, but an `public_key.S` created by
`erlc -S -I lib/public_key-1.4/include lib/public_key-1.4/src/public_key.erl`
does also fail to compile (identical error).

* `test_server_node.S` from `lib/common_test-1.114/ebin/test_server_node.beam`
does not validate, but an `test_server_node.S` created by
`erlc -S -I lib/common_test-1.114/src lib/common_test-1.114/src/test_server_node.erl`
does also fail to compile (identical error)

* `wx_object.S` from `lib/wx-1.8/ebin/wx_object.beam` does not validate, but an
`wx_object.S` created by `erlc -S -I lib/wx-1.8/include lib/wx-1.8/src/wx_object.erl`
doea also fail to compile (identical error).
