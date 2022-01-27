
# Compiler Course

[![Antar workflow](https://github.com/hasantouma/antar/actions/workflows/workflow.yml/badge.svg?branch=main)](https://github.com/hasantouma/antar/actions/workflows/workflow.yml)

* GitHub Gist: https://gist.github.com/camoy/522230d19f546b7f5994189482eab8db
* Course website: http://jeapostrophe.github.io/courses/2021/spring/406/course/

## Building the compiler
Simply run `make build` and look for an `hcc` binary file
Note: `hcc` is a joke off of `gcc` :)

## Running `hcc`
If you simply type `$ ./hcc` it will open a REPL where you can type valid `rlang` expressions.

Running `$ ./hcc [-help|--help]` will list all the valid commands.

```shell
$ ./hcc --help
'Antar R1' programming language
  -f <file_path> Parsing file
  -g <int> Generate random program of size n
  -gv <int> Generate, and visualize, random program of size n
  -v <file_path> File to visualize. Will output a 'mygraph.png' file.
  -S Output X86 assembly file as <input_file>.s
  -o Set output file name
  -opt Output rlang file with Optimize pass as <input_file>.opt.ht
  -uni Output rlang file with Uniquify pass as <input_file>.uni.ht
  -rco Output rlang file with Resolve-Complex pass as <input_file>.rco.ht
  -econ Output clang file with Explicate-Control pass as <input_file>.econ.ht
  -si Output xlang file with Select-Instruction pass as <input_file>.si.ht
  -ah Output xlang file with Assign-Homes pass as <input_file>.ah.ht
  -pi Output xlang file with Patch-Instructions pass as <input_file>.pi.ht
  -help  Display this list of options
  --help  Display this list of options
```

## `dune-deps` graph

[dune-deps GitHub source code](https://github.com/mjambon/dune-deps)

![graph for antar dependencies](img/deps.png)

### Helpful links
* Dune project: https://medium.com/@bobbypriambodo/starting-an-ocaml-app-project-using-dune-d4f74e291de8

