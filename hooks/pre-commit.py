#!/usr/bin/python

import subprocess

print("Running Git pre-commit hook!")

try:
    cmd = ["make", "graph"]
    print("Running 'make graph' to update deps graph.")
    output = subprocess.check_output(cmd)

    cmd = ["dune", "clean"]
    print("Running 'dune clean' to clean up after 'make graph'.")
    output = subprocess.check_output(cmd)

    cmd = ["dune", "build", "@fmt"]
    print("Running 'dune build @fmt' to format all OCaml files..")
    output = subprocess.check_output(cmd)

except subprocess.CalledProcessError as exc:
    print("Status: FAIL")
    print("Command:", cmd)
    print("Return code:", exc.returncode)
    print("Output:", exc.output)
    print("If cmd is '@fmt' then run command: dune promote")

    exit(exc.returncode)
else:
    print("Status: Success")

