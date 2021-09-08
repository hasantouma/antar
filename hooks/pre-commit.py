#!/usr/bin/python

import subprocess

print("Running Git pre-commit hook!")

try:
    cmd = ["dune", "build", "@fmt"]
    output = subprocess.check_output(cmd)
except subprocess.CalledProcessError as exc:
    print("Status: FAIL")
    print("Command:", cmd)
    print("Return code:", exc.returncode)
    print("Output:", exc.output)

    exit(exc.returncode)
else:
    print("Status: Success")

