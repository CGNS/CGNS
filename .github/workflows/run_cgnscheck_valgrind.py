#!/usr/bin/env python3
"""Re-run every cgnscheck_* CTest entry directly under valgrind memcheck.

Assumes the normal test suite has already been run once (via `ctest`) so all
fixture .cgns files exist. Does not rely on CTest's own memcheck/dashboard
machinery (this project uses plain enable_testing(), not include(CTest), so
DartConfiguration.tcl is unavailable) -- instead re-derives each test's exact
command and working directory from `ctest --show-only=json-v1` and re-invokes
it wrapped in valgrind.
"""
import json
import subprocess
import sys

build_dir = sys.argv[1] if len(sys.argv) > 1 else "."
suppressions = sys.argv[2] if len(sys.argv) > 2 else None

result = subprocess.run(
    ["ctest", "--show-only=json-v1"], cwd=build_dir, capture_output=True, text=True
)
data = json.loads(result.stdout)
tests = [t for t in data["tests"] if t["name"].startswith("cgnscheck")]

print(f"Found {len(tests)} cgnscheck_* tests to run under valgrind", flush=True)

failures = []
for t in tests:
    name = t["name"]
    command = t["command"]
    props = {p["name"]: p["value"] for p in t.get("properties", [])}
    wd = props.get("WORKING_DIRECTORY", build_dir)

    vg_cmd = [
        "valgrind",
        "--leak-check=full",
        "--error-exitcode=97",
        "--track-origins=yes",
        "-q",
    ]
    if suppressions:
        vg_cmd.append(f"--suppressions={suppressions}")
    vg_cmd += command

    r = subprocess.run(vg_cmd, cwd=wd, capture_output=True, text=True, timeout=120)
    if r.returncode == 97:
        print(f"FAIL (valgrind): {name}", flush=True)
        print(r.stderr[-4000:])
        failures.append(name)
    else:
        print(f"ok: {name}", flush=True)

if failures:
    print(f"\n{len(failures)} test(s) failed under valgrind: {failures}")
    sys.exit(1)
print(f"\nAll {len(tests)} cgnscheck tests clean under valgrind.")
