import json
import os
import shutil
import subprocess
import sys


if len(sys.argv) != 2:
    print("Usage:")
    print("\t./harness.py <path-to-test-directory>")
    exit(0)

testdir = sys.argv[1]


if not os.access(testdir + "/settings.json", os.R_OK):
    raise FileNotFoundError("settings.json either does not exist, or program does not have permission to read it")

f = open(testdir + "/settings.json", "r")
settings = json.load(f)

if "executables" not in settings:
    raise ValueError("list of required executables is not present")

if "stages" not in settings:
    raise ValueError("list of stages is not present")

for stage in settings['stages']:
    if stage not in settings:
        raise ValueError("description of stage \"{}\" is not present".format(stage))

print("Checking environment...")
for executable in settings["executables"]:
    if shutil.which(executable) is None:
        raise ValueError("can't find executable \"{}\"".format(executable))
print("All the executables were found, moving on to testing")


# Output looks like this

# +--------+---------+---------+-----+---------+
# | Tes    | Stage 1 | Stage 2 | ... | Stage N |
# +--------+---------+---------+-----+---------+
# | Test 0 | OK      | OK      |     | OK      |
# | Test 1 | OK      | FAIL    |     | SKIP    |
