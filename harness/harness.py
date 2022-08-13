import json
import os
import shutil
import subprocess
import sys
from termcolor import colored


def text_col_len(string):
    return len(string) + 2


def print_row(text, width):
    text = [" " + msg + " " for msg in text]
    text = [msg + " " * (w - len(msg)) for (msg, w) in zip(text, width)]
    print("|" + "|".join(text) + "|")


def print_sep(width):
    lines = ["-" * w for w in width]
    print("+" + "+".join(lines) + "+")


def verify_output(test, stage, result, expected, testdir):
    errors = []
    if expected['returned'] is not None:
        if str(result.returncode) != expected['returned']:
            errors.append(
                ("return code", expected['returned'], result.returncode, test, stage))

    if expected['stdout'] is not None and expected['stdout']['type'] == 'compare':
        if not os.access(expected['stdout']['value'], os.R_OK):
            errors.append(
                    ("stdout", "File Not Found", "", test, stage))
        else:
            f = open(expected['stdout']['value'], "rb")
            value = f.read()
            if result.stdout != value:
                errors.append(
                    ("stdout", value.decode('utf-8'), result.stdout.decode('utf-8'), test, stage))

    if expected['stderr'] is not None and expected['stderr']['type'] == 'compare':
        if not os.access(expected['stderr']['value'], os.R_OK):
            errors.append(
                    ("stderr", "File Not Fount", "", test, stage))
        else:
            f = open(expected['stderr']['value'], "rb")
            value = f.read()
            if result.stderr != value:
                errors.append(
                    ("stderr", value.decode('utf-8'), result.stderr.decode('utf-8'), test, stage))

    return errors


def pick_out_dst(config):
    if config is None:
        return subprocess.DEVNULL

    match config['type']:
        case 'write':
            return open(config['value'], 'wb')
        case 'compare':
            return subprocess.PIPE

    return subprocess.DEVNULL


def process_test_suite(testdir):
    if not os.access(testdir + "settings.json", os.R_OK):
        raise FileNotFoundError(
            "settings.json either does not exist, or program does not have permission to read it")

    f = open(testdir + "settings.json", "r")
    settings = json.load(f)

    if "executables" not in settings:
        raise ValueError("list of required executables is not present")

    if "stages" not in settings:
        raise ValueError("list of stages is not present")

    if "tests" not in settings:
        raise ValueError("list of tests is not present")

    for stage in settings['stages']:
        if stage not in settings:
            raise ValueError(
                "description of stage \"{}\" is not present".format(stage))

    print("Checking environment...")
    for executable in settings["executables"]:
        if shutil.which(executable) is None:
            raise ValueError("can't find executable \"{}\"".format(executable))
    print("All the executables were found, moving on to testing")
    print()

    TEST_NAME_COL_HEADER = "Test Name"
    OK_MSG = "OK"
    FAIL_MSG = "FAIL"
    SKIP_MSG = "SKIP"

    max_msg_len = max(text_col_len(OK_MSG), text_col_len(
        FAIL_MSG), text_col_len(SKIP_MSG))

    col_length = [max(text_col_len(TEST_NAME_COL_HEADER), max(map(text_col_len, settings["tests"])))
                  ] + [max(text_col_len(stage), max_msg_len) for stage in settings["stages"]]
    col_length_color = [w + 9 for w in col_length]
    col_length_color[0] -= 9

    header_row = [TEST_NAME_COL_HEADER] + settings["stages"]

    print_sep(col_length)
    print_row(header_row, col_length)
    print_sep(col_length)

    errors = list()

    for test in settings["tests"]:
        if not os.access(testdir + test + '.json', os.R_OK):
            print(
                test + ".json either does not exist or the program does not have permission to read it")
            continue

        f = open(testdir + test + ".json", "r")
        test_config = json.load(f)

        for stage in settings['stages']:
            if test_config[stage]['stdout'] is not None:
                test_config[stage]['stdout']['value'] = testdir + test_config[stage]['stdout']['value']
            if test_config[stage]['stderr'] is not None:
                test_config[stage]['stderr']['value'] = testdir + test_config[stage]['stderr']['value']

        variables = test_config["vars"]
        variables["TESTNAME"] = test

        output_row = [test]

        for stage in settings["stages"]:
            executable = settings[stage].format(**variables)

            stdout_dst = pick_out_dst(test_config[stage]['stdout'])
            stderr_dst = pick_out_dst(test_config[stage]['stderr'])

            result = subprocess.run(executable.split(' '), stdout=stdout_dst, stderr=stderr_dst)
            new_errors = verify_output(test, stage, result, test_config[stage], testdir)

            if len(new_errors) != 0:
                output_row.append(colored(FAIL_MSG, 'red'))
                errors += new_errors
                break

            output_row.append(colored(OK_MSG, 'green'))

        output_row += [colored(SKIP_MSG, 'yellow')] * \
            (len(col_length) - len(output_row))
        print_row(output_row, col_length_color)

    print_sep(col_length)
    print()

    if len(errors) == 0:
        print("No errors were found")
        return

    for (ty, expected, got, test, stage) in errors:
        print("Test: {}, stage: {}, mismatch in {}".format(test, stage, ty))
        print("Expected: \"{}\"".format(expected))
        print("Got: \"{}\"".format(got))
        print()


if __name__ == "__main__":
    if len(sys.argv) == 1:
        print("Usage:")
        print("\t./harness.py <path-to-test-directory>")
        exit(0)

    for testdir in sys.argv[1:]:
        if testdir[-1] != '/':
            testdir += '/'

        print()
        print("Processing test suite {}".format(testdir))
        process_test_suite(testdir)
