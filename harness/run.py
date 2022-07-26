import subprocess
from harness import process_test_suite

if __name__ == "__main__":
    output_dir = input("Directory for test suite: ")
    num_tests = input("Number of tests: ")
    skip = input("Number of tests to skip during generation: ")
    fuel = input("Fuel for generator: ")

    print("Running generator:")
    subprocess.run("build/exec/real-thing-deptycheck-tested -o {} -n {} -s {} -f {}".format(output_dir, num_tests, skip, fuel).split(' '))
    print("Generation finished, running tests:")
    if output_dir[-1] != "/":
        output_dir += "/"
        
    process_test_suite(output_dir)
    exit(0)
