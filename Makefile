export IDRIS2 ?= idris2
export EXEC_PATH ?= build/exec/
export PYTHON ?= python

.PHONY: all build clean

all: build

build:
	${IDRIS2} --build real-thing-deptycheck-tested.ipkg

run_gen: build
	@echo "Running..."
	@${EXEC_PATH}real-thing-deptycheck-tested

run: build
	@${PYTHON} harness/run.py

clean:
	${IDRIS2} --clean real-thing-deptycheck-tested.ipkg
	${RM} -r build
