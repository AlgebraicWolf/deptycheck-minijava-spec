export IDRIS2 ?= idris2
export EXEC_PATH ?= build/exec/

.PHONY: all build clean

all: build

build:
	${IDRIS2} --build real-thing-deptycheck-tested.ipkg

run: build
	@echo "Running..."
	@${EXEC_PATH}real-thing-deptycheck-tested

clean:
	${IDRIS2} --clean real-thing-deptycheck-tested.ipkg
	${RM} -r build
