export CC := /usr/local/opt/llvm/bin/clang

USE_BUNDLED_DEPS  := OFF
CMAKE_BUILD_TYPE  := Debug
CMAKE_EXTRA_FLAGS := -DBUSTED_OUTPUT_TYPE=gtest

.PHONY: all
default: all

.PHONY: info
info:
	@awk -F = '/BUILD_TYPE:/ { print "BUILD_TYPE:", $$2 }' build/CMakeCache.txt
	@awk -F = '/C_COMPILER:/ { print "C_COMPILER:", $$2 }' build/CMakeCache.txt

.PHONY: release
release:
	$(MAKE) CMAKE_BUILD_TYPE=RelWithDebInfo

rebuild-%:
	rm -f .deps/build/src/$*-stamp/$*-build
	$(MAKE) deps

.PHONY: cl
cl:
	rm -f errors.json
	wget https://raw.githubusercontent.com/neovim/doc/gh-pages/reports/clint/errors.json
	$(eval FILES = $(shell git diff-tree --name-only --no-commit-id -r master..))
	src/clint.py --suppress-errors=errors.json ${FILES}
