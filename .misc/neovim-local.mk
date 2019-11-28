CMAKE_BUILD_TYPE  := Debug
CMAKE_EXTRA_FLAGS := -DCMAKE_INSTALL_PREFIX=${HOME}/local/nvim
USE_BUNDLED       := ON

# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_GETTEXT=OFF
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_GPERF=OFF
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_LIBICONV=OFF
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_LIBTERMKEY=OFF
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_LUA=OFF
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_MSGPACK=OFF
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_UNIBILIUM=OFF

# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_LIBUV=ON
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_BUSTED=ON
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_LUAJIT=ON
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_LUAROCKS=ON

# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_LIBVTERM=ON
# DEPS_CMAKE_FLAGS  += -DUSE_BUNDLED_LUV=ON

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
	DEBUG=1 $(MAKE) deps

.PHONY: cl
cl:
	rm -f errors.json
	wget https://raw.githubusercontent.com/neovim/doc/gh-pages/reports/clint/errors.json
	$(eval FILES = $(shell git diff-tree --name-only --no-commit-id -r master..))
	src/clint.py --suppress-errors=errors.json ${FILES}
