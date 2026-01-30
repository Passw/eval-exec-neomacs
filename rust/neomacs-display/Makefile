# Neomacs Display Engine Build Configuration

# Note: If you have build-std enabled in ~/.cargo/config.toml, you need to
# temporarily disable it or build will fail with duplicate lang item errors.
#
# Quick fix: Run `make setup` or `make test` which handles this automatically.

CARGO := cargo
RUST_TOOLCHAIN := +nightly
CARGO_HOME := $(HOME)/.cargo
BACKUP_CONFIG := $(CARGO_HOME)/config.toml.neomacs.bak

.PHONY: all build test clean setup restore check

all: build

# Setup: backup global config if it has build-std
setup:
	@if grep -q "build-std" $(CARGO_HOME)/config.toml 2>/dev/null; then \
		echo "Backing up global cargo config (has build-std)..."; \
		cp $(CARGO_HOME)/config.toml $(BACKUP_CONFIG); \
		sed -i '/build-std/d' $(CARGO_HOME)/config.toml; \
	fi

# Restore global config
restore:
	@if [ -f $(BACKUP_CONFIG) ]; then \
		echo "Restoring global cargo config..."; \
		mv $(BACKUP_CONFIG) $(CARGO_HOME)/config.toml; \
	fi

# Build (in nix-shell for dependencies)
build: setup
	nix-shell --run "$(CARGO) $(RUST_TOOLCHAIN) build"
	@$(MAKE) restore

# Build release
build-release: setup
	nix-shell --run "$(CARGO) $(RUST_TOOLCHAIN) build --release"
	@$(MAKE) restore

# Run tests
test: setup
	nix-shell --run "$(CARGO) $(RUST_TOOLCHAIN) test"
	@$(MAKE) restore

# Check (faster than build)
check: setup
	nix-shell --run "$(CARGO) $(RUST_TOOLCHAIN) check"
	@$(MAKE) restore

# Clean build artifacts
clean:
	rm -rf target

# Generate C header
header: build
	@mkdir -p include
	nix-shell --run "cbindgen --config cbindgen.toml --crate neomacs-display --output include/neomacs_display.h"

# Format code
fmt:
	nix-shell --run "$(CARGO) $(RUST_TOOLCHAIN) fmt"

# Lint
clippy: setup
	nix-shell --run "$(CARGO) $(RUST_TOOLCHAIN) clippy"
	@$(MAKE) restore

# Documentation
doc: setup
	nix-shell --run "$(CARGO) $(RUST_TOOLCHAIN) doc --no-deps"
	@$(MAKE) restore
