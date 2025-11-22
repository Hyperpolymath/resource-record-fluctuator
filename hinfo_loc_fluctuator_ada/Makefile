# Makefile for HINFO-LOC Fluctuator
# Provides convenient build targets for different modes

PROJECT = hinfo_loc_fluctuator
GPR_FILE = hinfo_loc_fluctuator.gpr
EXEC = bin/hinfo_loc_fluctuator

# Default target: build in debug mode
.PHONY: all
all: debug

# Debug build (default)
.PHONY: debug
debug:
	@echo "Building in DEBUG mode..."
	gprbuild -P $(GPR_FILE) -XBUILD_MODE=debug
	@echo "Build complete: $(EXEC)"
	@echo ""
	@echo "Run with: ./$(EXEC)"

# Release build (optimized, but keeps safety checks)
.PHONY: release
release:
	@echo "Building in RELEASE mode..."
	gprbuild -P $(GPR_FILE) -XBUILD_MODE=release
	@echo "Build complete: $(EXEC)"
	@echo ""
	@echo "NOTE: Safety checks (overflow, stack, validity) remain enabled in release!"

# SPARK verification mode
.PHONY: prove
prove:
	@echo "Building in PROVE mode for SPARK verification..."
	gprbuild -P $(GPR_FILE) -XBUILD_MODE=prove
	@echo "Build complete: $(EXEC)"
	@echo ""
	@echo "To run SPARK proofs: gnatprove -P $(GPR_FILE)"

# Run SPARK formal verification (requires SPARK Pro)
.PHONY: verify
verify:
	@echo "Running SPARK formal verification..."
	gnatprove -P $(GPR_FILE) -XBUILD_MODE=prove
	@echo ""
	@echo "Verification complete. Check spark/gnatprove for results."

# Clean build artifacts
.PHONY: clean
clean:
	@echo "Cleaning build artifacts..."
	gprclean -P $(GPR_FILE)
	rm -rf obj/* bin/*
	rm -rf spark/
	@echo "Clean complete."

# Install to system (requires sudo)
.PHONY: install
install: release
	@echo "Installing to /usr/local/bin..."
	sudo install -m 755 $(EXEC) /usr/local/bin/
	sudo mkdir -p /usr/local/share/hinfo_loc_fluctuator/data
	sudo cp -r data/* /usr/local/share/hinfo_loc_fluctuator/data/
	@echo "Installation complete."
	@echo "Run with: hinfo_loc_fluctuator"

# Uninstall from system
.PHONY: uninstall
uninstall:
	@echo "Uninstalling from /usr/local/bin..."
	sudo rm -f /usr/local/bin/hinfo_loc_fluctuator
	sudo rm -rf /usr/local/share/hinfo_loc_fluctuator
	@echo "Uninstallation complete."

# Run the program (debug build)
.PHONY: run
run: debug
	@echo ""
	@echo "Running HINFO-LOC Fluctuator..."
	@echo ""
	./$(EXEC)

# Check code style
.PHONY: style
style:
	@echo "Checking Ada style..."
	gnatcheck -P $(GPR_FILE) -rules -from=style_rules.txt || echo "Note: style_rules.txt not found"

# Show help
.PHONY: help
help:
	@echo "HINFO-LOC Fluctuator Makefile Targets:"
	@echo ""
	@echo "  make                 - Build in debug mode (default)"
	@echo "  make debug           - Build with debug symbols and assertions"
	@echo "  make release         - Build optimized (safety checks still enabled!)"
	@echo "  make prove           - Build for SPARK verification"
	@echo "  make verify          - Run SPARK formal verification (requires SPARK Pro)"
	@echo "  make clean           - Remove build artifacts"
	@echo "  make run             - Build and run the program"
	@echo "  make install         - Install to /usr/local/bin (requires sudo)"
	@echo "  make uninstall       - Uninstall from /usr/local/bin"
	@echo "  make help            - Show this help message"
	@echo ""
	@echo "Build Modes:"
	@echo "  DEBUG   - No optimization, debug symbols, assertions"
	@echo "  RELEASE - Optimized, but keeps overflow/stack/validity checks"
	@echo "  PROVE   - For SPARK formal verification"
	@echo ""
	@echo "Security Features (ALWAYS enabled):"
	@echo "  -gnato       : Overflow checking"
	@echo "  -fstack-check: Stack overflow checking"
	@echo "  -gnatVa      : Validity checking for all data"
	@echo ""

# Development: quick test cycle
.PHONY: test
test: clean debug
	@echo ""
	@echo "Quick test build complete."
	@echo "Run './$(EXEC)' to test."

# Show project structure
.PHONY: tree
tree:
	@echo "Project Structure:"
	@tree -L 2 -I 'obj|bin|spark' || \
	find . -type d -name obj -prune -o -type d -name bin -prune -o -print | sort

.PHONY: info
info:
	@echo "HINFO-LOC Fluctuator Build Information"
	@echo "======================================"
	@echo "Project:     $(PROJECT)"
	@echo "GPR File:    $(GPR_FILE)"
	@echo "Executable:  $(EXEC)"
	@echo ""
	@echo "GNAT Version:"
	@gnatls -v | head -n 1
	@echo ""
	@echo "Data Files:"
	@ls -lh data/
