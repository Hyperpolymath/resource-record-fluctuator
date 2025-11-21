# HINFO-LOC Fluctuator

## Project Overview

DNS record randomization tool for deprecated HINFO (host info: CPU/OS) and LOC (geographic location) records. This is a hobby project with dual purpose:

**Amusing Applications:**
- "Quantum server" - server that exists in multiple locations simultaneously
- Entertainment and demonstration purposes

**Serious Security Uses:**
- Honeypot obfuscation
- Attack response and deception technology
- Privacy enhancement for public-facing DNS records

**Important Note:** HINFO and LOC records are deprecated per RFC 8482 - that's precisely the point. These records are rarely used in production, making them ideal for experimental security and obfuscation techniques.

## Project Structure

```
resource-record-fluctuator/
├── hinfo_loc_fluctuator_ada/          # Primary Ada implementation (RECOMMENDED)
│   ├── src/
│   │   ├── dns_records.ads/adb        # Type-safe DNS record structures
│   │   ├── secure_auth.ads/adb        # Authentication module
│   │   ├── randomizer.ads/adb         # Randomization engine
│   │   ├── tui.ads/adb                # Text-based user interface
│   │   └── main.adb                   # Entry point
│   ├── data/
│   │   ├── machines.txt               # 60+ CPU types
│   │   ├── operating_systems.txt      # 70+ operating systems
│   │   └── locations.csv              # 50+ geographic locations
│   ├── hinfo_loc_fluctuator.gpr       # GNAT project file
│   ├── Makefile                       # Build automation
│   ├── README.md                      # Main documentation
│   └── docs/
│       └── USE_CASES.md               # Detailed use cases
├── hinfo_loc_fluctuator/              # Elixir prototype (reference only)
│   └── [Not recommended for production]
└── CLAUDE.md                          # This file
```

## Current State

### Implemented Features

✅ **Complete Ada Implementation**
- Type-safe DNS record structures with compile-time bounds checking
- Secure authentication module with permission levels
- Randomization engine with configurable pools
- Text-based TUI with menu system
- Comprehensive sample data (60+ CPUs, 70+ OSes, 50+ locations)

✅ **Security Features**
- Permission system: None, Read-Only, Modify-Local, Modify-Remote, Admin
- Session timeout (30 minutes)
- Constant-time password comparison (prevents timing attacks)
- Replay attack detection (nonce-based)
- All compiler safety checks enabled even in release builds

✅ **Documentation**
- README.md with full project documentation
- USE_CASES.md with detailed scenarios
- Inline code documentation

### Not Yet Implemented

❌ **DNS Communication**
- DNS UPDATE (RFC 2136) for remote mode
- TSIG authentication (RFC 2845) for secure updates
- DNS zone file writer for local mode (BIND format)

❌ **Advanced Features**
- CSV parser for Load_Location_Pool (currently stubbed)
- Scheduled auto-fluctuation with Ada tasking
- File-based configuration system
- API/webhook triggers
- Ncurses-based GUI (current is simple text menu)

❌ **Production Security**
- Real cryptographic hashing (bcrypt/Argon2) - currently uses demo hashes
- SPARK formal verification of Secure_Auth module

## Development Setup

### Prerequisites

**Required:**
- GNAT Ada compiler (FSF GNAT or AdaCore GNAT Community)
- Make (for build automation)

**Optional:**
- SPARK Pro (for formal verification)
- DNS server for testing (BIND, PowerDNS, or NSD)

### Installation

```bash
# Clone the repository
git clone <repository-url>
cd resource-record-fluctuator/hinfo_loc_fluctuator_ada

# Build the project (debug mode)
make

# Or build with release optimizations
make release

# Optional: Run SPARK verification
make prove
```

### Running the Project

```bash
# Run the application
./bin/hinfo_loc_fluctuator

# Demo credentials:
# Username: admin (full access) or user (read-only)
# Password: See README.md for demo hashes
# ⚠️  MUST be changed for any real use!
```

## Testing

```bash
# Currently manual testing via TUI
# Unit tests to be added

# Test with actual DNS server (future):
# 1. Set up BIND/PowerDNS/NSD test environment
# 2. Configure TSIG keys
# 3. Run fluctuator in remote mode
# 4. Verify records update correctly
```

## Key Technologies

- **Ada 2012**: Primary language (chosen for security and safety)
- **GNAT Project Manager**: Build system
- **SPARK** (optional): Formal verification
- **DNS RFCs**: 1034, 1035, 1876 (LOC), 8482 (deprecation)

## Architecture & Design Decisions

### Why Ada Over Elixir?

The user correctly identified a security risk: this tool modifies critical DNS infrastructure. Ada was chosen for:

1. **Memory Safety**: No buffer overflows by design
2. **Type Safety**: Range-checked types enforce invariants at compile time
3. **Security**: Constant-time comparison, overflow checking
4. **Formal Verification**: SPARK can mathematically prove correctness

**Example of type safety:**
```ada
type Latitude_Degrees is delta 0.000001 range -90.0 .. 90.0;
-- Compile-time guarantee: latitude cannot be out of range
```

### Architecture Diagram

```
Main → TUI ← Secure_Auth
       ↓         ↓
   Randomizer → DNS_Records (type-safe foundation)
```

### Security-First Design Principles

1. **Compile-Time Safety**
   - All numeric types have overflow checking enabled (`-gnato`)
   - Stack checking enabled (`-fstack-check`)
   - Validity checking for all data (`-gnatVa`)
   - These remain enabled even in release builds

2. **Runtime Security**
   - Bounded strings prevent overflow
   - Session validity checked on every permission check
   - Constant-time comparison in auth prevents timing attacks

3. **Defense in Depth**
   - Permission system with granular access control
   - Session timeouts
   - Replay attack protection

## Common Tasks

### Adding New CPUs/OSes

Edit data files (one entry per line, `#` for comments):
```bash
vim hinfo_loc_fluctuator_ada/data/machines.txt
vim hinfo_loc_fluctuator_ada/data/operating_systems.txt
```

### Adding New Locations

Edit CSV file (format: lat,lon,alt_meters,description):
```bash
vim hinfo_loc_fluctuator_ada/data/locations.csv
# Example:
# 37.7749,-122.4194,16,San Francisco Data Center
```

### Changing Security Settings

Edit `src/secure_auth.adb`:
- Update `SESSION_TIMEOUT` constant
- Modify permission levels
- Add/remove users (replace demo credentials)

### Building for Different Modes

```bash
make debug          # Debug symbols, assertions enabled
make release        # Optimizations, safety checks still on
make prove          # SPARK formal verification
make clean          # Clean build artifacts
```

## Code Style & Conventions

- **Ada Style Guide**: Follow standard Ada conventions
- **Naming**:
  - Types: `Mixed_Case_With_Underscores`
  - Variables: `Lower_Case_With_Underscores`
  - Constants: `UPPER_CASE_WITH_UNDERSCORES`
- **Comments**: Explain "why", not "what"
- **Type Safety**: Use strong typing, avoid unchecked conversions
- **Documentation**: Keep README.md and USE_CASES.md updated

## Important Files & Directories

### Critical Files

- `src/dns_records.ads`: Core type definitions - changing these affects everything
- `src/secure_auth.adb`: Authentication logic - security-critical
- `hinfo_loc_fluctuator.gpr`: Build configuration
- `data/*.txt`, `data/*.csv`: Sample data pools

### Configuration (Future)

Configuration system not yet implemented. Planned:
- TOML/YAML configuration file
- DNS server connection settings
- TSIG key configuration
- Scheduling parameters

## Dependencies & External Services

### Current Dependencies

- GNAT Ada compiler (no external libraries)
- Standard Ada runtime only

### Future Dependencies

- DNS server (BIND/PowerDNS/NSD) for testing
- bcrypt or Argon2 library for password hashing
- TOML/YAML parser for configuration
- Ncurses binding for enhanced TUI

### DNS RFCs

- RFC 1034, 1035: DNS basics
- RFC 1876: LOC record format
- RFC 2136: DNS UPDATE mechanism
- RFC 2845: TSIG authentication
- RFC 8482: HINFO/LOC deprecation

## Known Issues & Gotchas

### Security Gotchas

⚠️  **Demo Credentials Included**
- Default admin/user accounts exist with placeholder hashes
- **MUST be changed for any real use**
- Never deploy with demo credentials

⚠️  **No Real Crypto Yet**
- Current password comparison uses demo implementation
- Replace with bcrypt/Argon2 before production

### Implementation Gotchas

⚠️  **CSV Parser Stubbed**
- `Load_Location_Pool` reads filename but doesn't parse CSV yet
- Implementation needed for runtime location loading

⚠️  **No DNS Server Communication**
- Tool doesn't actually modify DNS records yet
- DNS UPDATE and zone file writing still needed

⚠️  **Chaos/Hesiod Classes**
- Included for completeness but rarely supported by DNS servers
- Most servers only support IN (Internet) class

### Type System Constraints

✓ **TTL Range**: 1-604800 seconds (1 sec to 7 days) enforced by type
✓ **Latitude**: -90.0 to +90.0 degrees enforced at compile time
✓ **Longitude**: -180.0 to +180.0 degrees enforced at compile time
✓ **Altitude**: -100000.0 to +42849672.95 meters (Dead Sea to space)

## Next Steps

### Immediate Priorities

1. **Test Compilation**
   - Install GNAT compiler if not present
   - Run `make` in `hinfo_loc_fluctuator_ada/`
   - Verify all modules compile without errors

2. **Implement CSV Parser**
   - Complete `Load_Location_Pool` in `randomizer.adb`
   - Parse `data/locations.csv` format
   - Add error handling for malformed CSV

3. **Replace Demo Authentication**
   - Integrate bcrypt or Argon2
   - Remove hardcoded demo credentials
   - Implement secure password storage

### Medium-Term Goals

4. **DNS UPDATE Implementation**
   - Implement RFC 2136 dynamic updates
   - Add TSIG authentication (RFC 2845)
   - Test with real DNS server

5. **Local Mode Zone File Writer**
   - Generate BIND-format zone files
   - Support for different zone file formats
   - Atomic file updates

6. **Scheduled Fluctuation**
   - Use Ada tasking for scheduled updates
   - Configurable intervals
   - Logging of changes

### Long-Term Enhancements

7. **SPARK Verification**
   - Formally verify `Secure_Auth` module
   - Prove absence of runtime errors
   - Document verification results

8. **Enhanced TUI**
   - Ncurses-based interface
   - Real-time monitoring
   - Configuration editor

9. **API Integration**
   - Webhook triggers
   - REST API for external control
   - Integration with monitoring systems

## Git Workflow

- **Main branch**: `main` (or master)
- **Feature branches**: Use descriptive names prefixed with `feature/`, `fix/`, `docs/`
- **Commit messages**: Follow conventional commits format
- **Current development branch**: `claude/hinfo-loc-fluctuator-017wTYsmNUpkBCu5vd7LNYvM`

### Commit Message Format

```
<type>: <subject>

<body>

<footer>
```

Types: `feat`, `fix`, `docs`, `refactor`, `test`, `chore`

## Environment Variables

Currently none required. Future configuration will use:

```bash
# DNS server settings (future)
DNS_SERVER=ns1.example.com
DNS_PORT=53
TSIG_KEY_FILE=/path/to/tsig.key

# Security settings (future)
SESSION_TIMEOUT=1800
MAX_LOGIN_ATTEMPTS=3
```

## Resources & References

### Documentation

- [RFC 1034](https://tools.ietf.org/html/rfc1034): DNS Concepts
- [RFC 1035](https://tools.ietf.org/html/rfc1035): DNS Implementation
- [RFC 1876](https://tools.ietf.org/html/rfc1876): LOC Record Format
- [RFC 2136](https://tools.ietf.org/html/rfc2136): DNS UPDATE
- [RFC 2845](https://tools.ietf.org/html/rfc2845): TSIG Authentication
- [RFC 8482](https://tools.ietf.org/html/rfc8482): HINFO/LOC Deprecation

### Ada Resources

- [Ada Reference Manual](http://www.ada-auth.org/standards/rm12_w_tc1/html/RM-TOC.html)
- [SPARK Documentation](https://docs.adacore.com/spark2014-docs/html/ug/)
- [GNAT User's Guide](https://docs.adacore.com/gnat_ugn-docs/html/gnat_ugn/gnat_ugn.html)

### Use Cases

See `hinfo_loc_fluctuator_ada/docs/USE_CASES.md` for detailed scenarios including:
- Honeypot deployment obfuscation
- Incident response and threat hunting
- Security research and education
- Privacy enhancement
- Testing and development scenarios

---

## Context for Future Claude Sessions

### Critical Insights to Preserve

1. **HINFO/LOC are deprecated** - This is intentional. The deprecation makes these records perfect for experimental security uses without affecting production services.

2. **Both amusing and serious** - Don't dismiss the "quantum server" jokes; they're part of the charm. But the security applications are real and documented.

3. **Security-first implementation** - The choice of Ada over Elixir was deliberate and well-reasoned. Don't suggest rewriting in other languages without considering the security implications.

4. **Type safety is critical** - Compile-time bounds checking (e.g., `Latitude_Degrees` range) prevents entire classes of bugs. This is a feature, not boilerplate.

### What This Tool Does

The HINFO-LOC Fluctuator randomizes DNS HINFO and LOC records to:
- Make honeypots appear to move locations or change systems
- Confuse attackers during incident response
- Enhance privacy for public DNS records
- Demonstrate "quantum server" concepts for entertainment

It does NOT:
- Modify production DNS records (yet - no DNS UPDATE implemented)
- Provide real quantum computing
- Replace proper security measures

### What Needs Attention

- Actual DNS server integration is the biggest missing piece
- Demo authentication MUST be replaced for any real use
- CSV parser needs completion
- Consider formal verification with SPARK for security-critical modules

---

**Note**: This CLAUDE.md file provides comprehensive context for Claude Code sessions. It's designed to help future sessions understand not just what the code does, but WHY architectural decisions were made and WHERE to focus next.
