# mcp-abap-adt-go

A Go-native MCP (Model Context Protocol) server for SAP ABAP Development Tools (ADT).

This provides a single-binary distribution of ABAP ADT tools for use with Claude and other MCP-compatible LLMs.

## Features

- **Single Binary**: Zero runtime dependencies, trivial distribution
- **13 ADT Tools**: Full read-only access to ABAP objects
- **Go Concurrency**: Built for parallel operations (future enhancement)
- **Cross-Platform**: Builds for Linux, macOS, Windows (amd64, arm64)

## Available Tools

| Tool | Description |
|------|-------------|
| `GetProgram` | Retrieve ABAP program source code |
| `GetClass` | Retrieve ABAP class source code |
| `GetInterface` | Retrieve ABAP interface source code |
| `GetFunction` | Retrieve function module source code |
| `GetFunctionGroup` | Retrieve function group structure |
| `GetInclude` | Retrieve ABAP include source code |
| `GetTable` | Retrieve ABAP table structure |
| `GetTableContents` | Retrieve data from ABAP table |
| `GetStructure` | Retrieve ABAP structure definition |
| `GetPackage` | Retrieve package contents |
| `GetTransaction` | Retrieve transaction details |
| `GetTypeInfo` | Retrieve data type information |
| `SearchObject` | Search for ABAP objects |

## Installation

### From Source

```bash
# Clone the repository
git clone https://github.com/vibingsteamer/mcp-abap-adt-go.git
cd mcp-abap-adt-go

# Build
make build

# Install to $GOPATH/bin
make install
```

### Pre-built Binaries

Download from the [releases page](https://github.com/vibingsteamer/mcp-abap-adt-go/releases).

## Configuration

The server is configured via environment variables:

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `SAP_URL` | Yes | - | SAP system URL (e.g., `https://host:44300`) |
| `SAP_USER` | Yes | - | SAP username |
| `SAP_PASSWORD` | Yes | - | SAP password |
| `SAP_CLIENT` | No | `001` | SAP client number |
| `SAP_LANGUAGE` | No | `EN` | SAP language |
| `SAP_INSECURE` | No | `false` | Skip TLS certificate verification |

## Usage with Claude Desktop

Add to your Claude Desktop configuration (`~/.config/claude/claude_desktop_config.json`):

```json
{
  "mcpServers": {
    "abap-adt": {
      "command": "/path/to/mcp-abap-adt-go",
      "env": {
        "SAP_URL": "https://your-sap-host:44300",
        "SAP_USER": "your-username",
        "SAP_PASSWORD": "your-password",
        "SAP_CLIENT": "001",
        "SAP_LANGUAGE": "EN"
      }
    }
  }
}
```

## Development

```bash
# Download dependencies
make deps

# Run linter
make lint

# Run tests
make test

# Build for all platforms
make build-all

# Clean build artifacts
make clean
```

## Architecture

```
mcp-abap-adt-go/
├── cmd/mcp-abap-adt-go/    # Main entry point
├── pkg/adt/                 # ADT client library
│   ├── client.go           # Main client facade
│   ├── config.go           # Configuration
│   ├── http.go             # HTTP transport with CSRF handling
│   └── xml.go              # XML parsing utilities
├── internal/mcp/           # MCP server implementation
└── testdata/               # Test fixtures
```

## License

MIT
