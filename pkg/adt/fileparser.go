package adt

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
)

// ABAPFileInfo contains parsed information about an ABAP source file.
type ABAPFileInfo struct {
	FilePath          string
	ObjectType        CreatableObjectType
	ObjectName        string
	Description       string // Parsed from comments if available
	HasDefinition     bool   // For classes
	HasImplementation bool
	HasTestClasses    bool
}

// ParseABAPFile analyzes an ABAP source file and extracts metadata.
// It detects the object type from file extension and parses the content
// to extract the object name and other metadata.
func ParseABAPFile(filePath string) (*ABAPFileInfo, error) {
	// 1. Detect from extension
	ext := filepath.Ext(filePath)
	info := &ABAPFileInfo{FilePath: filePath}

	switch ext {
	case ".abap":
		// Check for compound extensions like .clas.abap
		baseName := filepath.Base(filePath)
		if strings.HasSuffix(baseName, ".clas.abap") {
			info.ObjectType = ObjectTypeClass
		} else if strings.HasSuffix(baseName, ".prog.abap") {
			info.ObjectType = ObjectTypeProgram
		} else if strings.HasSuffix(baseName, ".intf.abap") {
			info.ObjectType = ObjectTypeInterface
		} else if strings.HasSuffix(baseName, ".fugr.abap") {
			info.ObjectType = ObjectTypeFunctionGroup
		} else if strings.HasSuffix(baseName, ".func.abap") {
			info.ObjectType = ObjectTypeFunctionMod
		} else {
			// Fallback: detect from content
			return parseFromContent(filePath)
		}
	default:
		return nil, fmt.Errorf("unsupported file extension: %s (expected .clas.abap, .prog.abap, .intf.abap, .fugr.abap, or .func.abap)", ext)
	}

	// 2. Parse file content to extract name and metadata
	file, err := os.Open(filePath)
	if err != nil {
		return nil, fmt.Errorf("opening file: %w", err)
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lineNum := 0
	inComment := false

	for scanner.Scan() && lineNum < 200 { // Scan first 200 lines
		line := scanner.Text()
		lineNum++

		// Parse based on object type
		switch info.ObjectType {
		case ObjectTypeClass:
			if name := parseClassName(line); name != "" {
				info.ObjectName = name
			}
			if strings.Contains(strings.ToUpper(line), "DEFINITION") {
				info.HasDefinition = true
			}
			if strings.Contains(strings.ToUpper(line), "IMPLEMENTATION") {
				info.HasImplementation = true
			}
			if strings.Contains(strings.ToUpper(line), "FOR TESTING") {
				info.HasTestClasses = true
			}

		case ObjectTypeProgram:
			if name := parseProgramName(line); name != "" {
				info.ObjectName = name
			}

		case ObjectTypeInterface:
			if name := parseInterfaceName(line); name != "" {
				info.ObjectName = name
			}

		case ObjectTypeFunctionGroup:
			if name := parseFunctionGroupName(line); name != "" {
				info.ObjectName = name
			}

		case ObjectTypeFunctionMod:
			if name := parseFunctionModuleName(line); name != "" {
				info.ObjectName = name
			}
		}

		// Parse description from header comments
		trimmed := strings.TrimSpace(line)
		if info.Description == "" {
			if strings.HasPrefix(trimmed, "*") || strings.HasPrefix(trimmed, "\"") {
				comment := strings.TrimPrefix(trimmed, "*")
				comment = strings.TrimPrefix(comment, "\"")
				comment = strings.TrimSpace(comment)

				// Skip common patterns
				if comment != "" &&
					!strings.HasPrefix(comment, "-") &&
					!strings.HasPrefix(comment, "=") &&
					!strings.HasPrefix(comment, "*") &&
					!strings.Contains(strings.ToLower(comment), "author") &&
					!strings.Contains(strings.ToLower(comment), "date") &&
					len(comment) > 10 && len(comment) < 60 {
					info.Description = comment
					inComment = true
				}
			} else if inComment {
				inComment = false
			}
		}

		// Early exit if we have all required info
		if info.ObjectName != "" && info.Description != "" {
			break
		}
	}

	if err := scanner.Err(); err != nil {
		return nil, fmt.Errorf("reading file: %w", err)
	}

	if info.ObjectName == "" {
		return nil, fmt.Errorf("could not parse object name from file (expected CLASS/PROGRAM/INTERFACE/FUNCTION GROUP/FUNCTION statement in first 200 lines)")
	}

	// Provide default description if none found
	if info.Description == "" {
		info.Description = fmt.Sprintf("Generated from %s", filepath.Base(filePath))
	}

	return info, nil
}

// parseFromContent detects object type by scanning file content
func parseFromContent(filePath string) (*ABAPFileInfo, error) {
	file, err := os.Open(filePath)
	if err != nil {
		return nil, err
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	for scanner.Scan() {
		line := strings.ToUpper(strings.TrimSpace(scanner.Text()))

		if strings.HasPrefix(line, "CLASS ") && strings.Contains(line, "DEFINITION") {
			// Re-parse with known type
			file.Close()
			return ParseABAPFile(filePath)
		}
		if strings.HasPrefix(line, "REPORT ") || strings.HasPrefix(line, "PROGRAM ") {
			file.Close()
			return ParseABAPFile(filePath)
		}
		if strings.HasPrefix(line, "INTERFACE ") {
			file.Close()
			return ParseABAPFile(filePath)
		}
		if strings.HasPrefix(line, "FUNCTION-POOL ") {
			file.Close()
			return ParseABAPFile(filePath)
		}
		if strings.HasPrefix(line, "FUNCTION ") {
			file.Close()
			return ParseABAPFile(filePath)
		}
	}

	return nil, fmt.Errorf("could not detect object type from file content")
}

// parseClassName extracts class name from CLASS <name> DEFINITION
func parseClassName(line string) string {
	re := regexp.MustCompile(`(?i)^\s*CLASS\s+([a-z0-9_/]+)\s+DEFINITION`)
	matches := re.FindStringSubmatch(line)
	if len(matches) > 1 {
		return strings.ToUpper(matches[1])
	}
	return ""
}

// parseProgramName extracts program name from REPORT/PROGRAM statement
func parseProgramName(line string) string {
	re := regexp.MustCompile(`(?i)^\s*(REPORT|PROGRAM)\s+([a-z0-9_/]+)`)
	matches := re.FindStringSubmatch(line)
	if len(matches) > 2 {
		return strings.ToUpper(matches[2])
	}
	return ""
}

// parseInterfaceName extracts interface name from INTERFACE <name> DEFINITION
func parseInterfaceName(line string) string {
	re := regexp.MustCompile(`(?i)^\s*INTERFACE\s+([a-z0-9_/]+)`)
	matches := re.FindStringSubmatch(line)
	if len(matches) > 1 {
		return strings.ToUpper(matches[1])
	}
	return ""
}

// parseFunctionGroupName extracts function group name from FUNCTION-POOL statement
func parseFunctionGroupName(line string) string {
	re := regexp.MustCompile(`(?i)^\s*FUNCTION-POOL\s+([a-z0-9_/]+)`)
	matches := re.FindStringSubmatch(line)
	if len(matches) > 1 {
		return strings.ToUpper(matches[1])
	}
	return ""
}

// parseFunctionModuleName extracts function module name from FUNCTION statement
func parseFunctionModuleName(line string) string {
	re := regexp.MustCompile(`(?i)^\s*FUNCTION\s+([a-z0-9_/]+)`)
	matches := re.FindStringSubmatch(line)
	if len(matches) > 1 {
		return strings.ToUpper(matches[1])
	}
	return ""
}
