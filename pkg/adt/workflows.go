package adt

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// --- Workflow Tools ---
// These tools combine multiple operations into atomic workflows for simpler usage.

// WriteProgramResult represents the result of writing a program.
type WriteProgramResult struct {
	Success      bool                       `json:"success"`
	ProgramName  string                     `json:"programName"`
	ObjectURL    string                     `json:"objectUrl"`
	SyntaxErrors []SyntaxCheckResult        `json:"syntaxErrors,omitempty"`
	Activation   *ActivationResult          `json:"activation,omitempty"`
	Message      string                     `json:"message,omitempty"`
}

// WriteProgram performs Lock -> SyntaxCheck -> UpdateSource -> Unlock -> Activate workflow.
// This is a convenience method for updating existing programs.
func (c *Client) WriteProgram(ctx context.Context, programName string, source string, transport string) (*WriteProgramResult, error) {
	// Safety check for workflow operations
	if err := c.checkSafety(OpWorkflow, "WriteProgram"); err != nil {
		return nil, err
	}

	programName = strings.ToUpper(programName)
	objectURL := fmt.Sprintf("/sap/bc/adt/programs/programs/%s", programName)
	sourceURL := objectURL + "/source/main"

	result := &WriteProgramResult{
		ProgramName: programName,
		ObjectURL:   objectURL,
	}

	// Step 1: Syntax check before making changes
	syntaxErrors, err := c.SyntaxCheck(ctx, objectURL, source)
	if err != nil {
		result.Message = fmt.Sprintf("Syntax check failed: %v", err)
		return result, nil
	}

	// Check for syntax errors
	for _, se := range syntaxErrors {
		if se.Severity == "E" || se.Severity == "A" || se.Severity == "X" {
			result.SyntaxErrors = syntaxErrors
			result.Message = "Source has syntax errors - not saved"
			return result, nil
		}
	}
	result.SyntaxErrors = syntaxErrors // Include warnings if any

	// Step 2: Lock the object
	lock, err := c.LockObject(ctx, objectURL, "MODIFY")
	if err != nil {
		result.Message = fmt.Sprintf("Failed to lock object: %v", err)
		return result, nil
	}

	// Ensure we unlock on any error
	defer func() {
		if !result.Success {
			c.UnlockObject(ctx, objectURL, lock.LockHandle)
		}
	}()

	// Step 3: Update source
	err = c.UpdateSource(ctx, sourceURL, source, lock.LockHandle, transport)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to update source: %v", err)
		return result, nil
	}

	// Step 4: Unlock before activation (SAP requirement)
	err = c.UnlockObject(ctx, objectURL, lock.LockHandle)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to unlock object: %v", err)
		return result, nil
	}

	// Step 5: Activate
	activation, err := c.Activate(ctx, objectURL, programName)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to activate: %v", err)
		result.Activation = activation
		return result, nil
	}

	result.Activation = activation
	if activation.Success {
		result.Success = true
		result.Message = "Program updated and activated successfully"
	} else {
		result.Message = "Activation failed - check activation messages"
	}

	return result, nil
}

// WriteClassResult represents the result of writing a class.
type WriteClassResult struct {
	Success      bool                       `json:"success"`
	ClassName    string                     `json:"className"`
	ObjectURL    string                     `json:"objectUrl"`
	SyntaxErrors []SyntaxCheckResult        `json:"syntaxErrors,omitempty"`
	Activation   *ActivationResult          `json:"activation,omitempty"`
	Message      string                     `json:"message,omitempty"`
}

// WriteClass performs Lock -> SyntaxCheck -> UpdateSource -> Unlock -> Activate workflow for classes.
func (c *Client) WriteClass(ctx context.Context, className string, source string, transport string) (*WriteClassResult, error) {
	// Safety check for workflow operations
	if err := c.checkSafety(OpWorkflow, "WriteClass"); err != nil {
		return nil, err
	}

	className = strings.ToUpper(className)
	objectURL := fmt.Sprintf("/sap/bc/adt/oo/classes/%s", className)
	sourceURL := objectURL + "/source/main"

	result := &WriteClassResult{
		ClassName: className,
		ObjectURL: objectURL,
	}

	// Step 1: Syntax check
	syntaxErrors, err := c.SyntaxCheck(ctx, objectURL, source)
	if err != nil {
		result.Message = fmt.Sprintf("Syntax check failed: %v", err)
		return result, nil
	}

	// Check for syntax errors
	for _, se := range syntaxErrors {
		if se.Severity == "E" || se.Severity == "A" || se.Severity == "X" {
			result.SyntaxErrors = syntaxErrors
			result.Message = "Source has syntax errors - not saved"
			return result, nil
		}
	}
	result.SyntaxErrors = syntaxErrors

	// Step 2: Lock
	lock, err := c.LockObject(ctx, objectURL, "MODIFY")
	if err != nil {
		result.Message = fmt.Sprintf("Failed to lock object: %v", err)
		return result, nil
	}

	defer func() {
		if !result.Success {
			c.UnlockObject(ctx, objectURL, lock.LockHandle)
		}
	}()

	// Step 3: Update source
	err = c.UpdateSource(ctx, sourceURL, source, lock.LockHandle, transport)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to update source: %v", err)
		return result, nil
	}

	// Step 4: Unlock
	err = c.UnlockObject(ctx, objectURL, lock.LockHandle)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to unlock object: %v", err)
		return result, nil
	}

	// Step 5: Activate
	activation, err := c.Activate(ctx, objectURL, className)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to activate: %v", err)
		result.Activation = activation
		return result, nil
	}

	result.Activation = activation
	if activation.Success {
		result.Success = true
		result.Message = "Class updated and activated successfully"
	} else {
		result.Message = "Activation failed - check activation messages"
	}

	return result, nil
}

// CreateProgramResult represents the result of creating a program.
type CreateProgramResult struct {
	Success      bool                `json:"success"`
	ProgramName  string              `json:"programName"`
	ObjectURL    string              `json:"objectUrl"`
	SyntaxErrors []SyntaxCheckResult `json:"syntaxErrors,omitempty"`
	Activation   *ActivationResult   `json:"activation,omitempty"`
	Message      string              `json:"message,omitempty"`
}

// CreateAndActivateProgram creates a new program with source code and activates it.
// Workflow: CreateObject -> Lock -> UpdateSource -> Unlock -> Activate
func (c *Client) CreateAndActivateProgram(ctx context.Context, programName string, description string, packageName string, source string, transport string) (*CreateProgramResult, error) {
	// Safety check for workflow operations
	if err := c.checkSafety(OpWorkflow, "CreateAndActivateProgram"); err != nil {
		return nil, err
	}

	programName = strings.ToUpper(programName)
	packageName = strings.ToUpper(packageName)

	// Check package restrictions
	if err := c.checkPackageSafety(packageName); err != nil {
		return nil, err
	}

	objectURL := fmt.Sprintf("/sap/bc/adt/programs/programs/%s", programName)
	sourceURL := objectURL + "/source/main"

	result := &CreateProgramResult{
		ProgramName: programName,
		ObjectURL:   objectURL,
	}

	// Step 1: Create the program
	err := c.CreateObject(ctx, CreateObjectOptions{
		ObjectType:  ObjectTypeProgram,
		Name:        programName,
		Description: description,
		PackageName: packageName,
		Transport:   transport,
	})
	if err != nil {
		result.Message = fmt.Sprintf("Failed to create program: %v", err)
		return result, nil
	}

	// Step 2: Lock
	lock, err := c.LockObject(ctx, objectURL, "MODIFY")
	if err != nil {
		result.Message = fmt.Sprintf("Failed to lock object: %v", err)
		return result, nil
	}

	defer func() {
		if !result.Success {
			c.UnlockObject(ctx, objectURL, lock.LockHandle)
		}
	}()

	// Step 3: Update source
	err = c.UpdateSource(ctx, sourceURL, source, lock.LockHandle, transport)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to update source: %v", err)
		return result, nil
	}

	// Step 4: Unlock
	err = c.UnlockObject(ctx, objectURL, lock.LockHandle)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to unlock object: %v", err)
		return result, nil
	}

	// Step 5: Activate
	activation, err := c.Activate(ctx, objectURL, programName)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to activate: %v", err)
		result.Activation = activation
		return result, nil
	}

	result.Activation = activation
	if activation.Success {
		result.Success = true
		result.Message = "Program created and activated successfully"
	} else {
		result.Message = "Activation failed - check activation messages"
	}

	return result, nil
}

// CreateClassWithTestsResult represents the result of creating a class with unit tests.
type CreateClassWithTestsResult struct {
	Success        bool              `json:"success"`
	ClassName      string            `json:"className"`
	ObjectURL      string            `json:"objectUrl"`
	Activation     *ActivationResult `json:"activation,omitempty"`
	UnitTestResult *UnitTestResult   `json:"unitTestResult,omitempty"`
	Message        string            `json:"message,omitempty"`
}

// CreateClassWithTests creates a new class with unit tests and runs them.
// Workflow: CreateObject -> Lock -> UpdateSource -> CreateTestInclude -> UpdateClassInclude -> Unlock -> Activate -> RunUnitTests
func (c *Client) CreateClassWithTests(ctx context.Context, className string, description string, packageName string, classSource string, testSource string, transport string) (*CreateClassWithTestsResult, error) {
	// Safety check for workflow operations
	if err := c.checkSafety(OpWorkflow, "CreateClassWithTests"); err != nil {
		return nil, err
	}

	className = strings.ToUpper(className)
	packageName = strings.ToUpper(packageName)

	// Check package restrictions
	if err := c.checkPackageSafety(packageName); err != nil {
		return nil, err
	}

	objectURL := fmt.Sprintf("/sap/bc/adt/oo/classes/%s", className)
	sourceURL := objectURL + "/source/main"

	result := &CreateClassWithTestsResult{
		ClassName: className,
		ObjectURL: objectURL,
	}

	// Step 1: Create the class
	err := c.CreateObject(ctx, CreateObjectOptions{
		ObjectType:  ObjectTypeClass,
		Name:        className,
		Description: description,
		PackageName: packageName,
		Transport:   transport,
	})
	if err != nil {
		result.Message = fmt.Sprintf("Failed to create class: %v", err)
		return result, nil
	}

	// Step 2: Lock
	lock, err := c.LockObject(ctx, objectURL, "MODIFY")
	if err != nil {
		result.Message = fmt.Sprintf("Failed to lock object: %v", err)
		return result, nil
	}

	defer func() {
		if !result.Success {
			c.UnlockObject(ctx, objectURL, lock.LockHandle)
		}
	}()

	// Step 3: Update main source
	err = c.UpdateSource(ctx, sourceURL, classSource, lock.LockHandle, transport)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to update class source: %v", err)
		return result, nil
	}

	// Step 4: Create test include
	err = c.CreateTestInclude(ctx, className, lock.LockHandle, transport)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to create test include: %v", err)
		return result, nil
	}

	// Step 5: Update test include
	err = c.UpdateClassInclude(ctx, className, ClassIncludeTestClasses, testSource, lock.LockHandle, transport)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to update test source: %v", err)
		return result, nil
	}

	// Step 6: Unlock
	err = c.UnlockObject(ctx, objectURL, lock.LockHandle)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to unlock object: %v", err)
		return result, nil
	}

	// Step 7: Activate
	activation, err := c.Activate(ctx, objectURL, className)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to activate: %v", err)
		result.Activation = activation
		return result, nil
	}
	result.Activation = activation

	if !activation.Success {
		result.Message = "Activation failed - check activation messages"
		return result, nil
	}

	// Step 8: Run unit tests
	flags := DefaultUnitTestFlags()
	testResult, err := c.RunUnitTests(ctx, objectURL, &flags)
	if err != nil {
		result.Message = fmt.Sprintf("Class activated but unit tests failed to run: %v", err)
		result.Success = true // Class was created successfully
		return result, nil
	}

	result.UnitTestResult = testResult
	result.Success = true
	result.Message = "Class created, activated, and unit tests executed successfully"

	return result, nil
}
// --- File-Based Deployment Workflows ---

// DeployResult contains the result of a file deployment operation.
type DeployResult struct {
	ObjectURL     string   `json:"objectUrl"`
	ObjectName    string   `json:"objectName"`
	ObjectType    string   `json:"objectType"`
	FilePath      string   `json:"filePath"`
	Success       bool     `json:"success"`
	Created       bool     `json:"created"` // true if created, false if updated
	SyntaxErrors  []string `json:"syntaxErrors,omitempty"`
	Errors        []string `json:"errors,omitempty"`
	Message       string   `json:"message,omitempty"`
}

// CreateFromFile creates a new ABAP object from a file and activates it.
//
// Workflow: Parse → Create → Lock → SyntaxCheck → Write → Unlock → Activate
//
// The function automatically detects the object type and name from the file extension
// and content. Supported file extensions: .clas.abap, .prog.abap, .intf.abap
//
// Example:
//   result, err := client.CreateFromFile(ctx, "/path/to/zcl_test.clas.abap", "$TMP", "")
func (c *Client) CreateFromFile(ctx context.Context, filePath, packageName, transport string) (*DeployResult, error) {
	// Safety check
	if err := c.checkSafety(OpCreate, "CreateFromFile"); err != nil {
		return nil, err
	}

	// 1. Parse file to detect type and name
	info, err := ParseABAPFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("parsing file: %w", err)
	}

	// 2. Read source code
	sourceBytes, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("reading file: %w", err)
	}
	source := string(sourceBytes)

	// 3. Create object
	err = c.CreateObject(ctx, CreateObjectOptions{
		ObjectType:  info.ObjectType,
		Name:        info.ObjectName,
		Description: info.Description,
		PackageName: packageName,
		Transport:   transport,
	})
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("create failed: %v", err)},
			Message:    fmt.Sprintf("Failed to create %s %s", info.ObjectType, info.ObjectName),
		}, nil
	}

	// 4. Build object URL
	objectURL, err := c.buildObjectURL(info.ObjectType, info.ObjectName)
	if err != nil {
		return nil, err
	}

	// 5. Lock object
	lockResult, err := c.LockObject(ctx, objectURL, "MODIFY")
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("lock failed: %v", err)},
			Message:    fmt.Sprintf("Object created but failed to lock: %v", err),
		}, nil
	}

	// Ensure unlock on any error
	unlocked := false
	defer func() {
		if !unlocked {
			_ = c.UnlockObject(ctx, objectURL, lockResult.LockHandle)
		}
	}()

	// 6. Syntax check (optional pre-check)
	syntaxErrors, err := c.SyntaxCheck(ctx, objectURL, source)
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("syntax check failed: %v", err)},
			Message:    fmt.Sprintf("Object created but syntax check failed: %v", err),
		}, nil
	}

	if len(syntaxErrors) > 0 {
		// Convert syntax errors to strings
		errorMsgs := make([]string, len(syntaxErrors))
		for i, e := range syntaxErrors {
			errorMsgs[i] = fmt.Sprintf("Line %d: %s", e.Line, e.Text)
		}
		return &DeployResult{
			FilePath:     filePath,
			ObjectURL:    objectURL,
			ObjectName:   info.ObjectName,
			ObjectType:   string(info.ObjectType),
			Success:      false,
			SyntaxErrors: errorMsgs,
			Message:      fmt.Sprintf("Object created but has %d syntax errors", len(syntaxErrors)),
		}, nil
	}

	// 7. Write source
	err = c.UpdateSource(ctx, objectURL, source, lockResult.LockHandle, transport)
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("write source failed: %v", err)},
			Message:    fmt.Sprintf("Object created but failed to write source: %v", err),
		}, nil
	}

	// 8. Unlock
	err = c.UnlockObject(ctx, objectURL, lockResult.LockHandle)
	unlocked = true
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("unlock failed: %v", err)},
			Message:    fmt.Sprintf("Source written but failed to unlock: %v", err),
		}, nil
	}

	// 9. Activate
	_, err = c.Activate(ctx, objectURL, info.ObjectName)
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("activation failed: %v", err)},
			Message:    fmt.Sprintf("Source written but activation failed: %v", err),
		}, nil
	}

	return &DeployResult{
		FilePath:   filePath,
		ObjectURL:  objectURL,
		ObjectName: info.ObjectName,
		ObjectType: string(info.ObjectType),
		Success:    true,
		Created:    true,
		Message:    fmt.Sprintf("Successfully created and activated %s %s from %s", info.ObjectType, info.ObjectName, filePath),
	}, nil
}

// UpdateFromFile updates an existing ABAP object from a file.
//
// Workflow: Parse → Lock → SyntaxCheck → Write → Unlock → Activate
//
// Example:
//   result, err := client.UpdateFromFile(ctx, "/path/to/zcl_test.clas.abap", "")
func (c *Client) UpdateFromFile(ctx context.Context, filePath, transport string) (*DeployResult, error) {
	// Safety check
	if err := c.checkSafety(OpUpdate, "UpdateFromFile"); err != nil {
		return nil, err
	}

	// 1. Parse file to detect type and name
	info, err := ParseABAPFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("parsing file: %w", err)
	}

	// 2. Read source code
	sourceBytes, err := os.ReadFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("reading file: %w", err)
	}
	source := string(sourceBytes)

	// 3. Build object URL
	objectURL, err := c.buildObjectURL(info.ObjectType, info.ObjectName)
	if err != nil {
		return nil, err
	}

	// 4. Lock object
	lockResult, err := c.LockObject(ctx, objectURL, "MODIFY")
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("lock failed: %v", err)},
			Message:    fmt.Sprintf("Failed to lock object: %v", err),
		}, nil
	}

	// Ensure unlock on any error
	unlocked := false
	defer func() {
		if !unlocked {
			_ = c.UnlockObject(ctx, objectURL, lockResult.LockHandle)
		}
	}()

	// 5. Syntax check
	syntaxErrors, err := c.SyntaxCheck(ctx, objectURL, source)
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("syntax check failed: %v", err)},
			Message:    fmt.Sprintf("Syntax check failed: %v", err),
		}, nil
	}

	if len(syntaxErrors) > 0 {
		// Convert syntax errors to strings
		errorMsgs := make([]string, len(syntaxErrors))
		for i, e := range syntaxErrors {
			errorMsgs[i] = fmt.Sprintf("Line %d: %s", e.Line, e.Text)
		}
		return &DeployResult{
			FilePath:     filePath,
			ObjectURL:    objectURL,
			ObjectName:   info.ObjectName,
			ObjectType:   string(info.ObjectType),
			Success:      false,
			SyntaxErrors: errorMsgs,
			Message:      fmt.Sprintf("Source has %d syntax errors", len(syntaxErrors)),
		}, nil
	}

	// 6. Write source
	err = c.UpdateSource(ctx, objectURL, source, lockResult.LockHandle, transport)
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("write source failed: %v", err)},
			Message:    fmt.Sprintf("Failed to write source: %v", err),
		}, nil
	}

	// 7. Unlock
	err = c.UnlockObject(ctx, objectURL, lockResult.LockHandle)
	unlocked = true
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("unlock failed: %v", err)},
			Message:    fmt.Sprintf("Source written but failed to unlock: %v", err),
		}, nil
	}

	// 8. Activate
	_, err = c.Activate(ctx, objectURL, info.ObjectName)
	if err != nil {
		return &DeployResult{
			FilePath:   filePath,
			ObjectURL:  objectURL,
			ObjectName: info.ObjectName,
			ObjectType: string(info.ObjectType),
			Success:    false,
			Errors:     []string{fmt.Sprintf("activation failed: %v", err)},
			Message:    fmt.Sprintf("Source written but activation failed: %v", err),
		}, nil
	}

	return &DeployResult{
		FilePath:   filePath,
		ObjectURL:  objectURL,
		ObjectName: info.ObjectName,
		ObjectType: string(info.ObjectType),
		Success:    true,
		Created:    false,
		Message:    fmt.Sprintf("Successfully updated and activated %s %s from %s", info.ObjectType, info.ObjectName, filePath),
	}, nil
}

// DeployFromFile intelligently creates or updates an object from a file.
//
// Workflow: Parse → CheckExists → CreateFromFile OR UpdateFromFile
//
// This is the recommended method for deploying ABAP objects from files as it
// automatically determines whether to create a new object or update an existing one.
//
// Example:
//   result, err := client.DeployFromFile(ctx, "/path/to/zcl_test.clas.abap", "$TMP", "")
func (c *Client) DeployFromFile(ctx context.Context, filePath, packageName, transport string) (*DeployResult, error) {
	// 1. Parse file
	info, err := ParseABAPFile(filePath)
	if err != nil {
		return nil, fmt.Errorf("parsing file: %w", err)
	}

	// 2. Check if object exists
	objectURL, err := c.buildObjectURL(info.ObjectType, info.ObjectName)
	if err != nil {
		return nil, err
	}

	// Try to get object (if 404, doesn't exist)
	_, err = c.transport.Request(ctx, objectURL, &RequestOptions{
		Method: "GET",
		Accept: "text/plain",
	})

	if err != nil {
		// Object doesn't exist - create it
		return c.CreateFromFile(ctx, filePath, packageName, transport)
	}

	// Object exists - update it
	return c.UpdateFromFile(ctx, filePath, transport)
}

// buildObjectURL constructs the ADT URL for an object type and name
func (c *Client) buildObjectURL(objType CreatableObjectType, name string) (string, error) {
	name = strings.ToLower(name)
	switch objType {
	case ObjectTypeClass:
		return fmt.Sprintf("/sap/bc/adt/oo/classes/%s", name), nil
	case ObjectTypeProgram:
		return fmt.Sprintf("/sap/bc/adt/programs/programs/%s", name), nil
	case ObjectTypeInterface:
		return fmt.Sprintf("/sap/bc/adt/oo/interfaces/%s", name), nil
	case ObjectTypeFunctionGroup:
		return fmt.Sprintf("/sap/bc/adt/functions/groups/%s", name), nil
	case ObjectTypeInclude:
		return fmt.Sprintf("/sap/bc/adt/programs/includes/%s", name), nil
	default:
		return "", fmt.Errorf("unsupported object type for URL building: %s", objType)
	}
}

// --- Utility Workflows ---

// RenameObjectResult contains the result of renaming an object.
type RenameObjectResult struct {
	OldName    string `json:"oldName"`
	NewName    string `json:"newName"`
	ObjectType string `json:"objectType"`
	Success    bool   `json:"success"`
	Message    string `json:"message,omitempty"`
	Errors     []string `json:"errors,omitempty"`
}

// RenameObject renames an ABAP object by creating a copy with the new name and deleting the old one.
//
// Workflow: GetSource → CreateNew → ActivateNew → DeleteOld
//
// This is a destructive operation - use with caution!
func (c *Client) RenameObject(ctx context.Context, objType CreatableObjectType, oldName, newName, packageName, transport string) (*RenameObjectResult, error) {
	// Safety check
	if err := c.checkSafety(OpDelete, "RenameObject"); err != nil {
		return nil, err
	}

	result := &RenameObjectResult{
		OldName:    oldName,
		NewName:    newName,
		ObjectType: string(objType),
	}

	// 1. Get old object source
	oldURL, err := c.buildObjectURL(objType, oldName)
	if err != nil {
		return nil, err
	}

	resp, err := c.transport.Request(ctx, oldURL+"/source/main", &RequestOptions{
		Method: "GET",
		Accept: "text/plain",
	})
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to read old object: %v", err))
		return result, nil
	}
	oldSource := string(resp.Body)

	// 2. Replace old name with new name in source
	newSource := strings.ReplaceAll(oldSource, strings.ToUpper(oldName), strings.ToUpper(newName))
	newSource = strings.ReplaceAll(newSource, strings.ToLower(oldName), strings.ToLower(newName))

	// 3. Create new object
	err = c.CreateObject(ctx, CreateObjectOptions{
		ObjectType:  objType,
		Name:        newName,
		Description: fmt.Sprintf("Renamed from %s", oldName),
		PackageName: packageName,
		Transport:   transport,
	})
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to create new object: %v", err))
		return result, nil
	}

	// 4. Write source to new object
	newURL, _ := c.buildObjectURL(objType, newName)
	lockResult, err := c.LockObject(ctx, newURL, "MODIFY")
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to lock new object: %v", err))
		return result, nil
	}

	defer func() {
		_ = c.UnlockObject(ctx, newURL, lockResult.LockHandle)
	}()

	err = c.UpdateSource(ctx, newURL, newSource, lockResult.LockHandle, transport)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to write source: %v", err))
		return result, nil
	}

	_ = c.UnlockObject(ctx, newURL, lockResult.LockHandle)

	// 5. Activate new object
	_, err = c.Activate(ctx, newURL, newName)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("Failed to activate new object: %v", err))
		return result, nil
	}

	// 6. Delete old object
	oldLockResult, err := c.LockObject(ctx, oldURL, "MODIFY")
	if err != nil {
		result.Message = fmt.Sprintf("New object %s created successfully, but failed to lock old object %s for deletion: %v. Please delete manually.", newName, oldName, err)
		result.Success = true
		return result, nil
	}

	err = c.DeleteObject(ctx, oldURL, oldLockResult.LockHandle, transport)
	if err != nil {
		result.Message = fmt.Sprintf("New object %s created successfully, but failed to delete old object %s: %v. Please delete manually.", newName, oldName, err)
		result.Success = true
		return result, nil
	}

	result.Success = true
	result.Message = fmt.Sprintf("Successfully renamed %s to %s", oldName, newName)
	return result, nil
}

// SaveToFileResult contains the result of saving an object to a file.
type SaveToFileResult struct {
	ObjectName string `json:"objectName"`
	ObjectType string `json:"objectType"`
	FilePath   string `json:"filePath"`
	LineCount  int    `json:"lineCount"`
	Success    bool   `json:"success"`
	Message    string `json:"message,omitempty"`
}

// SaveToFile saves an ABAP object's source code to a local file.
//
// Workflow: GetSource → WriteFile
//
// The file extension is automatically determined based on object type.
func (c *Client) SaveToFile(ctx context.Context, objType CreatableObjectType, objectName, outputPath string) (*SaveToFileResult, error) {
	result := &SaveToFileResult{
		ObjectName: objectName,
		ObjectType: string(objType),
	}

	// 1. Determine file extension
	var ext string
	switch objType {
	case ObjectTypeClass:
		ext = ".clas.abap"
	case ObjectTypeProgram:
		ext = ".prog.abap"
	case ObjectTypeInterface:
		ext = ".intf.abap"
	case ObjectTypeFunctionGroup:
		ext = ".fugr.abap"
	case ObjectTypeInclude:
		ext = ".abap"
	default:
		ext = ".abap"
	}

	// 2. Build file path
	if outputPath == "" {
		outputPath = "."
	}
	if !strings.HasSuffix(outputPath, ext) {
		// outputPath is a directory
		objectName = strings.ToLower(objectName)
		result.FilePath = filepath.Join(outputPath, objectName+ext)
	} else {
		result.FilePath = outputPath
	}

	// 3. Get object source
	objectURL, err := c.buildObjectURL(objType, objectName)
	if err != nil {
		return nil, err
	}

	resp, err := c.transport.Request(ctx, objectURL+"/source/main", &RequestOptions{
		Method: "GET",
		Accept: "text/plain",
	})
	if err != nil {
		result.Message = fmt.Sprintf("Failed to read object: %v", err)
		return result, nil
	}

	source := string(resp.Body)
	result.LineCount = len(strings.Split(source, "\n"))

	// 4. Write to file
	err = os.WriteFile(result.FilePath, []byte(source), 0644)
	if err != nil {
		result.Message = fmt.Sprintf("Failed to write file: %v", err)
		return result, nil
	}

	result.Success = true
	result.Message = fmt.Sprintf("Saved %s %s to %s (%d lines)", objType, objectName, result.FilePath, result.LineCount)
	return result, nil
}
