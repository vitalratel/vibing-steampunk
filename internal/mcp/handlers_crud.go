// Package mcp provides the MCP server implementation for ABAP ADT tools.
// handlers_crud.go contains handlers for CRUD operations (lock, unlock, create, update, delete).
package mcp

import (
	"context"
	"encoding/json"
	"strings"

	"github.com/mark3labs/mcp-go/mcp"
	"github.com/oisee/vibing-steampunk/pkg/adt"
)

// --- CRUD Routing ---
// Routes for this module:
//   edit:    LOCK, UNLOCK, UPDATE_SOURCE, MOVE
//   create:  OBJECT, DEVC, TABL, CLONE
//   delete:  OBJECT
//   read:    CLAS_INFO
//   analyze: compare_source

// routeCRUDAction routes CRUD operations (lock, unlock, create, update, delete).
// Returns (result, true) if handled, (nil, false) if not handled by this module.
func (s *Server) routeCRUDAction(ctx context.Context, action, objectType, objectName string, params map[string]any) (*mcp.CallToolResult, bool, error) {
	switch action {
	case "edit":
		switch objectType {
		case "LOCK":
			objectURL, _ := params["object_url"].(string)
			if objectURL == "" {
				return newToolResultError("object_url is required in params"), true, nil
			}
			args := map[string]any{"object_url": objectURL}
			if accessMode, ok := params["access_mode"].(string); ok {
				args["access_mode"] = accessMode
			}
			result, err := s.handleLockObject(ctx, newRequest(args))
			return result, true, err

		case "UNLOCK":
			objectURL, _ := params["object_url"].(string)
			lockHandle, _ := params["lock_handle"].(string)
			if objectURL == "" || lockHandle == "" {
				return newToolResultError("object_url and lock_handle are required in params"), true, nil
			}
			result, err := s.handleUnlockObject(ctx, newRequest(map[string]any{
				"object_url":  objectURL,
				"lock_handle": lockHandle,
			}))
			return result, true, err

		case "UPDATE_SOURCE":
			objectURL, _ := params["object_url"].(string)
			source, _ := params["source"].(string)
			lockHandle, _ := params["lock_handle"].(string)
			if objectURL == "" || source == "" || lockHandle == "" {
				return newToolResultError("object_url, source, and lock_handle are required in params"), true, nil
			}
			args := map[string]any{
				"object_url":  objectURL,
				"source":      source,
				"lock_handle": lockHandle,
			}
			if transport, ok := params["transport"].(string); ok {
				args["transport"] = transport
			}
			result, err := s.handleUpdateSource(ctx, newRequest(args))
			return result, true, err

		case "MOVE":
			objType, _ := params["object_type"].(string)
			objName := objectName
			if objName == "" {
				objName, _ = params["object_name"].(string)
			}
			newPackage, _ := params["new_package"].(string)
			if objType == "" || objName == "" || newPackage == "" {
				return newToolResultError("object_type, object_name, and new_package are required"), true, nil
			}
			result, err := s.handleMoveObject(ctx, newRequest(map[string]any{
				"object_type": objType,
				"object_name": objName,
				"new_package": newPackage,
			}))
			return result, true, err
		}

	case "create":
		switch objectType {
		case "OBJECT":
			objType, _ := params["object_type"].(string)
			name := objectName
			if name == "" {
				name, _ = params["name"].(string)
			}
			description, _ := params["description"].(string)
			packageName, _ := params["package_name"].(string)
			if objType == "" || name == "" || description == "" || packageName == "" {
				return newToolResultError("object_type, name, description, and package_name are required"), true, nil
			}
			args := map[string]any{
				"object_type":  objType,
				"name":         name,
				"description":  description,
				"package_name": packageName,
			}
			if transport, ok := params["transport"].(string); ok {
				args["transport"] = transport
			}
			if parent, ok := params["parent_name"].(string); ok {
				args["parent_name"] = parent
			}
			if sd, ok := params["service_definition"].(string); ok {
				args["service_definition"] = sd
			}
			if bv, ok := params["binding_version"].(string); ok {
				args["binding_version"] = bv
			}
			if bc, ok := params["binding_category"].(string); ok {
				args["binding_category"] = bc
			}
			result, err := s.handleCreateObject(ctx, newRequest(args))
			return result, true, err

		case "DEVC":
			name := objectName
			if name == "" {
				name, _ = params["name"].(string)
			}
			description, _ := params["description"].(string)
			if name == "" || description == "" {
				return newToolResultError("name and description are required"), true, nil
			}
			args := map[string]any{
				"name":        name,
				"description": description,
			}
			if parent, ok := params["parent"].(string); ok {
				args["parent"] = parent
			}
			result, err := s.handleCreatePackage(ctx, newRequest(args))
			return result, true, err

		case "TABL":
			name := objectName
			if name == "" {
				name, _ = params["name"].(string)
			}
			description, _ := params["description"].(string)
			fields, _ := params["fields"].(string)
			if name == "" || description == "" || fields == "" {
				return newToolResultError("name, description, and fields (JSON) are required"), true, nil
			}
			args := map[string]any{
				"name":        name,
				"description": description,
				"fields":      fields,
			}
			if pkg, ok := params["package"].(string); ok {
				args["package"] = pkg
			}
			if transport, ok := params["transport"].(string); ok {
				args["transport"] = transport
			}
			if dc, ok := params["delivery_class"].(string); ok {
				args["delivery_class"] = dc
			}
			result, err := s.handleCreateTable(ctx, newRequest(args))
			return result, true, err

		case "CLONE":
			objType, _ := params["object_type"].(string)
			sourceName := objectName
			if sourceName == "" {
				sourceName, _ = params["source_name"].(string)
			}
			targetName, _ := params["target_name"].(string)
			pkg, _ := params["package"].(string)
			if objType == "" || sourceName == "" || targetName == "" || pkg == "" {
				return newToolResultError("object_type, source_name, target_name, and package are required"), true, nil
			}
			result, err := s.handleCloneObject(ctx, newRequest(map[string]any{
				"object_type": objType,
				"source_name": sourceName,
				"target_name": targetName,
				"package":     pkg,
			}))
			return result, true, err
		}

	case "delete":
		if objectType == "OBJECT" {
			objectURL, _ := params["object_url"].(string)
			lockHandle, _ := params["lock_handle"].(string)
			if objectURL == "" || lockHandle == "" {
				return newToolResultError("object_url and lock_handle are required in params"), true, nil
			}
			args := map[string]any{
				"object_url":  objectURL,
				"lock_handle": lockHandle,
			}
			if transport, ok := params["transport"].(string); ok {
				args["transport"] = transport
			}
			result, err := s.handleDeleteObject(ctx, newRequest(args))
			return result, true, err
		}

	case "read":
		if objectType == "CLAS_INFO" {
			className := objectName
			if className == "" {
				className, _ = params["class_name"].(string)
			}
			if className == "" {
				return newToolResultError("class_name is required"), true, nil
			}
			result, err := s.handleGetClassInfo(ctx, newRequest(map[string]any{"class_name": className}))
			return result, true, err
		}

	case "analyze":
		analysisType, _ := params["type"].(string)
		if analysisType == "compare_source" {
			type1, _ := params["type1"].(string)
			name1, _ := params["name1"].(string)
			type2, _ := params["type2"].(string)
			name2, _ := params["name2"].(string)
			if type1 == "" || name1 == "" || type2 == "" || name2 == "" {
				return newToolResultError("type1, name1, type2, and name2 are required in params"), true, nil
			}
			args := map[string]any{
				"type1": type1,
				"name1": name1,
				"type2": type2,
				"name2": name2,
			}
			if inc, ok := params["include1"].(string); ok {
				args["include1"] = inc
			}
			if parent, ok := params["parent1"].(string); ok {
				args["parent1"] = parent
			}
			if inc, ok := params["include2"].(string); ok {
				args["include2"] = inc
			}
			if parent, ok := params["parent2"].(string); ok {
				args["parent2"] = parent
			}
			result, err := s.handleCompareSource(ctx, newRequest(args))
			return result, true, err
		}
	}

	return nil, false, nil
}

// --- CRUD Handlers ---

func (s *Server) handleLockObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	accessMode := "MODIFY"
	if am, ok := request.Params.Arguments["access_mode"].(string); ok && am != "" {
		accessMode = am
	}

	result, err := s.adtClient.LockObject(ctx, objectURL, accessMode)
	if err != nil {
		return wrapErr("LockObject", err), nil
	}
	return newToolResultJSON(result), nil
}

func (s *Server) handleUnlockObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	lockHandle, ok := request.Params.Arguments["lock_handle"].(string)
	if !ok || lockHandle == "" {
		return newToolResultError("lock_handle is required"), nil
	}

	err := s.adtClient.UnlockObject(ctx, objectURL, lockHandle)
	if err != nil {
		return wrapErr("UnlockObject", err), nil
	}
	return mcp.NewToolResultText("Object unlocked successfully"), nil
}

func (s *Server) handleUpdateSource(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	source, ok := request.Params.Arguments["source"].(string)
	if !ok || source == "" {
		return newToolResultError("source is required"), nil
	}

	lockHandle, ok := request.Params.Arguments["lock_handle"].(string)
	if !ok || lockHandle == "" {
		return newToolResultError("lock_handle is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	// Append /source/main to object URL if not already present
	sourceURL := objectURL
	if !strings.HasSuffix(sourceURL, "/source/main") {
		sourceURL = objectURL + "/source/main"
	}

	err := s.adtClient.UpdateSource(ctx, sourceURL, source, lockHandle, transport)
	if err != nil {
		return wrapErr("UpdateSource", err), nil
	}
	return mcp.NewToolResultText("Source updated successfully"), nil
}

func (s *Server) handleCreateObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectType, ok := request.Params.Arguments["object_type"].(string)
	if !ok || objectType == "" {
		return newToolResultError("object_type is required"), nil
	}

	name, ok := request.Params.Arguments["name"].(string)
	if !ok || name == "" {
		return newToolResultError("name is required"), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	packageName, ok := request.Params.Arguments["package_name"].(string)
	if !ok || packageName == "" {
		return newToolResultError("package_name is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	parentName := ""
	if p, ok := request.Params.Arguments["parent_name"].(string); ok {
		parentName = p
	}

	// RAP-specific options
	serviceDefinition := ""
	if sd, ok := request.Params.Arguments["service_definition"].(string); ok {
		serviceDefinition = sd
	}
	bindingVersion := ""
	if bv, ok := request.Params.Arguments["binding_version"].(string); ok {
		bindingVersion = bv
	}
	bindingCategory := ""
	if bc, ok := request.Params.Arguments["binding_category"].(string); ok {
		bindingCategory = bc
	}

	opts := adt.CreateObjectOptions{
		ObjectType:        adt.CreatableObjectType(objectType),
		Name:              name,
		Description:       description,
		PackageName:       packageName,
		Transport:         transport,
		ParentName:        parentName,
		ServiceDefinition: serviceDefinition,
		BindingVersion:    bindingVersion,
		BindingCategory:   bindingCategory,
	}

	err := s.adtClient.CreateObject(ctx, opts)
	if err != nil {
		return wrapErr("CreateObject", err), nil
	}

	// Return the object URL for convenience
	objURL := adt.GetObjectURL(opts.ObjectType, opts.Name, opts.ParentName)
	return newToolResultJSON(map[string]string{
		"status":     "created",
		"object_url": objURL,
	}), nil
}

func (s *Server) handleCreatePackage(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	name, ok := request.Params.Arguments["name"].(string)
	if !ok || name == "" {
		return newToolResultError("name is required"), nil
	}

	// Validate package name starts with $
	name = strings.ToUpper(name)
	if !strings.HasPrefix(name, "$") {
		return newToolResultError("package name must start with $ (local packages only)"), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	parent := ""
	if p, ok := request.Params.Arguments["parent"].(string); ok && p != "" {
		parent = strings.ToUpper(p)
	}

	opts := adt.CreateObjectOptions{
		ObjectType:  adt.ObjectTypePackage,
		Name:        name,
		Description: description,
		PackageName: parent, // Parent package
	}

	err := s.adtClient.CreateObject(ctx, opts)
	if err != nil {
		return wrapErr("CreatePackage", err), nil
	}

	result := map[string]string{
		"status":      "created",
		"package":     name,
		"description": description,
	}
	if parent != "" {
		result["parent"] = parent
	}
	return newToolResultJSON(result), nil
}

func (s *Server) handleCreateTable(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	name, ok := request.Params.Arguments["name"].(string)
	if !ok || name == "" {
		return newToolResultError("name is required"), nil
	}

	description, ok := request.Params.Arguments["description"].(string)
	if !ok || description == "" {
		return newToolResultError("description is required"), nil
	}

	fieldsJSON, ok := request.Params.Arguments["fields"].(string)
	if !ok || fieldsJSON == "" {
		return newToolResultError("fields is required (JSON array)"), nil
	}

	// Parse fields JSON
	var fields []adt.TableField
	if err := json.Unmarshal([]byte(fieldsJSON), &fields); err != nil {
		return wrapErr("ParseFieldsJSON", err), nil
	}

	if len(fields) == 0 {
		return newToolResultError("At least one field is required"), nil
	}

	// Optional parameters
	pkg := "$TMP"
	if p, ok := request.Params.Arguments["package"].(string); ok && p != "" {
		pkg = strings.ToUpper(p)
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok && t != "" {
		transport = t
	}

	deliveryClass := "A"
	if dc, ok := request.Params.Arguments["delivery_class"].(string); ok && dc != "" {
		deliveryClass = strings.ToUpper(dc)
	}

	opts := adt.CreateTableOptions{
		Name:          name,
		Description:   description,
		Package:       pkg,
		Fields:        fields,
		Transport:     transport,
		DeliveryClass: deliveryClass,
	}

	err := s.adtClient.CreateTable(ctx, opts)
	if err != nil {
		return wrapErr("CreateTable", err), nil
	}

	return newToolResultJSON(map[string]any{
		"status":      "created",
		"table":       strings.ToUpper(name),
		"package":     pkg,
		"description": description,
		"fields":      len(fields),
	}), nil
}

func (s *Server) handleCompareSource(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	type1, _ := request.Params.Arguments["type1"].(string)
	name1, _ := request.Params.Arguments["name1"].(string)
	type2, _ := request.Params.Arguments["type2"].(string)
	name2, _ := request.Params.Arguments["name2"].(string)

	if type1 == "" || name1 == "" || type2 == "" || name2 == "" {
		return newToolResultError("type1, name1, type2, and name2 are all required"), nil
	}

	// Build options for first object
	opts1 := &adt.GetSourceOptions{}
	if inc, ok := request.Params.Arguments["include1"].(string); ok && inc != "" {
		opts1.Include = inc
	}
	if parent, ok := request.Params.Arguments["parent1"].(string); ok && parent != "" {
		opts1.Parent = parent
	}

	// Build options for second object
	opts2 := &adt.GetSourceOptions{}
	if inc, ok := request.Params.Arguments["include2"].(string); ok && inc != "" {
		opts2.Include = inc
	}
	if parent, ok := request.Params.Arguments["parent2"].(string); ok && parent != "" {
		opts2.Parent = parent
	}

	diff, err := s.adtClient.CompareSource(ctx, type1, name1, type2, name2, opts1, opts2)
	if err != nil {
		return wrapErr("CompareSource", err), nil
	}
	return newToolResultJSON(diff), nil
}

func (s *Server) handleCloneObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectType, _ := request.Params.Arguments["object_type"].(string)
	sourceName, _ := request.Params.Arguments["source_name"].(string)
	targetName, _ := request.Params.Arguments["target_name"].(string)
	pkg, _ := request.Params.Arguments["package"].(string)

	if objectType == "" || sourceName == "" || targetName == "" || pkg == "" {
		return newToolResultError("object_type, source_name, target_name, and package are all required"), nil
	}

	result, err := s.adtClient.CloneObject(ctx, objectType, sourceName, targetName, pkg)
	if err != nil {
		return wrapErr("CloneObject", err), nil
	}
	return newToolResultJSON(result), nil
}

func (s *Server) handleGetClassInfo(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	className, _ := request.Params.Arguments["class_name"].(string)
	if className == "" {
		return newToolResultError("class_name is required"), nil
	}

	info, err := s.adtClient.GetClassInfo(ctx, className)
	if err != nil {
		return wrapErr("GetClassInfo", err), nil
	}
	return newToolResultJSON(info), nil
}

func (s *Server) handleDeleteObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectURL, ok := request.Params.Arguments["object_url"].(string)
	if !ok || objectURL == "" {
		return newToolResultError("object_url is required"), nil
	}

	lockHandle, ok := request.Params.Arguments["lock_handle"].(string)
	if !ok || lockHandle == "" {
		return newToolResultError("lock_handle is required"), nil
	}

	transport := ""
	if t, ok := request.Params.Arguments["transport"].(string); ok {
		transport = t
	}

	err := s.adtClient.DeleteObject(ctx, objectURL, lockHandle, transport)
	if err != nil {
		return wrapErr("DeleteObject", err), nil
	}
	return mcp.NewToolResultText("Object deleted successfully"), nil
}

func (s *Server) handleMoveObject(ctx context.Context, request mcp.CallToolRequest) (*mcp.CallToolResult, error) {
	objectType, ok := request.Params.Arguments["object_type"].(string)
	if !ok || objectType == "" {
		return newToolResultError("object_type is required"), nil
	}

	objectName, ok := request.Params.Arguments["object_name"].(string)
	if !ok || objectName == "" {
		return newToolResultError("object_name is required"), nil
	}

	newPackage, ok := request.Params.Arguments["new_package"].(string)
	if !ok || newPackage == "" {
		return newToolResultError("new_package is required"), nil
	}

	// Ensure WebSocket client is connected
	if err := s.ensureDebugWSClient(ctx); err != nil {
		return wrapErr("ConnectWebSocket", err), nil
	}

	result, err := s.debugWSClient.MoveObject(ctx, objectType, objectName, newPackage)
	if err != nil {
		return wrapErr("MoveObject", err), nil
	}

	// Format result
	if result.Success {
		return newToolResultJSON(map[string]any{
			"status":      "success",
			"object":      result.Object,
			"object_name": result.ObjName,
			"new_package": result.NewPackage,
			"message":     result.Message,
		}), nil
	}
	return newToolResultError("Move failed: " + result.Message), nil
}
