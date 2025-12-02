package adt

import (
	"os"
	"path/filepath"
	"testing"
)

func TestParseABAPFile_Class(t *testing.T) {
	tmpDir := t.TempDir()
	filePath := filepath.Join(tmpDir, "zcl_test.clas.abap")

	source := `CLASS zcl_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.

  PUBLIC SECTION.
    METHODS test.
ENDCLASS.

CLASS zcl_test IMPLEMENTATION.
  METHOD test.
  ENDMETHOD.
ENDCLASS.
`
	if err := os.WriteFile(filePath, []byte(source), 0644); err != nil {
		t.Fatal(err)
	}

	info, err := ParseABAPFile(filePath)
	if err != nil {
		t.Fatalf("ParseABAPFile failed: %v", err)
	}

	if info.ObjectName != "ZCL_TEST" {
		t.Errorf("Expected ObjectName ZCL_TEST, got %s", info.ObjectName)
	}
	if info.ObjectType != ObjectTypeClass {
		t.Errorf("Expected ObjectType %s, got %s", ObjectTypeClass, info.ObjectType)
	}
	if !info.HasDefinition {
		t.Error("Expected HasDefinition to be true")
	}
	if !info.HasImplementation {
		t.Error("Expected HasImplementation to be true")
	}
}

func TestParseABAPFile_ClassWithDescription(t *testing.T) {
	tmpDir := t.TempDir()
	filePath := filepath.Join(tmpDir, "zcl_ml_iris.clas.abap")

	source := `*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations
* Iris Flower Classification Model
CLASS zcl_ml_iris DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
ENDCLASS.

CLASS zcl_ml_iris IMPLEMENTATION.
ENDCLASS.
`
	if err := os.WriteFile(filePath, []byte(source), 0644); err != nil {
		t.Fatal(err)
	}

	info, err := ParseABAPFile(filePath)
	if err != nil {
		t.Fatalf("ParseABAPFile failed: %v", err)
	}

	if info.ObjectName != "ZCL_ML_IRIS" {
		t.Errorf("Expected ObjectName ZCL_ML_IRIS, got %s", info.ObjectName)
	}
	if info.Description != "Iris Flower Classification Model" {
		t.Errorf("Expected description 'Iris Flower Classification Model', got %s", info.Description)
	}
}

func TestParseABAPFile_Program(t *testing.T) {
	tmpDir := t.TempDir()
	filePath := filepath.Join(tmpDir, "ztest_prog.prog.abap")

	source := `REPORT ztest_prog.

WRITE: / 'Hello, World!'.
`
	if err := os.WriteFile(filePath, []byte(source), 0644); err != nil {
		t.Fatal(err)
	}

	info, err := ParseABAPFile(filePath)
	if err != nil {
		t.Fatalf("ParseABAPFile failed: %v", err)
	}

	if info.ObjectName != "ZTEST_PROG" {
		t.Errorf("Expected ObjectName ZTEST_PROG, got %s", info.ObjectName)
	}
	if info.ObjectType != ObjectTypeProgram {
		t.Errorf("Expected ObjectType %s, got %s", ObjectTypeProgram, info.ObjectType)
	}
}

func TestParseABAPFile_Interface(t *testing.T) {
	tmpDir := t.TempDir()
	filePath := filepath.Join(tmpDir, "zif_test.intf.abap")

	source := `INTERFACE zif_test
  PUBLIC.

  METHODS test.
ENDINTERFACE.
`
	if err := os.WriteFile(filePath, []byte(source), 0644); err != nil {
		t.Fatal(err)
	}

	info, err := ParseABAPFile(filePath)
	if err != nil {
		t.Fatalf("ParseABAPFile failed: %v", err)
	}

	if info.ObjectName != "ZIF_TEST" {
		t.Errorf("Expected ObjectName ZIF_TEST, got %s", info.ObjectName)
	}
	if info.ObjectType != ObjectTypeInterface {
		t.Errorf("Expected ObjectType %s, got %s", ObjectTypeInterface, info.ObjectType)
	}
}

func TestParseABAPFile_InvalidExtension(t *testing.T) {
	tmpDir := t.TempDir()
	filePath := filepath.Join(tmpDir, "test.txt")

	if err := os.WriteFile(filePath, []byte("test"), 0644); err != nil {
		t.Fatal(err)
	}

	_, err := ParseABAPFile(filePath)
	if err == nil {
		t.Error("Expected error for invalid extension, got nil")
	}
}

func TestParseABAPFile_NoObjectName(t *testing.T) {
	tmpDir := t.TempDir()
	filePath := filepath.Join(tmpDir, "test.clas.abap")

	source := `* Just a comment
* No class definition
`
	if err := os.WriteFile(filePath, []byte(source), 0644); err != nil {
		t.Fatal(err)
	}

	_, err := ParseABAPFile(filePath)
	if err == nil {
		t.Error("Expected error for file with no object name, got nil")
	}
}

func TestParseABAPFile_ClassWithTestClasses(t *testing.T) {
	tmpDir := t.TempDir()
	filePath := filepath.Join(tmpDir, "zcl_test_with_tests.clas.abap")

	source := `CLASS zcl_test_with_tests DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC.
ENDCLASS.

CLASS zcl_test_with_tests IMPLEMENTATION.
ENDCLASS.

CLASS ltc_test DEFINITION FINAL FOR TESTING
  DURATION SHORT
  RISK LEVEL HARMLESS.

  PRIVATE SECTION.
    METHODS test FOR TESTING.
ENDCLASS.

CLASS ltc_test IMPLEMENTATION.
  METHOD test.
  ENDMETHOD.
ENDCLASS.
`
	if err := os.WriteFile(filePath, []byte(source), 0644); err != nil {
		t.Fatal(err)
	}

	info, err := ParseABAPFile(filePath)
	if err != nil {
		t.Fatalf("ParseABAPFile failed: %v", err)
	}

	if !info.HasTestClasses {
		t.Error("Expected HasTestClasses to be true")
	}
}
