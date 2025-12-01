//go:build integration

package adt

import (
	"context"
	"os"
	"testing"
	"time"
)

// Integration tests require SAP_URL, SAP_USER, SAP_PASSWORD environment variables.
// Run with: go test -tags=integration -v ./pkg/adt/

func getIntegrationClient(t *testing.T) *Client {
	url := os.Getenv("SAP_URL")
	user := os.Getenv("SAP_USER")
	pass := os.Getenv("SAP_PASSWORD")

	if url == "" || user == "" || pass == "" {
		t.Skip("SAP_URL, SAP_USER, SAP_PASSWORD required for integration tests")
	}

	client := os.Getenv("SAP_CLIENT")
	if client == "" {
		client = "001"
	}
	lang := os.Getenv("SAP_LANGUAGE")
	if lang == "" {
		lang = "EN"
	}

	opts := []Option{
		WithClient(client),
		WithLanguage(lang),
		WithTimeout(30 * time.Second),
	}

	if os.Getenv("SAP_INSECURE") == "true" {
		opts = append(opts, WithInsecureSkipVerify())
	}

	return NewClient(url, user, pass, opts...)
}

func TestIntegration_SearchObject(t *testing.T) {
	client := getIntegrationClient(t)
	ctx := context.Background()

	results, err := client.SearchObject(ctx, "CL_*", 10)
	if err != nil {
		t.Fatalf("SearchObject failed: %v", err)
	}

	if len(results) == 0 {
		t.Log("No results found for CL_* search")
	} else {
		t.Logf("Found %d results", len(results))
		for i, r := range results {
			if i >= 3 {
				break
			}
			t.Logf("  %s (%s) - %s", r.Name, r.Type, r.Description)
		}
	}
}

func TestIntegration_GetProgram(t *testing.T) {
	client := getIntegrationClient(t)
	ctx := context.Background()

	// Try to get a standard SAP program
	source, err := client.GetProgram(ctx, "SAPMSSY0")
	if err != nil {
		t.Logf("Could not get SAPMSSY0: %v", err)
		// Try another common program
		source, err = client.GetProgram(ctx, "RS_ABAP_SOURCE_SCAN")
		if err != nil {
			t.Skipf("Could not retrieve any standard program: %v", err)
		}
	}

	if len(source) == 0 {
		t.Error("Program source is empty")
	} else {
		t.Logf("Retrieved %d characters of source code", len(source))
		// Show first 200 chars
		preview := source
		if len(preview) > 200 {
			preview = preview[:200] + "..."
		}
		t.Logf("Preview:\n%s", preview)
	}
}

func TestIntegration_GetClass(t *testing.T) {
	client := getIntegrationClient(t)
	ctx := context.Background()

	// Try to get a standard SAP class
	sources, err := client.GetClass(ctx, "CL_ABAP_TYPEDESCR")
	if err != nil {
		t.Skipf("Could not get CL_ABAP_TYPEDESCR: %v", err)
	}

	mainSource, ok := sources["main"]
	if !ok {
		t.Error("No main source in class")
	} else if len(mainSource) == 0 {
		t.Error("Main source is empty")
	} else {
		t.Logf("Retrieved %d characters of class source", len(mainSource))
	}
}

func TestIntegration_GetTableContents(t *testing.T) {
	client := getIntegrationClient(t)
	ctx := context.Background()

	// Get contents of T000 (clients table - should exist in any system)
	contents, err := client.GetTableContents(ctx, "T000", 5)
	if err != nil {
		t.Skipf("Could not get T000 contents: %v", err)
	}

	t.Logf("Retrieved %d columns, %d rows", len(contents.Columns), len(contents.Rows))

	if len(contents.Columns) == 0 {
		t.Error("No columns returned")
	}
	if len(contents.Rows) == 0 {
		t.Error("No rows returned")
	} else {
		t.Logf("First row: %v", contents.Rows[0])
	}
}

func TestIntegration_GetTable(t *testing.T) {
	client := getIntegrationClient(t)
	ctx := context.Background()

	source, err := client.GetTable(ctx, "T000")
	if err != nil {
		t.Skipf("Could not get T000 source: %v", err)
	}

	if len(source) == 0 {
		t.Error("Table source is empty")
	} else {
		t.Logf("Retrieved %d characters of table source", len(source))
		// Show first 200 chars
		preview := source
		if len(preview) > 200 {
			preview = preview[:200] + "..."
		}
		t.Logf("Preview:\n%s", preview)
	}
}

func TestIntegration_GetPackage(t *testing.T) {
	client := getIntegrationClient(t)
	ctx := context.Background()

	pkg, err := client.GetPackage(ctx, "BASIS")
	if err != nil {
		t.Skipf("Could not get BASIS package: %v", err)
	}

	t.Logf("Package: %s", pkg.Name)
	t.Logf("Sub-packages: %d, Objects: %d", len(pkg.SubPackages), len(pkg.Objects))
}
