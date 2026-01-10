// ABOUTME: Source update operations for ABAP objects.
// ABOUTME: Provides UpdateSource for writing source code to ABAP objects.

package adt

import (
	"context"
	"fmt"
	"net/http"
	"net/url"
	"strings"
)

// UpdateSource writes source code to an ABAP object.
// objectSourceURL is the source URL (e.g., "/sap/bc/adt/programs/programs/ZTEST/source/main")
// lockHandle is required (from LockObject)
// transport is optional (for transportable objects)
func (c *Client) UpdateSource(ctx context.Context, objectSourceURL string, source string, lockHandle string, transport string) error {
	if err := c.checkSafety(OpUpdate, "UpdateSource"); err != nil {
		return err
	}

	params := url.Values{}
	params.Set("lockHandle", lockHandle)
	if transport != "" {
		params.Set("corrNr", transport)
	}

	contentType := "text/plain; charset=utf-8"
	if strings.HasPrefix(strings.TrimSpace(source), "<?xml") {
		contentType = "application/*"
	}

	_, err := c.transport.Request(ctx, objectSourceURL, &RequestOptions{
		Method:      http.MethodPut,
		Query:       params,
		Body:        []byte(source),
		ContentType: contentType,
	})
	if err != nil {
		return fmt.Errorf("updating source: %w", err)
	}

	return nil
}
