package adt

import "encoding/json"

// WSMessage is the WebSocket message format for ZADT_VSP.
type WSMessage struct {
	ID      string         `json:"id"`
	Domain  string         `json:"domain"`
	Action  string         `json:"action"`
	Params  map[string]any `json:"params,omitempty"`
	Timeout int            `json:"timeout,omitempty"`
}

// WSResponse is the WebSocket response format from ZADT_VSP.
type WSResponse struct {
	ID      string          `json:"id"`
	Success bool            `json:"success"`
	Data    json.RawMessage `json:"data,omitempty"`
	Error   *WSError        `json:"error,omitempty"`
}

// WSError represents an error from ZADT_VSP.
type WSError struct {
	Code    string `json:"code"`
	Message string `json:"message"`
}
