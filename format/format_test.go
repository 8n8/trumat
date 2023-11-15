package format

import (
	"testing"
)

func TestFormat(t *testing.T) {
	m := initMemory()
	input := textFromString(m, `module X exposing (x)


x =
    0
`)

	formatted, _ := Format(m, input)

	if input != formatted {
		t.Errorf("expected %s but got %s", textToString(m, input), textToString(m, formatted))
	}
}
