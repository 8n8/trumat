package format

import (
	"io/fs"
	"path/filepath"
	"testing"
)

func isElmPath(path string) bool {
	return path[len(path)-4:] == ".elm"
}

var I int = 0

func walkDirFuncDontChange(
	path string,
	d fs.DirEntry,
	err error,
	m []byte,
	t *testing.T) error {

	if d.IsDir() || !isElmPath(path) {
		return nil
	}

	m = m[:0]

	contents, err := textFromFile(m, path)
	if err != nil {
		return err
	}

	formatted, err := Format(m, contents)
	if err != nil {
		return nil
	}

	if !textEqual(m, contents, formatted) {
		t.Errorf("%s: expected %s but got %s", path, textToString(m, contents), textToString(m, formatted))
	}

	return nil
}

func TestDontChange(t *testing.T) {
	m := initMemory()
	_ = filepath.WalkDir(
		"test_data/dont_change",
		func(path string, d fs.DirEntry, err error) error {
			return walkDirFuncDontChange(path, d, err, m, t)
		})
}
