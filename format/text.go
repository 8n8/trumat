package format

import (
	"os"
)

func initMemory() []byte {
	return make([]byte, 0, 50000000)
}

type Text struct {
	start uint32
	end   uint32
}

func textFromString(m []byte, s string) Text {
	var text Text
	text.start = uint32(len(m))
	_ = append(m, []byte(s)...)
	text.end = uint32(len(m))
	return text
}

func textToString(m []byte, text Text) string {
	return string(m[text.start:text.end])
}

func sliceEqual(a []byte, b []byte) bool {
	if len(a) != len(b) {
		return false
	}

	for i := 0; i < len(a); i++ {
		if a[i] != b[i] {
			return false
		}
	}

	return true
}

func textEqual(m []byte, a Text, b Text) bool {
	return sliceEqual(m[a.start:a.end], m[b.start:b.end])
}

func textFromFile(m []byte, path string) (Text, error) {
	var result Text
	file, err := os.Open(path)
	if err != nil {
		return result, err
	}

	result.start = uint32(len(m))
	m = m[:cap(m)]

	count, err := file.Read(m[result.start:])
	if err != nil {
		return result, err
	}

	m = m[:int(result.start)+count]

	result.end = uint32(len(m))

	return result, nil
}
