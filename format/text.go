package format

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
