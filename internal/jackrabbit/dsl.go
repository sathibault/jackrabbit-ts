package jackrabbit

import (
	"fmt"

	"github.com/microsoft/typescript-go/internal/checker"
)

func parseTypeSpec(spec string) *checker.TypeDescriptor {
	if len(spec) >= 4 && spec[:4] == "uint" {
		return &checker.TypeDescriptor{IsSigned: false, Width: mustParseInt(spec[4:])}
	} else if len(spec) >= 3 && spec[:3] == "int" {
		return &checker.TypeDescriptor{IsSigned: true, Width: mustParseInt(spec[3:])}
	}
	panic("Unrecognized type spec " + spec)
}

func mustParseInt(s string) uint32 {
	var i uint32
	_, err := fmt.Sscanf(s, "%d", &i)
	if err != nil {
		panic(err)
	}
	return i
}
