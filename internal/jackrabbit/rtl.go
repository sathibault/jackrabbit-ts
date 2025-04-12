package jackrabbit

import (
	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
)

// PortMode corresponds to 'input' | 'output' | 'inout'
type PortMode string

const (
	Input  PortMode = "input"
	Output PortMode = "output"
	Inout  PortMode = "inout"
)

// IoRole corresponds to 'provider' | 'client'
type IoRole string

const (
	Provider IoRole = "provider"
	Client   IoRole = "client"
)

// RtlModDescriptor corresponds to the top-level module description
type RtlModDescriptor struct {
	Name         string
	CallParams   []IRtlIo
	ReturnParams []IRtlIo
}

// IRtlIo is shared by signals, classes, and bundles
type IRtlIo struct {
	Name string
	Type RtlIoType
}

// RtlIoType is a discriminated union represented with an interface and a kind method
type RtlIoType interface {
	isRtlIoType()
	GetKind() string
}

// RtlSignalIo variant
type RtlSignalIo struct {
	Kind string // should be "signal"
	Mode PortMode
	Desc checker.TypeDescriptor
}

func (RtlSignalIo) isRtlIoType()      {}
func (r RtlSignalIo) GetKind() string { return r.Kind }

// RtlClassIo variant
type RtlClassIo struct {
	Kind    string // should be "class"
	Name    string
	Signals []IRtlIo
	Proto   IoInterface
}

func (RtlClassIo) isRtlIoType()      {}
func (r RtlClassIo) GetKind() string { return r.Kind }

// RtlBundleIo variant
type RtlBundleIo struct {
	Kind    string // should be "bundle"
	Members []IRtlIo
}

func (RtlBundleIo) isRtlIoType()      {}
func (r RtlBundleIo) GetKind() string { return r.Kind }

// RtlBundle is a map from names to RtlIoType
type RtlBundle map[string]RtlIoType

// IoInterface represents a class interface signature
type IoInterface struct {
	Params []checker.TypeDescriptor
	Cls    IoInterfaceClass
}

// IRtlPort represents a signal port
type IRtlPort struct {
	Name string
	Mode PortMode
	Desc checker.TypeDescriptor
}

// ISignal represents a signal with drivers and metadata
type ISignal struct {
	Name    string
	Drivers []Driver
	Desc    checker.TypeDescriptor
	Depth   *int            // optional field, nil if not set
	Init    *ast.Expression // Placeholder for your AST expression type
}

// Driver for a signal
type Driver struct {
	Expr    *ast.Expression // required field
	Addr    *ast.Expression // optional
	Cond    *ast.Expression // optional
	State   []string        // optional
	Delayed bool
}
