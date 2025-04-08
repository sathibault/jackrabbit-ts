package jackrabbit

import (
	"fmt"

	"github.com/microsoft/typescript-go/internal/ast"
)

type Namer struct {
	names   map[string]bool
	renamed map[ast.SymbolId]string
}

func NewNamer() *Namer {
	return &Namer{
		names:   make(map[string]bool),
		renamed: make(map[ast.SymbolId]string),
	}
}

func (n *Namer) GetName(sym *ast.Symbol) string {
	id := ast.GetSymbolId(sym)
	if renamedName, ok := n.renamed[id]; ok {
		return renamedName
	}
	return sym.Name
}

func (n *Namer) GenName(decl *ast.VariableDeclaration) string {
	sym := decl.Symbol
	org := sym.Name
	gen := org
	if _, exists := n.names[gen]; exists {
		i := 2
		gen = fmt.Sprintf("%s_%d", org, i)
		for _, exists := n.names[gen]; exists; {
			i++
			gen = fmt.Sprintf("%s_%d", org, i)
		}
		id := ast.GetSymbolId(sym)
		n.renamed[id] = gen
	}
	n.names[gen] = true
	return gen
}

func (n *Namer) GenNewName(org string) string {
	gen := org
	if _, exists := n.names[gen]; exists {
		i := 2
		gen = fmt.Sprintf("%s_%d", org, i)
		for _, exists := n.names[gen]; exists; {
			i++
			gen = fmt.Sprintf("%s_%d", org, i)
		}
	}
	n.names[gen] = true
	return gen
}
