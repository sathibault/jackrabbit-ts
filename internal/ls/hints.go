package ls

import (
	"fmt"
	"io"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/jackrabbit"
)

type HlsHint struct {
	Method string
	Block  *jackrabbit.HlsBlockSummary
}

func (l *LanguageService) GetBlockAnnotation(fileName string, method string, no uint) (string, int) {
	_, file := l.getProgramAndFile(fileName)
	synthesis := l.host.GetSynthesis(fileName)
	if synthesis == nil {
		return "ERROR: synthesis not found", -1
	}

	node := core.Find(file.Statements.Nodes, func(node *ast.Node) bool {
		if ast.IsFunctionDeclaration(node) {
			decl := node.AsFunctionDeclaration()
			return method == decl.Name().Text()
		}
		return false
	})
	if node == nil {
		return fmt.Sprintf("ERROR: %s not found", method), -1
	}

	decl := node.AsFunctionDeclaration()
	desc := synthesis.GetHlsDescriptor(decl)

	if desc == nil {
		return fmt.Sprintf("ERROR: %s descriptor not found", method), -1
	}
	html, pos := desc.RenderBlock(no)
	if pos < 0 {
		return fmt.Sprintf("ERROR: block %d not found", no), -1
	}

	return html, pos
}

func (l *LanguageService) ProvideHlsHints(archPath string, fileName string, start_pos int, end_pos int, stderr io.Writer) []HlsHint {
	program, file := l.getProgramAndFile(fileName)
	synthesis := l.host.GetOrCreateSynthesis(archPath, fileName)

	fmt.Fprintln(stderr, "Inlay search", start_pos, end_pos)

	hints := make([]HlsHint, 0, 128)

	visit := func(node *ast.Node) *ast.Node {
		if ast.IsFunctionDeclaration(node) {
			decl := node.AsFunctionDeclaration()
			name := decl.Name().Text()
			synthesis.EnsureHlsDescriptor(decl, program.GetTypeChecker())
			desc := synthesis.GetHlsDescriptor(decl)
			details := desc.GetSummary()
			for _, detail := range details {
				fmt.Fprintln(stderr, "Block", detail.BlockNo, detail.Position, detail.End, detail.Stages)
				pos := int(detail.Position)
				if start_pos <= pos && pos <= end_pos {
					hints = append(hints, HlsHint{
						Method: name,
						Block:  &detail,
					})
				}
			}
		}
		return node
	}

	visitNode := func(node *ast.Node, visitor *ast.NodeVisitor) *ast.Node {
		if node == nil {
			return nil
		}
		if node.End() >= start_pos && node.Pos() <= end_pos {
			// if any overlap, visit
			// fmt.Fprintln(stderr, "Visit", node.Kind.String(), node.Pos(), node.End())
			visit(node)
			node.VisitEachChild(visitor)
		} else {
			// fmt.Fprintln(stderr, "Skip", node.Pos(), node.End())
		}
		return node
	}

	visitNodes := func(nodes *ast.NodeList, visitor *ast.NodeVisitor) *ast.NodeList {
		if nodes == nil || nodes.Nodes == nil {
			return nodes
		}
		for i := 0; i < len(nodes.Nodes); i++ {
			visitNode(nodes.Nodes[i], visitor)
		}
		return nodes
	}

	visitor := ast.NewNodeVisitor(core.Identity, nil, ast.NodeVisitorHooks{
		VisitNode:  visitNode,
		VisitNodes: visitNodes,
	})

	visitor.VisitEachChild(file.AsNode())

	return hints
}
