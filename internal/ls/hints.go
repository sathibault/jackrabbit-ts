package ls

import (
	"fmt"
	"io"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/jackrabbit"
)

func (l *LanguageService) ProvideHlsHints(archPath string, fileName string, start_pos int, end_pos int, stderr io.Writer) []jackrabbit.HlsBlockSummary {
	program, file := l.getProgramAndFile(fileName)

	fmt.Fprintln(stderr, "Inlay search", start_pos, end_pos)

	fmt.Fprintln(stderr, file.Text)
	hints := make([]jackrabbit.HlsBlockSummary, 0, 128)
	
	visitBody := func(body *ast.Node) {
	}

	visit := func(node *ast.Node) *ast.Node {
		if ast.IsFunctionDeclaration(node) {
			desc := jackrabbit.FunctionDescriptor{}
			proc := jackrabbit.NewHlsProcGen(node.AsFunctionDeclaration(), &desc, program.GetTypeChecker(), true)
			sm := jackrabbit.CreateSmContext(archPath)
			sm.Generate(proc)
			sm.Xic.Dump(stderr)
			details := sm.Analysis()
			fmt.Fprintln(stderr, "GOT", details)
			hints = append(hints, details...)
		}
		if ast.IsBlock(node) {
		} else if ast.IsForInOrOfStatement(node) {
			loop := node.AsForInOrOfStatement()
			visitBody(loop.Statement)
		} else if ast.IsForStatement(node) {
			loop := node.AsForStatement()
			visitBody(loop.Statement)
		} else if node.Kind == ast.KindDoStatement {
			loop := node.AsDoStatement()
			visitBody(loop.Statement)
		} else if node.Kind == ast.KindWhileStatement {
			loop := node.AsWhileStatement()
			visitBody(loop.Statement)
		} else if node.Kind == ast.KindIfStatement {
			cond := node.AsIfStatement()
			visitBody(cond.ThenStatement)
			if cond.ElseStatement != nil {
				visitBody(cond.ElseStatement)
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
			//fmt.Fprintln(stderr, "Visit", node.Kind.String(), node.Pos(), node.End())
			visit(node)
			node.VisitEachChild(visitor)
		} else {
			//fmt.Fprintln(stderr, "Skip", node.Pos(), node.End())
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
		VisitNode: visitNode,
		VisitNodes: visitNodes,
	})

	visitor.VisitEachChild(file.AsNode())

	return hints
}