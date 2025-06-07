package transformers

import (
	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/printer"
)

type InliningTransformer struct {
	Transformer
	tc *checker.Checker
}

func NewInliningTransformer(emitContext *printer.EmitContext, tc *checker.Checker) *Transformer {
	tx := &InliningTransformer{
		tc: tc,
	}
	return tx.newTransformer(tx.visit, emitContext)
}

func (tx *InliningTransformer) visit(node *ast.Node) *ast.Node {
	if node == nil {
		return nil
	}

	// Only handle call expressions
	if !ast.IsCallExpression(node) {
		return tx.visitor.VisitEachChild(node)
	}

	inliner := checker.GetInlineCallMapper(tx.tc, &tx.factory.NodeFactory, node)
	fn := inliner.Declartion
	if fn == nil || !ast.IsFunctionDeclaration(fn) && !ast.IsFunctionExpression(fn) && !ast.IsArrowFunction(fn) {
		return tx.visitor.VisitEachChild(node)
	}

	body := fn.Body()
	if body == nil || !ast.IsBlock(body) {
		return tx.visitor.VisitEachChild(node)
	}

	visitor := getInlineCloneVisitor(&tx.factory.NodeFactory, inliner, tx.tc)
	inlinedBody := visitor.VisitNode(body)

	// Handle functions with a single return statement
	bodyBlock := inlinedBody.AsBlock()
	if len(bodyBlock.Statements.Nodes) == 1 && ast.IsReturnStatement(bodyBlock.Statements.Nodes[0]) {
		ret := bodyBlock.Statements.Nodes[0].AsReturnStatement()
		if ret.Expression != nil {
			return ret.Expression
		}
	}

	// If more complex, inline as-is (as a block or IIFE)
	return inlinedBody
}

func getInlineCloneVisitor(f *ast.NodeFactory, mapper *checker.InlineMapper, tc *checker.Checker) *ast.NodeVisitor {
	var visitor *ast.NodeVisitor
	visitor = ast.NewNodeVisitor(
		func(node *ast.Node) *ast.Node {
			if ast.IsIdentifier(node) {
				sym := tc.GetSymbolAtLocation(node)
				if typeArg, mapType := mapper.CallTypeMap[sym]; mapType {
					return f.DeepCloneNode(typeArg)
				}
				if arg, mapArg := mapper.CallArgumentMap[sym]; mapArg {
					return f.DeepCloneNode(arg)
				}
			}
			visited := visitor.VisitEachChild(node)
			if visited != node {
				return visited
			}
			return node.Clone(f) // forcibly clone leaf nodes, which will then cascade new nodes/arrays upwards via `update` calls
		},
		f,
		ast.NodeVisitorHooks{
			VisitNodes: func(nodes *ast.NodeList, v *ast.NodeVisitor) *ast.NodeList {
				if nodes == nil {
					return nil
				}
				// force update empty lists
				if len(nodes.Nodes) == 0 {
					return nodes.Clone(v.Factory)
				}
				return v.VisitNodes(nodes)
			},
			VisitModifiers: func(nodes *ast.ModifierList, v *ast.NodeVisitor) *ast.ModifierList {
				if nodes == nil {
					return nil
				}
				// force update empty lists
				if len(nodes.Nodes) == 0 {
					return nodes.Clone(v.Factory)
				}
				return v.VisitModifiers(nodes)
			},
		},
	)
	return visitor
}
