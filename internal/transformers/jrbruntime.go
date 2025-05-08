package transformers

import (
	"fmt"
	"runtime"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/printer"
)

type JackrabbitTransformer struct {
	Transformer
	tc *checker.Checker
}

func NewJackrabbitTransformer(emitContext *printer.EmitContext, tc *checker.Checker) *Transformer {
	tx := &JackrabbitTransformer{
		tc: tc,
	}
	return tx.newTransformer(tx.visit, emitContext)
}

func (tx *JackrabbitTransformer) visit(node *ast.Node) *ast.Node {
	if node == nil {
		return nil
	}
	switch node.Kind {
	case ast.KindCallExpression:
		call := node.AsCallExpression()
		if call.TypeArguments != nil && len(call.TypeArguments.Nodes) > 0 {
			called := call.Expression
			if ast.IsIdentifier(called) {
				sym := tx.tc.GetSymbolAtLocation(called)
				if sym != nil {
					node = tx.reifyCall(sym, node)
				}
			}
		}
		// 	if fun.AsIdentifier().Text == "$reify" {
		// 		fmt.Fprintln(os.Stderr, "REIFY")
		// 		ty := tx.tc.GetTypeAtLocation(node)
		// 		if ty.Flags() == checker.TypeFlagsNumberLiteral {
		// 			v, ok := checker.GetLiteralUintValue(ty)
		// 			assert(ok, "Unknown $reify type", ty)
		// 			return tx.factory.NewNumericLiteral(fmt.Sprint(v))
		// 		}
		// 	}
		// }
	}
	node = tx.visitor.VisitEachChild(node)
	return node
}

func (tx *JackrabbitTransformer) reifyCall(sym *ast.Symbol, node *ast.Node) *ast.Node {
	var decl *ast.FunctionDeclaration
	// skip overloads to find implementation
	for _, d := range sym.Declarations {
		if ast.IsFunctionDeclaration(d) {
			fn := d.AsFunctionDeclaration()
			if decl == nil || len(fn.Parameters.Nodes) > len(decl.Parameters.Nodes) {
				decl = fn
			}
		}
	}
	if decl != nil {
		call := node.AsCallExpression()
		var arguments *ast.NodeList
		for arg, x := range decl.Parameters.Nodes {
			if ast.IsParameter(x) {
				p := x.AsParameterDeclaration()
				if p.Initializer != nil && ast.IsCallExpression(p.Initializer) {
					init := p.Initializer.AsCallExpression()
					if ast.IsIdentifier(init.Expression) {
						if init.Expression.AsIdentifier().Text == "$reify" {
							assert(len(init.TypeArguments.Nodes) == 1)
							ref := init.TypeArguments.Nodes[0].AsTypeReference()
							rn := ref.TypeName.Text()
							pos := -1
							var def *ast.TypeNode
							for idx, y := range decl.TypeParameters.Nodes {
								tp := y.AsTypeParameter()
								if tp.Name().Text() == rn {
									pos = idx
									def = tp.DefaultType
								}
							}
							assert(pos >= 0)
							var v uint32
							var ok bool
							if pos < len(call.TypeArguments.Nodes) {
								typ := tx.tc.GetTypeAtLocation(call.TypeArguments.Nodes[pos])
								v, ok = checker.GetLiteralUintValue(typ)
							} else {
								assert(def != nil, "Failed to resolve $reify parameter", rn)
								assert(ast.IsLiteralTypeNode(def))
								lit := def.AsLiteralTypeNode().Literal
								v, ok = checker.NumberToUint32(checker.GetConstExpression(lit))
							}
							assert(ok, "Failed to resolve $reify value", rn)
							if arg >= len(call.Arguments.Nodes) {
								if arguments == nil {
									arguments = tx.factory.NewNodeList(make([]*ast.Node, 0, len(decl.Parameters.Nodes)))
									for _, a0 := range call.Arguments.Nodes {
										arguments.Nodes = append(arguments.Nodes, a0)
									}
								}
								for len(arguments.Nodes) < arg {
									arguments.Nodes = append(arguments.Nodes, tx.factory.NewIdentifier("undefined"))
								}
								arguments.Nodes = append(arguments.Nodes, tx.factory.NewNumericLiteral(fmt.Sprint(v)))
							}
						}
					}
				}
			}
		}
		if arguments != nil {
			return tx.factory.UpdateCallExpression(call, call.Expression, call.QuestionDotToken, call.TypeArguments, arguments)
		}
	}
	return node
}

func assert(condition bool, args ...interface{}) {
	if !condition {
		message := "Assertion failed: "
		if len(args) > 0 {
			message += fmt.Sprint(args...)
		}

		_, file, line, ok := runtime.Caller(1)
		if ok {
			message += fmt.Sprintf(" in %s:%d", file, line)
		}

		panic(message)
	}
}
