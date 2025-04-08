package jackrabbit

import (
	"fmt"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
)

var functionAnalysis = make(map[string]*FunctionDescriptor)

func GetFunctionIdentDescriptor(fun *ast.Identifier, tc *checker.Checker) *FunctionDescriptor {
	name := checker.QualifiedIdentName(fun, tc)
	return functionAnalysis[*name]
}

func getCalleeDescriptor(expr *ast.CallExpression, tc *checker.Checker) *FunctionDescriptor {
  fun := expr.Expression
	if ast.IsIdentifier(fun) {
		return GetFunctionIdentDescriptor(fun.AsIdentifier(), tc)
	} else if ast.IsPropertyAccessExpression(fun) {
		access := fun.AsPropertyAccessExpression()
		method := access.Name().Text()
    objType := tc.GetTypeAtLocation(access.Expression)
		cls := checker.QualifiedTypeName(objType, tc)
		if cls != nil {
			name := fmt.Sprintf("%s.%s", *cls, method)
			return functionAnalysis[name]	
		}
	}
	return nil
}

type FunctionDescriptor struct {
	MethodName string
	Declaration *ast.FunctionDeclaration
	Inlined bool
}

func (f *FunctionDescriptor) FilterCallerLhs(nodes []*ast.Node) []*ast.Node {
	return nodes
}

func (f *FunctionDescriptor) GetReturnType(t *checker.Type, tc *checker.Checker) *checker.Type {
	return t
}

func (f *FunctionDescriptor) GetReturnExpression(expr *ast.Expression) *ast.Expression {
	return expr
}

func (f *FunctionDescriptor) IsReferenceParameter(name string, t *checker.Type) bool {
	return false
}