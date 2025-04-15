package jackrabbit

import (
	"fmt"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
)

type RabbitDen struct {
	sourceAnalysis   map[string]*SourceDescriptor
	functionAnalysis map[string]*FunctionDescriptor
}

func NewRabbitDen() *RabbitDen {
	return &RabbitDen{
		sourceAnalysis:   make(map[string]*SourceDescriptor),
		functionAnalysis: make(map[string]*FunctionDescriptor),
	}
}

func (den *RabbitDen) GetSourceDescriptor(sourceFile *ast.SourceFile) *SourceDescriptor {
	if sd, ok := den.sourceAnalysis[sourceFile.FileName()]; ok {
		return sd
	}
	return nil
}

func (den *RabbitDen) GetFunctionIdentDescriptor(fun *ast.Identifier, tc *checker.Checker) *FunctionDescriptor {
	name := checker.QualifiedIdentName(fun, tc)
	if name != nil {
		if f, ok := den.functionAnalysis[*name]; ok {
			return f
		}
	}
	return nil
}

func (den *RabbitDen) GetFunctionDeclDescriptor(fun *ast.FunctionDeclaration) *FunctionDescriptor {
	qualified := checker.QualifiedFuncName(fun)
	if qualified != nil {
		if fd, ok := den.functionAnalysis[*qualified]; ok {
			return fd
		}
	}
	return nil
}

func (den *RabbitDen) getCalleeDescriptor(expr *ast.CallExpression, tc *checker.Checker) *FunctionDescriptor {
	fun := expr.Expression
	if ast.IsIdentifier(fun) {
		return den.GetFunctionIdentDescriptor(fun.AsIdentifier(), tc)
	} else if ast.IsPropertyAccessExpression(fun) {
		access := fun.AsPropertyAccessExpression()
		method := access.Name().Text()
		objType := tc.GetTypeAtLocation(access.Expression)
		cls := checker.QualifiedTypeName(objType, tc)
		if cls != nil {
			name := fmt.Sprintf("%s.%s", *cls, method)
			if f, ok := den.functionAnalysis[name]; ok {
				return f
			}
		}
	}
	return nil
}
