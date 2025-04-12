package jackrabbit

import (
	"fmt"
	"os"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
)

var ASYNC_RUNNERS = map[string]bool{
	"dfa":      true,
	"stimulus": true,
}

var functionAnalysis = make(map[string]*FunctionDescriptor)

func GetFunctionIdentDescriptor(fun *ast.Identifier, tc *checker.Checker) *FunctionDescriptor {
	name := checker.QualifiedIdentName(fun, tc)
	if name != nil {
		if f, ok := functionAnalysis[*name]; ok {
			return f
		}
	}
	return nil
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
			if f, ok := functionAnalysis[name]; ok {
				return f
			}
		}
	}
	return nil
}

type FunctionDescriptor struct {
	MethodName          string
	Declaration         *ast.Node
	arrayParams         []uint
	ModuleType          *string
	ModuleUsers         []string
	CalledTypes         []string
	NeedAsync           bool
	AwaitToplevel       bool
	Inlined             bool
	ParamMap            map[string]uint
	ObservedParamShapes map[uint][][]uint
	resolvedParamShapes map[uint][]*uint
	Passthrough         map[string]uint
	GlobalRefs          map[string]map[string]struct{} // map to set
	RtlDescriptor       *RtlModDescriptor
	Dirty               bool
}

func newFunctionDescriptor(name string, decl *ast.Node, tc *checker.Checker) *FunctionDescriptor {
	fd := &FunctionDescriptor{
		Declaration:         decl,
		MethodName:          name,
		ModuleUsers:         []string{},
		CalledTypes:         []string{},
		Inlined:             false,
		NeedAsync:           false,
		AwaitToplevel:       true,
		Passthrough:         map[string]uint{},
		GlobalRefs:          map[string]map[string]struct{}{},
		Dirty:               false,
		ParamMap:            map[string]uint{},
		arrayParams:         []uint{},
		ObservedParamShapes: map[uint][][]uint{},
		resolvedParamShapes: map[uint][]*uint{},
	}

	for idx, p := range decl.ParameterList().Nodes {
		name := p.Name().Text()
		fd.ParamMap[name] = uint(idx)
		typ := tc.GetTypeAtLocation(p)
		if checker.IsArrayType(tc, typ) {
			fd.arrayParams = append(fd.arrayParams, uint(idx))
			fd.ObservedParamShapes[uint(idx)] = [][]uint{}
		}
	}

	return fd
}

func (fd *FunctionDescriptor) FinalizeAnalysis() {
	if fd.ModuleType != nil {
		if *fd.ModuleType == "test" {
			for _, t := range fd.CalledTypes {
				if t == "logic" {
					fd.NeedAsync = false
					fd.AwaitToplevel = false
					break
				}
			}
		} else if *fd.ModuleType == "logic" || *fd.ModuleType == "hls" {
			fd.AwaitToplevel = false
		}
	}
}

func (fd *FunctionDescriptor) addArrayParamCall(pos uint, shape []uint) {
	for _, s := range fd.ObservedParamShapes[pos] {
		if equalArrays(s, shape) {
			return
		}
	}
	// fmt.Println("GOT", fd.MethodName, pos, shape)
	fd.ObservedParamShapes[pos] = append(fd.ObservedParamShapes[pos], shape)
}

func (fd *FunctionDescriptor) ResolveShapes() {
	fd.resolvedParamShapes = map[uint][]*uint{}
	for _, pos := range fd.arrayParams {
		shapes := fd.ObservedParamShapes[pos]
		if len(shapes) > 0 {
			fd.resolvedParamShapes[pos] = mergeShapes(shapes)
		}
	}
}

func (fd *FunctionDescriptor) ResolvedParamShape(name string) []*uint {
	if idx, ok := fd.ParamMap[name]; ok {
		return fd.resolvedParamShapes[idx]
	}
	return nil
}

func (fd *FunctionDescriptor) UpdateUsers(caller *FunctionDescriptor) {
	users := make(map[string]struct{})
	if caller.ModuleType != nil {
		users[*caller.ModuleType] = struct{}{}
	}
	for _, u := range caller.ModuleUsers {
		users[u] = struct{}{}
	}
	for user := range users {
		if (fd.ModuleType == nil || *fd.ModuleType != user) && !contains(fd.ModuleUsers, user) {
			fd.ModuleUsers = append(fd.ModuleUsers, user)
			fd.Dirty = true
			fmt.Fprintf(os.Stderr, "%s %s via %s\n", fd.MethodName, user, caller.MethodName)
		}
	}
}

func (fd *FunctionDescriptor) FlowAnalysis(checker *checker.Checker) {
	body := fd.Declaration.Body()
	if body == nil {
		return
	}

	runner := NewAbstractRunner(NewFlowAnalysis(checker))
	runner.Run(body.AsBlock())
	flow := runner.State

	if retVal, ok := flow.Returned.(AbstractArrayValue); ok {
		returns := map[string]int{}
		for idx, elm := range retVal.Elements {
			if id, ok := elm.(AbstractIdentifierValue); ok {
				name := id.Identifier.Name().Text()
				returns[name] = idx
			}
		}
		for _, param := range fd.Declaration.ParameterList().Nodes {
			name := param.Name().Text()
			if pos, ok := returns[name]; ok {
				fd.Passthrough[name] = uint(pos)
			}
		}
	}
}

func (f *FunctionDescriptor) IsReferenceParameter(name string, t *checker.Type, tc *checker.Checker) bool {
	if _, ok := f.Passthrough[name]; ok {
		return !checker.IsArrayType(tc, t)
	}
	return false
}

func (fd *FunctionDescriptor) GetReturnType(typ *checker.Type, tc *checker.Checker) *checker.Type {
	if checker.IsTypeReference(typ) && checker.IsTupleType(tc, typ) {
		elements := checker.ResolvedTypeArguments(typ)
		pass := make([]bool, len(elements))
		for _, idx := range fd.Passthrough {
			if idx >= 0 && int(idx) < len(pass) {
				pass[idx] = true
			}
		}
		filtered := []*checker.Type{}
		for i, el := range elements {
			if !pass[i] {
				filtered = append(filtered, el)
			}
		}
		if len(filtered) > 1 {
			panic(fmt.Sprintf("Failed to reduce function return %s", fd.MethodName))
		} else if len(filtered) == 1 {
			return filtered[0]
		}
		return nil
	}
	return typ
}

func (fd *FunctionDescriptor) GetReturnTypeNode(typ *ast.Node) *ast.Node {
	if ast.IsTupleTypeNode(typ) {
		elements := typ.AsTupleTypeNode().Elements.Nodes
		pass := make([]bool, len(elements))

		for _, idx := range fd.Passthrough {
			if idx >= 0 && int(idx) < len(pass) {
				pass[idx] = true
			}
		}

		var filtered []*ast.Node
		for idx, el := range elements {
			if !pass[idx] {
				filtered = append(filtered, el)
			}
		}

		if len(filtered) > 1 {
			assert(false, fmt.Sprintf("Failed to reduce function return %s", fd.MethodName))
		} else if len(filtered) == 1 {
			return filtered[0]
		} else {
			return nil
		}
	}

	return typ
}

func (fd *FunctionDescriptor) GetReturnExpression(expr *ast.Node) *ast.Node {
	if ast.IsArrayLiteralExpression(expr) {
		arrLit := expr.AsArrayLiteralExpression()
		pass := make([]bool, len(arrLit.Elements.Nodes))
		for _, idx := range fd.Passthrough {
			if idx >= 0 && int(idx) < len(pass) {
				pass[idx] = true
			}
		}
		filtered := []*ast.Node{}
		for i, el := range arrLit.Elements.Nodes {
			if !pass[i] {
				filtered = append(filtered, el)
			}
		}
		if len(filtered) > 1 {
			panic(fmt.Sprintf("Failed to reduce function return %s", fd.MethodName))
		} else if len(filtered) == 1 {
			return filtered[0]
		}
		return nil
	}
	return expr
}

func (fd *FunctionDescriptor) FilterCallerLhs(elements []*ast.Node) []*ast.Node {
	pass := make([]bool, len(elements))
	for _, idx := range fd.Passthrough {
		if idx >= 0 && int(idx) < len(pass) {
			pass[idx] = true
		}
	}

	var result []*ast.Node
	for idx, el := range elements {
		if !pass[idx] {
			result = append(result, el)
		}
	}

	return result
}

func equalArrays[T comparable](a, b []T) bool {
	if len(a) != len(b) {
		return false
	}
	for i, v := range a {
		if v != b[i] {
			return false
		}
	}
	return true
}

// mergeShapes nulls out dimensions that are not the same across all shapes
func mergeShapes(shapes [][]uint) []*uint {
	if len(shapes) == 0 {
		return nil
	}

	// Pop the last shape from the slice
	shape := shapes[len(shapes)-1]
	shapes = shapes[:len(shapes)-1]

	// Convert to []*int to allow nulls (represented by nil)
	result := make([]*uint, len(shape))
	for i := range shape {
		val := shape[i]
		result[i] = &val
	}

	// Compare with other shapes
	for _, other := range shapes {
		for i, val := range other {
			if result[i] != nil && *result[i] != val {
				result[i] = nil
			}
		}
	}

	return result
}

func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}
