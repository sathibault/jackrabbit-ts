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

type FunctionDescriptor struct {
	methodName          string
	declaration         *ast.Node
	arrayParams         []uint
	moduleType          *string
	moduleUsers         []string
	calledTypes         []string
	needAsync           bool
	awaitToplevel       bool
	inlined             bool
	paramMap            map[string]uint
	observedParamShapes map[uint][][]uint
	resolvedParamShapes map[uint][]*uint
	passthrough         map[string]uint
	globalRefs          map[string]map[string]struct{} // map to set
	rtlDescriptor       *RtlModDescriptor
	dirty               bool
}

func newFunctionDescriptor(name string, decl *ast.Node, tc *checker.Checker) *FunctionDescriptor {
	fd := &FunctionDescriptor{
		declaration:         decl,
		methodName:          name,
		moduleUsers:         []string{},
		calledTypes:         []string{},
		inlined:             false,
		needAsync:           false,
		awaitToplevel:       true,
		passthrough:         map[string]uint{},
		globalRefs:          map[string]map[string]struct{}{},
		dirty:               false,
		paramMap:            map[string]uint{},
		arrayParams:         []uint{},
		observedParamShapes: map[uint][][]uint{},
		resolvedParamShapes: map[uint][]*uint{},
	}

	for idx, p := range decl.ParameterList().Nodes {
		name := p.Name().Text()
		fd.paramMap[name] = uint(idx)
		typ := tc.GetTypeAtLocation(p)
		if checker.IsArrayType(tc, typ) {
			fd.arrayParams = append(fd.arrayParams, uint(idx))
			fd.observedParamShapes[uint(idx)] = [][]uint{}
		}
	}

	return fd
}

func (fd *FunctionDescriptor) InHlsSet() bool {
	if fd.moduleType != nil && *fd.moduleType == "hls" {
		return true
	}
	return contains(fd.moduleUsers, "hls")
}

func (fd *FunctionDescriptor) InFirmwareSet() bool {
	if fd.moduleType != nil && *fd.moduleType == "main" {
		return true
	}
	return contains(fd.moduleUsers, "main")
}

func (fd *FunctionDescriptor) FinalizeAnalysis() {
	if fd.moduleType != nil {
		if *fd.moduleType == "test" {
			for _, t := range fd.calledTypes {
				if t == "logic" {
					fd.needAsync = false
					fd.awaitToplevel = false
					break
				}
			}
		} else if *fd.moduleType == "logic" || *fd.moduleType == "hls" {
			fd.awaitToplevel = false
		}
	}
}

func (fd *FunctionDescriptor) addArrayParamCall(pos uint, shape []uint) {
	for _, s := range fd.observedParamShapes[pos] {
		if equalArrays(s, shape) {
			return
		}
	}
	// fmt.Fprintln(os.Stderr, "GOT", fd.MethodName, pos, shape)
	fd.observedParamShapes[pos] = append(fd.observedParamShapes[pos], shape)
}

func (fd *FunctionDescriptor) ResolveShapes() {
	fd.resolvedParamShapes = map[uint][]*uint{}
	for _, pos := range fd.arrayParams {
		shapes := fd.observedParamShapes[pos]
		if len(shapes) > 0 {
			fd.resolvedParamShapes[pos] = mergeShapes(shapes)
		}
	}
}

func (fd *FunctionDescriptor) ResolvedParamShape(name string) []*uint {
	if idx, ok := fd.paramMap[name]; ok {
		return fd.resolvedParamShapes[idx]
	}
	return nil
}

func (fd *FunctionDescriptor) UpdateUsers(caller *FunctionDescriptor) {
	users := make(map[string]struct{})
	if caller.moduleType != nil {
		users[*caller.moduleType] = struct{}{}
	}
	for _, u := range caller.moduleUsers {
		users[u] = struct{}{}
	}
	for user := range users {
		if (fd.moduleType == nil || *fd.moduleType != user) && !contains(fd.moduleUsers, user) {
			fd.moduleUsers = append(fd.moduleUsers, user)
			fd.dirty = true
			fmt.Fprintf(os.Stderr, "%s %s via %s\n", fd.methodName, user, caller.methodName)
		}
	}
}

func (fd *FunctionDescriptor) FlowAnalysis(den *RabbitDen, checker *checker.Checker) {
	body := fd.declaration.Body()
	if body == nil {
		return
	}

	runner := NewAbstractRunner(NewFlowAnalysis(den, checker))
	runner.Run(body.AsBlock())
	flow := runner.State

	if flow.returned != nil {
		if retVal, ok := flow.returned.(*AbstractArrayValue); ok {
			returns := map[string]int{}
			for idx, elm := range retVal.Elements {
				if id, ok := elm.(*AbstractIdentifierValue); ok {
					name := id.Identifier.Text
					returns[name] = idx
				}
			}
			for _, param := range fd.declaration.ParameterList().Nodes {
				name := param.Name().Text()
				if pos, ok := returns[name]; ok {
					fd.passthrough[name] = uint(pos)
				}
			}
		}
	}
}

func (f *FunctionDescriptor) IsReferenceParameter(name string, t *checker.Type, tc *checker.Checker) bool {
	if _, ok := f.passthrough[name]; ok {
		return !checker.IsArrayType(tc, t)
	}
	return false
}

func (fd *FunctionDescriptor) GetReturnType(typ *checker.Type, tc *checker.Checker) *checker.Type {
	if checker.IsTypeReference(typ) && checker.IsTupleType(tc, typ) {
		elements := checker.ResolvedTypeArguments(typ)
		pass := make([]bool, len(elements))
		for _, idx := range fd.passthrough {
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
			panic(fmt.Sprintf("Failed to reduce function return %s", fd.methodName))
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

		for _, idx := range fd.passthrough {
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
			assert(false, fmt.Sprintf("Failed to reduce function return %s", fd.methodName))
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
		for _, idx := range fd.passthrough {
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
			panic(fmt.Sprintf("Failed to reduce function return %s", fd.methodName))
		} else if len(filtered) == 1 {
			return filtered[0]
		}
		return nil
	}
	return expr
}

func (fd *FunctionDescriptor) FilterCallerLhs(elements []*ast.Node) []*ast.Node {
	pass := make([]bool, len(elements))
	for _, idx := range fd.passthrough {
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
