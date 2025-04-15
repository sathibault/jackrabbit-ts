package jackrabbit

import (
	"fmt"
	"os"
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/core"
)

type AbstractValue interface {
	Kind() string
	Equals(other AbstractValue) bool
}

type AbstractLiteralValue struct {
	Literal *ast.Expression
}

func (v AbstractLiteralValue) Kind() string { return "literal" }
func (v AbstractLiteralValue) Equals(other AbstractValue) bool {
	o, ok := other.(AbstractLiteralValue)
	if !ok {
		return false
	}
	return o.Literal.Text() == v.Literal.Text()
}

type AbstractIdentifierValue struct {
	Identifier *ast.Identifier
}

func (v AbstractIdentifierValue) Equals(other AbstractValue) bool {
	o, ok := other.(AbstractIdentifierValue)
	if !ok {
		return false
	}
	return o.Identifier.Text == v.Identifier.Text
}

func (v AbstractIdentifierValue) Kind() string { return "identifier" }

type AbstractArrayValue struct {
	Elements []AbstractValue
}

func (v AbstractArrayValue) Kind() string { return "array" }
func (v AbstractArrayValue) Equals(other AbstractValue) bool {
	o, ok := other.(AbstractArrayValue)
	if !ok || len(v.Elements) != len(o.Elements) {
		return false
	}
	for i := range v.Elements {
		if !v.Elements[i].Equals(o.Elements[i]) {
			return false
		}
	}
	return true
}

type AbstractObjectValue struct {
	Properties map[string]AbstractValue
}

func (v AbstractObjectValue) Kind() string { return "object" }
func (v AbstractObjectValue) Equals(other AbstractValue) bool {
	o, ok := other.(AbstractObjectValue)
	if !ok || len(v.Properties) != len(o.Properties) {
		return false
	}
	for key, val := range v.Properties {
		otherVal, exists := o.Properties[key]
		if !exists || !val.Equals(otherVal) {
			return false
		}
	}
	return true
}

type AbstractUnknownValue struct {
	Shape []int // this is signed to use -1 for unknown dimensions
}

func (v AbstractUnknownValue) Kind() string { return "unknown" }
func (v AbstractUnknownValue) Equals(other AbstractValue) bool {
	o, ok := other.(AbstractUnknownValue)
	if !ok || len(v.Shape) != len(o.Shape) {
		return false
	}
	for i := range v.Shape {
		if v.Shape[i] != o.Shape[i] {
			return false
		}
	}
	return true
}

type AbstractUndefinedValue struct{}

func (v AbstractUndefinedValue) Kind() string { return "undefined" }
func (v AbstractUndefinedValue) Equals(other AbstractValue) bool {
	_, ok := other.(AbstractUndefinedValue)
	return ok
}

type AbstractStore struct {
	Variables map[string]AbstractValue
}

func NewAbstractStore() *AbstractStore {
	return &AbstractStore{
		Variables: make(map[string]AbstractValue),
	}
}

func (s *AbstractStore) Clone() *AbstractStore {
	copy := NewAbstractStore()
	for k, v := range s.Variables {
		copy.Variables[k] = v // Assuming deep copy if necessary
	}
	return copy
}

func (s *AbstractStore) Merge(other *AbstractStore) bool {
	conflicts := 0
	for k, v := range other.Variables {
		if existing, ok := s.Variables[k]; ok && existing.Kind() != "unknown" && existing.Kind() != "undefined" {
			if !abstractEqual(existing, v) {
				s.Variables[k] = AbstractUnknownValue{}
				conflicts++
			}
		} else {
			s.Variables[k] = v
		}
	}
	return conflicts > 0
}

func (s *AbstractStore) ToCompactString() string {
	var strs []string
	for k, v := range s.Variables {
		switch val := v.(type) {
		case AbstractLiteralValue:
			strs = append(strs, fmt.Sprintf("%s: %s", k, val.Literal.Text()))
		case AbstractUnknownValue:
			// Skip unknowns
		default:
			strs = append(strs, fmt.Sprintf("%s: %s", k, val.Kind()))
		}
	}
	return strings.Join(strs, " ")
}

type FlowAnalysis struct {
	den      *RabbitDen
	store    *AbstractStore
	returned AbstractValue
	indent   string
	tc       *checker.Checker
}

func NewFlowAnalysis(den *RabbitDen, c *checker.Checker) *FlowAnalysis {
	return &FlowAnalysis{
		den:    den,
		store:  NewAbstractStore(),
		indent: "",
		tc:     c,
	}
}

func (fa *FlowAnalysis) Enter(stmt *ast.Node) {
	// Optional: Implement logging or indentation
}

func (fa *FlowAnalysis) Leave(stmt *ast.Node) {
	// Optional: Implement logging or indentation
}

func (fa *FlowAnalysis) Apply(stmt *ast.Node) {
	fa.Visit(stmt)
}

func (fa *FlowAnalysis) Merge(other AbstractState) bool {
	o := other.(*FlowAnalysis)
	return fa.store.Merge(o.store)
}

func (fa *FlowAnalysis) Clone() AbstractState {
	copy := NewFlowAnalysis(fa.den, fa.tc)
	copy.store = fa.store.Clone()
	copy.returned = fa.returned
	copy.indent = fa.indent
	return copy
}

func (fa *FlowAnalysis) Dump() {
	fmt.Fprintln(os.Stderr, "STORE", fa.store.ToCompactString())
}

func (f *FlowAnalysis) Visit(n *ast.Node) {
	switch n.Kind {
	case ast.KindBinaryExpression:
		expr := n.AsBinaryExpression()
		switch expr.OperatorToken.Kind {
		case ast.KindEqualsToken:
			// ok
		case ast.KindPlusEqualsToken, ast.KindMinusEqualsToken, ast.KindAsteriskAsteriskEqualsToken, ast.KindAsteriskEqualsToken, ast.KindSlashEqualsToken,
			ast.KindPercentEqualsToken, ast.KindLessThanLessThanEqualsToken, ast.KindGreaterThanGreaterThanEqualsToken, ast.KindGreaterThanGreaterThanGreaterThanEqualsToken,
			ast.KindAmpersandEqualsToken, ast.KindCaretEqualsToken, ast.KindBarEqualsToken, ast.KindBarBarEqualsToken, ast.KindAmpersandAmpersandEqualsToken, ast.KindQuestionQuestionEqualsToken:
			f.killLhs(expr.Left)
		}

	case ast.KindPrefixUnaryExpression:
		pre := n.AsPrefixUnaryExpression()
		if pre.Operator == ast.KindPlusPlusToken || pre.Operator == ast.KindMinusMinusToken {
			f.killLhs(pre.Operand)
		}

	case ast.KindPostfixUnaryExpression:
		post := n.AsPostfixUnaryExpression()
		if post.Operator == ast.KindPlusPlusToken || post.Operator == ast.KindMinusMinusToken {
			f.killLhs(post.Operand)
		}

	case ast.KindVariableDeclaration:
		decl := n.AsVariableDeclaration()
		if decl.Initializer != nil {
			val := toAbstract(decl.Initializer, f.tc)
			if val != nil {
				f.store.Variables[decl.Symbol.Name] = val
			} else {
				f.store.Variables[decl.Symbol.Name] = &AbstractUndefinedValue{}
			}
		}
		return

	case ast.KindReturnStatement:
		ret := n.AsReturnStatement()
		var val AbstractValue = &AbstractUndefinedValue{}
		if ret.Expression != nil {
			if v := toAbstract(ret.Expression, f.tc); v != nil {
				val = v
			}
		}
		if f.returned == nil {
			f.returned = val
		} else {
			f.returned = mergeAbstract(f.returned, val)
		}
		return

	case ast.KindCallExpression:
		call := n.AsCallExpression()
		callee := f.den.getCalleeDescriptor(call, f.tc)
		if callee != nil {
			for _, pos := range callee.arrayParams {
				shape := f.evalShape(call.Arguments.Nodes[pos])
				if shape != nil {
					callee.addArrayParamCall(pos, core.Map(shape, intToUint))
				}
			}
		}
	}

	n.ForEachChild(func(n *ast.Node) bool {
		f.Visit(n)
		return false
	})
}

func (fa *FlowAnalysis) evalShape(expr *ast.Expression) []int {
	if ast.IsIdentifier(expr) {
		return fa.lookupShape(expr.AsIdentifier().Text)
	}
	shape := checker.ArrayExprShape(expr, fa.tc)
	return core.Map(shape, uint32ToInt)
}

func (fa *FlowAnalysis) lookupShape(name string) []int {
	value, ok := fa.store.Variables[name]
	if !ok {
		return nil
	}

	switch v := value.(type) {
	case *AbstractArrayValue:
		return abstractArrayShape(v.Elements)

	case *AbstractLiteralValue:
		return fa.evalShape(v.Literal)

	case *AbstractIdentifierValue:
		return fa.lookupShape(v.Identifier.Text)

	case *AbstractUnknownValue:
		return v.Shape
	}

	return nil
}

func (f *FlowAnalysis) assign(lhs *ast.Expression, rhs *ast.Expression) {
	val := toAbstract(rhs, f.tc)
	if val != nil {
		if ast.IsIdentifier(lhs) {
			ident := lhs.AsIdentifier()
			f.store.Variables[ident.Text] = val
		} else if ast.IsPropertyAccessExpression(lhs) {
		}
	} else {
		f.killLhs(lhs)
	}
}

func (f *FlowAnalysis) killLhs(expr *ast.Expression) {
	switch expr.Kind {
	case ast.KindIdentifier:
		f.store.Variables[expr.AsIdentifier().Text] = &AbstractUnknownValue{}
	}
}

func getAccessPath(expr *ast.Expression) (string, []string) {
	var result []string
	for {
		switch expr.Kind {
		case ast.KindPropertyAccessExpression:
			access := expr.AsPropertyAccessExpression()
			result = append(result, access.Text())
			expr = access.Expression

		case ast.KindIdentifier:
			return expr.AsIdentifier().Text, result

		case ast.KindThisKeyword:
			return "this", result

		default:
			panic("Internal error: getAccessPath")
		}
	}
}

func toAbstract(expr *ast.Expression, tc *checker.Checker) AbstractValue {
	switch expr.Kind {
	case ast.KindIdentifier:
		return &AbstractIdentifierValue{Identifier: expr.AsIdentifier()}

	case ast.KindArrayLiteralExpression:
		var elements []AbstractValue
		for _, el := range expr.AsArrayLiteralExpression().Elements.Nodes {
			elements = append(elements, toAbstract(el, tc))
		}
		return &AbstractArrayValue{Elements: elements}

	case ast.KindCallExpression:
		shape := checker.ArrayExprShape(expr, tc)
		if shape != nil {
			return &AbstractUnknownValue{Shape: core.Map(shape, uint32ToInt)}
		}
		return &AbstractUnknownValue{}

	case ast.KindNumericLiteral, ast.KindStringLiteral, ast.KindTrueKeyword, ast.KindFalseKeyword:
		return &AbstractLiteralValue{Literal: expr}

	}

	return nil
}

func mergeAbstract(x, y AbstractValue) AbstractValue {
	if abstractEqual(x, y) {
		return x
	}
	if x.Kind() == "unknown" && y.Kind() == "unknown" {
		xu := x.(*AbstractUnknownValue)
		yu := y.(*AbstractUnknownValue)
		if xu.Shape != nil && yu.Shape != nil && len(xu.Shape) == len(yu.Shape) {
			result := make([]int, len(xu.Shape))
			for i := range xu.Shape {
				if xu.Shape[i] == yu.Shape[i] {
					result[i] = xu.Shape[i]
				} else {
					result[i] = -1 // use -1 for unknown shape dimension
				}
			}
			return &AbstractUnknownValue{Shape: result}
		}
	}
	return &AbstractUnknownValue{}
}

func abstractEqual(x, y AbstractValue) bool {
	if x.Kind() != y.Kind() {
		return false
	}
	return x.Equals(y)
}

func abstractArrayShape(elements []AbstractValue) []int {
	shape := []int{len(elements)}
	if len(elements) == 0 {
		return shape
	}

	elm := elements[0]
	for {
		arrayVal, ok := elm.(*AbstractArrayValue)
		if !ok {
			break
		}
		shape = append(shape, len(arrayVal.Elements))
		if len(arrayVal.Elements) == 0 {
			break
		}
		elm = arrayVal.Elements[0]
	}

	return shape
}

func uintToInt(x uint) int {
	return int(x)
}

func uint32ToInt(x uint32) int {
	return int(x)
}

func intToUint(x int) uint {
	return uint(x)
}

func uint32ToUint(x uint32) uint {
	return uint(x)
}
