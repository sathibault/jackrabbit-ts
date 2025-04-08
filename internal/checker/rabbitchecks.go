package checker

import (
	"fmt"
	"math"
	"runtime"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/evaluator"
	"github.com/microsoft/typescript-go/internal/jsnum"
)

func QualifiedIdentName(ident *ast.Identifier, checker *Checker) *string {
  sym := checker.getSymbolAtLocation(ident.AsNode(), false)
	if sym != nil {
		return qualifiedName(sym, checker)
	}
	return nil
}

func QualifiedTypeName(t *Type, checker *Checker) *string {
  if t.symbol != nil {
		return qualifiedName(t.symbol, checker)
	}
  return nil
}

func qualifiedName(sym *ast.Symbol, checker *Checker) *string {
  name := sym.Name
  if sym.Flags&ast.SymbolFlagsAlias != 0 {
    sym = checker.getImmediateAliasedSymbol(sym)
  }
  if len(sym.Declarations) > 0 {
    decl := sym.Declarations[0]
    cur := decl
    for cur.Parent != nil {
      cur = cur.Parent
    }
    if ast.IsSourceFile(cur) {
			sourceFile := cur.AsSourceFile()
      name = fmt.Sprintf("%s::%s", sourceFile.FileName(), name)
    }
  }
  return &name
}

func QualifiedDeclName(decl *ast.Declaration) *string {
  if decl.Name() != nil {
		if ast.IsIdentifier(decl.Name()) {
      name := decl.Name().AsIdentifier().Text
      cur := decl
      for cur.Parent != nil {
        cur = cur.Parent
      }
			if ast.IsSourceFile(cur) {
				sourceFile := cur.AsSourceFile()
				name = fmt.Sprintf("%s::%s", sourceFile.FileName(), name)
			}
			return &name
    }
  }
  fmt.Println(decl, decl.Name().Kind.String())
  panic("invalid declaration name")
}

type TypeDescriptor struct {
	IsSigned bool
	Width    uint32
	Shape    []uint32
}

func GetDeclarationParameterTypes(c *Checker, node *ast.FunctionDeclaration) []*Type {
	s := c.getSignatureFromDeclaration(node.AsNode())
	types := make([]*Type, 0, len(s.parameters))
	for _, p := range s.parameters {
		t := c.getTypeOfSymbol(p)
		types = append(types, t)
	}
	return types
}

func GetDeclarationReturnType(c *Checker, node *ast.FunctionDeclaration) *Type {
	return c.getReturnTypeOfSignature(c.getSignatureFromDeclaration(node.AsNode()))
}

func RequireTypeDescriptor(tc *Checker, t *Type, expr *ast.Expression) *TypeDescriptor {
	info := GetTypeDescriptor(tc, t, expr, false)
	if info == nil {
		fmt.Println(tc.TypeToString(t))
		panic("Failed to get type descriptor")
	}
	return info
}

func GetTypeDescriptor(tc *Checker, t0 *Type, expr *ast.Expression, debug bool) *TypeDescriptor {
	t := tc.resolveRef(t0)
	if IsBitType(t) && ResolvedTypeArguments(t) != nil {
		isSigned, width := BitTypeDesc(t)
		return &TypeDescriptor{IsSigned: isSigned, Width: width}
	} else if tc.isArrayType(t) {
		idx := tc.getIndexInfoOfType(t, tc.numberType)
		dim := 1
		et := idx.valueType
		for tc.isArrayType(et) {
			idx = tc.getIndexInfoOfType(et, tc.numberType)
			et = idx.valueType
			dim++
		}
		desc := GetTypeDescriptor(tc, et, nil, debug)
		if debug {
			fmt.Println(dim, desc)
		}
		if desc != nil {
			if expr != nil {
				desc.Shape = ArrayExprShape(expr, tc)
			}
			if len(desc.Shape) == dim {
				return desc
			}
		}
	} else if (t.flags & TypeFlagsBoolean) != 0 {
		return &TypeDescriptor{IsSigned: false, Width: 1}
	} else {
		if expr != nil && isConstNumberExpression(expr) {
			value := TsEvaluateExpr(expr, tc)
			if num, ok := value.(float64); ok {
				isSigned := num < 0
				m := math.Abs(num)
				var width uint32 = 1
				for m >= math.Pow(2, float64(width)) {
					width++
				}
				if isSigned {
					width++
				}
				return &TypeDescriptor{IsSigned: isSigned, Width: width}
			}
		}
		if debug {
			fmt.Println(t, "UNRECOGNIZED")
		}
	}
	return nil
}

func IsArrayType(tc *Checker, t *Type) bool {
	return tc.isArrayType(t)
}

func GetArrayDim(tc *Checker, t *Type) int {
	if !tc.isArrayType(t) {
		panic("Expected array type")
	}
	idx := tc.getIndexInfoOfType(t, tc.numberType)
	dim := 1
	et := idx.valueType
	for tc.isArrayType(et) {
		idx = tc.getIndexInfoOfType(et, tc.numberType)
		et = idx.valueType
		dim++
	}
	return dim
}

func IsArrayConstructor(expr *ast.Expression) bool {
	if expr.Kind == ast.KindCallExpression {
		call := expr.AsCallExpression()
		if call.Expression.Kind == ast.KindIdentifier {
			id := call.Expression.AsIdentifier()
			return id.Text == "Array"
		}
	}
	return false
}

func ArrayExprShape(expr *ast.Expression, tc *Checker) []uint32 {
	var shape []uint32

	if expr.Kind == ast.KindCallExpression {
		call := expr.AsCallExpression()
		if call.Expression.Kind == ast.KindIdentifier {
			id := call.Expression.AsIdentifier()
			if id.Text == "Array" && len(call.Arguments.Nodes) == 1 {
				arg := call.Arguments.Nodes[0]
				val := TsEvaluateExpr(arg, tc)
				if intVal, ok := val.(uint32); ok {
					shape = append(shape, intVal)
					return shape
				}
			}
		}
	}

	for expr.Kind == ast.KindArrayLiteralExpression {
		lit := expr.AsArrayLiteralExpression()
		n := len(lit.Elements.Nodes)
		shape = append(shape, uint32(n))
		if len(lit.Elements.Nodes) > 0 {
			expr = lit.Elements.Nodes[0]
		} else {
			break
		}
	}

	return shape
}

func IsConstExpression(expr *ast.Node) bool {
	if expr.Kind == ast.KindPrefixUnaryExpression {
		unary := expr.AsPrefixUnaryExpression()
		if unary.Operator == ast.KindPlusToken || unary.Operator == ast.KindMinusToken {
			return ast.IsLiteralExpression(unary.Operand)
		}
	}
	return ast.IsLiteralExpression(expr) || isBoolLiteral(expr)
}

func isBoolLiteral(expr *ast.Node) bool {
	return expr.Kind == ast.KindTrueKeyword || expr.Kind == ast.KindFalseKeyword
}

func GetConstExpression(expr *ast.Node) any {
	var lit *ast.Node
	sign := 1

	if expr.Kind == ast.KindPrefixUnaryExpression {
		unary := expr.AsPrefixUnaryExpression()
		if unary.Operator == ast.KindPlusToken || unary.Operator == ast.KindMinusToken {
			if unary.Operator == ast.KindMinusToken {
				sign = -1
			}
			lit = unary.Operand
		} else {
			panic("internal error")
		}
	} else {
		lit = expr
	}

	switch lit.Kind {
	case ast.KindNumericLiteral:
		text := lit.AsNumericLiteral().Text
		num := jsnum.FromString(text)
		return num * jsnum.Number(sign)
	case ast.KindTrueKeyword:
		return true
	case ast.KindFalseKeyword:
		return false
	default:
		panic("unexpected literal kind")
	}
}

func ResolveIdentifierExpr(c *Checker, expr *ast.Node) (*ast.Symbol, any) {
	sym := c.GetSymbolAtLocation(expr)
	assert(sym != nil, "Expected symbol")

	imported := false
	if sym.Flags & ast.SymbolFlagsAlias != 0 {
		sym = c.getImmediateAliasedSymbol(sym)
		imported = true
	}

	if len(sym.Declarations) > 0 {
		decl := sym.Declarations[0]
		cur := decl
		for cur != nil && cur.Kind != ast.KindFunctionDeclaration && cur.Kind != ast.KindMethodDeclaration {
			cur = cur.Parent
		}

		if cur == nil {
			// Global
			if decl.Kind == ast.KindVariableDeclaration {
				varDecl := decl.AsVariableDeclaration()
				if (decl.Flags & ast.NodeFlagsConst != 0) || (varDecl.Parent.Flags&ast.NodeFlagsConst != 0) {
					if varDecl.Initializer != nil {
						value := TsEvaluateExpr(varDecl.Initializer, c)
						assert(value != nil, "Non-constant global")
						return sym, value
					}
				}
			}
			panic("Non-constant global")
		}
	}

	assert(!imported, "Non-constant import")
	return sym, nil
}

func TsEvaluateExpr(expr *ast.Expression, checker *Checker) any {
	evalEntity := func(expr *ast.Node, location *ast.Node) evaluator.Result {
		ident := expr.AsIdentifier()
		sym := ident.Symbol()
		if (sym.Flags & ast.SymbolFlagsAlias) != 0 {
			sym = checker.getImmediateAliasedSymbol(sym)
		}
		if len(sym.Declarations) > 0 {
			decl := sym.Declarations[0]
			if ast.IsVariableDeclaration(decl) {
				if ((decl.Flags & ast.NodeFlagsConst) != 0) || (decl.Parent != nil && (decl.Parent.Flags & ast.NodeFlagsConst) != 0) {
					if decl.Initializer() != nil {
						val := TsEvaluateExpr(decl.Initializer(), checker)
						return evaluator.Result{Value: val, IsSyntacticallyString: false, ResolvedOtherFiles: false, HasExternalReferences: false}
					}
				}
			}
		}
		return evaluator.Result{Value: nil, IsSyntacticallyString: false, ResolvedOtherFiles: false, HasExternalReferences: false}
	}
	eval := evaluator.NewEvaluator(evalEntity, 0)
	result := eval(expr, nil)
	return result.Value
}

func HasBaseType(t *Type, base string) bool {
	if !IsTypeReference(t) {
		return false
	}
	return checkBaseType(t, t, base, 0)
}

func checkBaseType(t, query *Type, base string, depth int) bool {
	if depth > 10 || (depth > 0 && t == query) {
		return false
	}
	if t.objectFlags&ObjectFlagsReference != 0 {
		data := t.AsTypeReference()
		if data.symbol.Name == base {
			rtlCache[query.id] = rtl_table{query, data}
			return true
		}
	}
	if t.objectFlags&(ObjectFlagsClassOrInterface|ObjectFlagsReference) != 0 {
		target := getTargetType(t)
		return target != nil && t.checker != nil && core.Some(t.checker.getBaseTypes(target), func(t *Type) bool {
			return checkBaseType(t, query, base, depth+1)
		})
	} else if t.flags&TypeFlagsIntersection != 0 {
		return core.Some(t.AsIntersectionType().types, func(t *Type) bool {
			return checkBaseType(t, query, base, depth+1)
		})
	}
	return false
}

func IsPrimitive(t *Type) bool {
	if IsBitType(t) {
		return true
	}
	flags := t.flags
	return flags & (TypeFlagsStringLike|TypeFlagsNumberLike|TypeFlagsBigIntLike|TypeFlagsEnumLike|TypeFlagsBooleanLike) != 0
}

func IsObjectType(t *Type) bool {
	return (t.flags & TypeFlagsObject) != 0
}

func IsTypeReference(t *Type) bool {
	return t.objectFlags&ObjectFlagsReference != 0
}


func IsTypeReferenceOf(t *Type, name string) bool {
	if t.objectFlags&ObjectFlagsReference != 0 {
		data := t.AsTypeReference()
		return data.symbol != nil && data.symbol.Name == name
	}
	return false
}

func GetObjectFlags(t *Type) ObjectFlags {
	return t.objectFlags
}

func ParseTypeSpec(spec string) TypeDescriptor {
	if len(spec) >= 4 && spec[:4] == "uint" {
		return TypeDescriptor{IsSigned: false, Width: mustParseInt(spec[4:])}
	} else if len(spec) >= 3 && spec[:3] == "int" {
		return TypeDescriptor{IsSigned: true, Width: mustParseInt(spec[3:])}
	}
	panic("Unrecognized type spec " + spec)
}

func mustParseInt(s string) uint32 {
	var i uint32
	_, err := fmt.Sscanf(s, "%d", &i)
	if err != nil {
		panic(err)
	}
	return i
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
