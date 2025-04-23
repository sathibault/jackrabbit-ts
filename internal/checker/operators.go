package checker

import (
	"fmt"
	"log"
	"os"
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/jsnum"
)

func checkUnaryOpOverload(operator ast.Kind, exprType *Type) *Type {
	switch operator {
	case ast.KindPlusToken, ast.KindMinusToken, ast.KindTildeToken, ast.KindPlusPlusToken, ast.KindMinusMinusToken:
		if IsBitType(exprType) && ResolvedTypeArguments(exprType) != nil {
			return exprType
		} else if IsRtlType(exprType) && ResolvedTypeArguments(exprType) != nil {
			return exprType
		}
	case ast.KindExclamationToken:
		if (IsBitType(exprType) || IsRtlType(exprType)) && ResolvedTypeArguments(exprType) != nil {
			return makeRtlBase(exprType, exprType.checker.booleanType)
		}
	}
	return nil
}

func checkBinaryOpOverload(leftType *Type, left *ast.Node, operatorToken *ast.Node, rightType *Type, right *ast.Node) *Type {
	operator := operatorToken.Kind
	switch operator {
	case ast.KindLessThanToken, ast.KindGreaterThanToken, ast.KindLessThanEqualsToken, ast.KindGreaterThanEqualsToken,
		ast.KindEqualsEqualsToken, ast.KindExclamationEqualsToken, ast.KindEqualsEqualsEqualsToken, ast.KindExclamationEqualsEqualsToken:
		if IsBitType(leftType) && ResolvedTypeArguments(leftType) != nil && isConstNumberExpression(right) {
			return leftType.checker.booleanType
		} else if IsBitType(rightType) && ResolvedTypeArguments(rightType) != nil && isConstNumberExpression(left) {
			return rightType.checker.booleanType
		}
		if IsRtlType(leftType) && ResolvedTypeArguments(leftType) != nil &&
			IsRtlType(rightType) && ResolvedTypeArguments(rightType) != nil {
			return makeRtlBase(leftType, leftType.checker.booleanType)
		}
		if IsRtlType(leftType) && ResolvedTypeArguments(leftType) != nil {
			if isConstNumberExpression(right) {
				return makeRtlBase(leftType, leftType.checker.booleanType)
			} else if isEqualLike(operator) && isStringGroup(ResolvedTypeArguments(leftType)[0]) && isStringGroup(rightType) {
				return makeRtlBase(leftType, leftType.checker.booleanType)
			}
		}
		if IsRtlType(rightType) && ResolvedTypeArguments(rightType) != nil {
			if isConstNumberExpression(left) {
				return makeRtlBase(rightType, rightType.checker.booleanType)
			} else if isEqualLike(operator) && isStringGroup(leftType) && isStringGroup(ResolvedTypeArguments(rightType)[0]) {
				return makeRtlBase(rightType, rightType.checker.booleanType)
			}
		}

	case ast.KindAmpersandAmpersandToken, ast.KindBarBarToken:
		if IsBitType(leftType) && ResolvedTypeArguments(leftType) != nil {
			return leftType.checker.booleanType
		} else if IsBitType(rightType) && ResolvedTypeArguments(rightType) != nil {
			return rightType.checker.booleanType
		}
		if IsRtlType(leftType) && ResolvedTypeArguments(leftType) != nil {
			return makeRtlBase(leftType, leftType.checker.booleanType)
		} else if IsRtlType(rightType) && ResolvedTypeArguments(rightType) != nil {
			return makeRtlBase(rightType, rightType.checker.booleanType)
		}

	case ast.KindLessThanLessThanToken, ast.KindGreaterThanGreaterThanToken:
		if IsBitType(leftType) && ResolvedTypeArguments(leftType) != nil &&
			IsBitType(rightType) && ResolvedTypeArguments(rightType) != nil {
			return leftType
		}
		if IsBitType(leftType) && ResolvedTypeArguments(leftType) != nil && isConstNumberExpression(right) {
			return leftType
		}
		if IsRtlType(leftType) && ResolvedTypeArguments(leftType) != nil &&
			IsRtlType(rightType) && ResolvedTypeArguments(rightType) != nil {
			return toRtlBase(leftType)
		}

	case ast.KindBarToken, ast.KindCaretToken, ast.KindAmpersandToken, ast.KindPlusToken, ast.KindMinusToken, ast.KindAsteriskToken, ast.KindSlashToken:
		return usualBinaryOverload(leftType, left, rightType, right, false)

	case ast.KindHashPlusToken, ast.KindHashMinusToken:
		return usualBinaryOverload(leftType, left, rightType, right, true)

	case ast.KindHashAsteriskToken:
		return extendedMultiplyOverload(leftType, left, rightType, right)

	case ast.KindEqualsToken, ast.KindBarEqualsToken, ast.KindCaretEqualsToken, ast.KindAmpersandEqualsToken,
		ast.KindPlusEqualsToken, ast.KindMinusEqualsToken, ast.KindAsteriskEqualsToken,
		ast.KindLessThanLessThanEqualsToken, ast.KindGreaterThanGreaterThanEqualsToken:
		if IsBitType(leftType) && ResolvedTypeArguments(leftType) != nil {
			if rightType.flags&TypeFlagsNumberLiteral != 0 || isConstNumberExpression(right) {
				return leftType
			}
			if assignableToBitType(rightType, leftType) {
				return leftType
			}
		}
	}
	return nil
}

func usualBinaryOverload(leftType *Type, left *ast.Node, rightType *Type, right *ast.Node, carry bool) *Type {
	if IsBitType(leftType) && ResolvedTypeArguments(leftType) != nil &&
		IsBitType(rightType) && ResolvedTypeArguments(rightType) != nil {
		w1 := getNumberLiteralValue(ResolvedTypeArguments(leftType)[0])
		w2 := getNumberLiteralValue(ResolvedTypeArguments(rightType)[0])
		ext := 0
		if carry {
			if w1 == w2 {
				s1 := isSignedBitType(leftType)
				s2 := isSignedBitType(rightType)
				if s1 == s2 {
					ext = 1
				} else {
					ext = 2
				}
			}
		}
		rw := max(w1, w2) + jsnum.Number(ext)
		return makeBinaryResultType(leftType, rightType, rw)
	} else if IsBitType(leftType) && ResolvedTypeArguments(leftType) != nil && isConstNumberExpression(right) {
		if carry {
			w1 := getNumberLiteralValue(ResolvedTypeArguments(leftType)[0])
			return makeBinaryResultType(leftType, rightType, w1)
		}
		return leftType
	} else if IsBitType(rightType) && ResolvedTypeArguments(rightType) != nil && isConstNumberExpression(left) {
		if carry {
			w1 := getNumberLiteralValue(ResolvedTypeArguments(rightType)[0])
			return makeBinaryResultType(leftType, rightType, w1)
		}
		return rightType
	}

	if IsRtlType(leftType) && ResolvedTypeArguments(leftType) != nil &&
		IsRtlType(rightType) && ResolvedTypeArguments(rightType) != nil {
		arg1 := ResolvedTypeArguments(leftType)[0]
		arg2 := ResolvedTypeArguments(rightType)[0]
		if IsBitType(arg1) && ResolvedTypeArguments(arg1) != nil &&
			IsBitType(arg2) && ResolvedTypeArguments(arg2) != nil {
			w1 := getNumberLiteralValue(ResolvedTypeArguments(arg1)[0])
			w2 := getNumberLiteralValue(ResolvedTypeArguments(arg2)[0])
			ext := 0
			if carry {
				if w1 == w2 {
					s1 := isSignedBitType(arg1)
					s2 := isSignedBitType(arg2)
					if s1 == s2 {
						ext = 1
					} else {
						ext = 2
					}
				}
			}
			rw := max(w1, w2) + jsnum.Number(ext)
			return makeBinaryResultType(leftType, rightType, rw)
		}
	} else if IsRtlType(leftType) && ResolvedTypeArguments(leftType) != nil && isConstNumberExpression(right) {
		if carry {
			arg1 := ResolvedTypeArguments(leftType)[0]
			if IsBitType(arg1) && ResolvedTypeArguments(arg1) != nil {
				w1 := getNumberLiteralValue(ResolvedTypeArguments(arg1)[0])
				makeRtlBase(leftType, makeBinaryResultType(arg1, arg1, w1+jsnum.Number(1)))
			}
		} else {
			return toRtlBase(leftType)
		}
	} else if IsRtlType(rightType) && ResolvedTypeArguments(rightType) != nil && isConstNumberExpression(left) {
		if carry {
			arg2 := ResolvedTypeArguments(rightType)[0]
			if IsBitType(arg2) && ResolvedTypeArguments(arg2) != nil {
				w1 := getNumberLiteralValue(ResolvedTypeArguments(arg2)[0])
				makeRtlBase(rightType, makeBinaryResultType(arg2, arg2, w1+jsnum.Number(1)))
			}
		} else {
			return toRtlBase(rightType)
		}
	}
	return nil
}

func extendedMultiplyOverload(leftType *Type, left *ast.Node, rightType *Type, right *ast.Node) *Type {
	if IsBitType(leftType) && ResolvedTypeArguments(leftType) != nil &&
		IsBitType(rightType) && ResolvedTypeArguments(rightType) != nil {
		w1 := getNumberLiteralValue(ResolvedTypeArguments(leftType)[0])
		w2 := getNumberLiteralValue(ResolvedTypeArguments(rightType)[0])
		rw := w1 + w2
		return makeBinaryResultType(leftType, rightType, rw)
	}

	if IsRtlType(leftType) && ResolvedTypeArguments(leftType) != nil &&
		IsRtlType(rightType) && ResolvedTypeArguments(rightType) != nil {
		arg1 := ResolvedTypeArguments(leftType)[0]
		arg2 := ResolvedTypeArguments(rightType)[0]
		if IsBitType(arg1) && ResolvedTypeArguments(arg1) != nil &&
			IsBitType(arg2) && ResolvedTypeArguments(arg2) != nil {
			w1 := getNumberLiteralValue(ResolvedTypeArguments(arg1)[0])
			w2 := getNumberLiteralValue(ResolvedTypeArguments(arg2)[0])
			rw := w1 + w2
			return makeBinaryResultType(leftType, rightType, rw)
		}
	}
	if IsBitType(leftType) || IsRtlType(leftType) {
		fmt.Fprintln(os.Stderr, "Binary fail", leftType.checker.TypeToString(leftType), rightType.checker.TypeToString(rightType))
		DumpType(leftType)
		DumpType(rightType)
	}
	return nil
}

func makeBinaryResultType(leftType *Type, rightType *Type, width jsnum.Number) *Type {
	arg := leftType.checker.getNumberLiteralType(width)
	if isSignedBitType(leftType) {
		data := leftType.AsTypeReference()
		return leftType.checker.createTypeReference(data.target, []*Type{arg})
	}
	data := rightType.AsTypeReference()
	return rightType.checker.createTypeReference(data.target, []*Type{arg})
}

func checkInitializerOverload(target *Type, initType *Type, initializer *ast.Node) bool {
	if IsBitType(target) && ResolvedTypeArguments(target) != nil {
		if isConstNumberExpression(initializer) {
			return true
		}
		okay := assignableToBitType(initType, target)
		if !okay {
			log.Println("checkInitializerOverload fail", target.checker.TypeToString(target), initType.checker.TypeToString(initType), exprToString(initializer, target.checker))
		}
		return okay
	} else if IsRtlType(target) && ResolvedTypeArguments(target) != nil {
		if isConstNumberExpression(initializer) {
			return true
		}
		okay := assignableToRtlType(initType, target)
		if !okay {
			log.Println("checkInitializerOverload fail", target.checker.TypeToString(target), initType.checker.TypeToString(initType), exprToString(initializer, target.checker))
		}
		return okay
	}
	return false
}

func assignableToBitType(source *Type, target *Type) bool {
	if source.flags&TypeFlagsNumberLiteral != 0 {
		return true
	} else if HasKnownTypeArguments(target, 1) {
		if IsBitType(source) && HasKnownTypeArguments(source, 1) {
			s1 := isSignedBitType(target)
			s2 := isSignedBitType(source)
			w1 := ResolvedTypeArguments(target)[0].AsLiteralType().value
			w2 := ResolvedTypeArguments(source)[0].AsLiteralType().value
			return s1 == s2 && w1 == w2
		} else if source.flags&TypeFlagsBooleanLike != 0 {
			w := ResolvedTypeArguments(target)[0].AsLiteralType().value
			log.Println("bool to bits", target.checker.TypeToString(target), source.checker.TypeToString(source), !isSignedBitType(target), w)
			return !isSignedBitType(target) && w == 1
		} else if source.flags&TypeFlagsUnion != 0 {
			subs := source.AsUnionType().types
			for _, subType := range subs {
				if !assignableToBitType(subType, target) {
					return false
				}
			}
			return true
		}
	}
	return false
}

func assignableToRtlType(source *Type, target *Type) bool {
	if source.flags&TypeFlagsNumberLiteral != 0 {
		return true
	} else if ResolvedTypeArguments(target) != nil {
		tgtArg := ResolvedTypeArguments(target)[0]
		if IsRtlType(source) && ResolvedTypeArguments(source) != nil {
			srcArg := ResolvedTypeArguments(source)[0]
			if IsBitType(srcArg) && ResolvedTypeArguments(srcArg) != nil {
				if IsBitType(tgtArg) && ResolvedTypeArguments(tgtArg) != nil {
					return assignableToBitType(srcArg, tgtArg)
				}
			} else if srcArg.flags&TypeFlagsBooleanLike != 0 {
				if IsBitType(tgtArg) && ResolvedTypeArguments(tgtArg) != nil {
					return assignableToBitType(srcArg, tgtArg)
				}
			}
		} else if IsBitType(source) && ResolvedTypeArguments(source) != nil {
			if IsBitType(tgtArg) && ResolvedTypeArguments(tgtArg) != nil {
				return assignableToBitType(source, tgtArg)
			}
		} else if source.flags&TypeFlagsUnion != 0 {
			subs := source.AsUnionType().types
			for _, subType := range subs {
				if !assignableToRtlType(subType, target) {
					return false
				}
			}
			return true
		}
	}
	// fmt.Fprintln(os.Stderr, "RTL fail")
	// DumpType(source)
	// DumpType(target)
	return false
}

func isConstNumberExpression(expr *ast.Node) bool {
	if expr.Kind == ast.KindPrefixUnaryExpression {
		unary := expr.AsPrefixUnaryExpression()
		if unary.Operator == ast.KindPlusToken || unary.Operator == ast.KindMinusToken {
			return unary.Operand.Kind == ast.KindNumericLiteral
		}
	}
	return expr.Kind == ast.KindNumericLiteral
}

func isSignedBitType(t *Type) bool {
	data := t.AsTypeReference()
	return data.symbol.Name == "Int"
}

func isTypeReference(t *Type) bool {
	return t.objectFlags&ObjectFlagsReference != 0
}

func isNumberType(t *Type) bool {
	return (t.flags&TypeFlagsNumberLike) != 0 || (t.flags&TypeFlagsBigIntLike) != 0
}

func toRtlBase(t *Type) *Type {
	typeArgs := ResolvedTypeArguments(t)
	if typeArgs != nil && len(typeArgs) > 0 {
		return makeRtlBase(t, typeArgs[0])
	}
	log.Fatal("Internal error: toRtlBase")
	return nil
}

func IsBitType(t *Type) bool {
	if t.objectFlags&ObjectFlagsReference != 0 {
		data := t.AsTypeReference()
		return data.symbol != nil && (data.symbol.Name == "UInt" || data.symbol.Name == "Int")
	}
	return false
}

func BitTypeDesc(t *Type) (bool, uint32) {
	data := t.AsTypeReference()
	isSigned := data.symbol.Name == "Int"
	width := getNumberLiteralValue(ResolvedTypeArguments(t)[0])
	return isSigned, uint32(width)
}

func makeRtlBase(t *Type, arg *Type) *Type {
	base := rtlBaseType(t)
	return base.checker.createTypeReference(base.target, []*Type{arg})
}

type rtl_table struct {
	t   *Type
	rtl *TypeReference
}

var rtlCache = make(map[TypeId]rtl_table)

func IsRtlBitType(t *Type) bool {
	if IsRtlType(t) && ResolvedTypeArguments(t) != nil {
		arg := ResolvedTypeArguments(t)[0]
		if IsBitType(arg) {
			return true
		}
	}
	return false
}

func IsRtlType(t *Type) bool {
	_, ok := rtlCache[t.id]
	if ok && rtlCache[t.id].t == t {
		return true
	}
	var check func(*Type, *Type, int) bool
	check = func(t *Type, query *Type, depth int) bool {
		if t == nil {
			return false
		}
		if depth > 10 {
			return false // run-away recursion
		}
		if depth > 0 && t == query {
			return false
		}
		if t.objectFlags&ObjectFlagsReference != 0 {
			data := t.AsTypeReference()
			if data != nil && data.symbol != nil && data.symbol.Name == "RtlExpr" {
				rtlCache[query.id] = rtl_table{query, data}
				return true
			}
		}
		if t.objectFlags&(ObjectFlagsClassOrInterface|ObjectFlagsReference) != 0 {
			target := getTargetType(t)
			if target != nil && t.checker != nil {
				if core.Some(t.checker.getBaseTypes(target), func(t *Type) bool {
					return check(t, query, depth+1)
				}) {
					return true
				}
				if core.Some(t.checker.getImplementsTypes(target), func(t *Type) bool {
					return check(t, query, depth+1)
				}) {
					return true
				}
			}
		}
		if t.flags&TypeFlagsIntersection != 0 {
			if core.Some(t.AsIntersectionType().types, func(t *Type) bool {
				return check(t, query, depth+1)
			}) {
				return true
			}
		}
		return false
	}
	return (t.objectFlags&ObjectFlagsReference) != 0 && check(t, t, 0)
}

func (c *Checker) getImplementsTypes(t *Type) []*Type {
	implemented := make([]*Type, 8)
	if t.symbol != nil && len(t.symbol.Declarations) > 0 {
		for _, n := range t.symbol.Declarations {
			if ast.IsClassLike(n) {
				implementedTypeNodes := ast.GetImplementsHeritageClauseElements(n)
				for _, typeRefNode := range implementedTypeNodes {
					implementsType := c.getTypeFromTypeNode(typeRefNode)
					if !c.isErrorType(implementsType) {
						implemented = append(implemented, implementsType)
					}
				}
			}
		}
	}
	return implemented
}

func rtlBaseType(t *Type) *TypeReference {
	value, ok := rtlCache[t.id]
	if ok {
		return value.rtl
	}
	log.Fatal("rtlCache miss")
	return nil
}

func HasKnownTypeArguments(t *Type, N int) bool {
	types := ResolvedTypeArguments(t)
	if len(types) == N {
		for i := range types {
			if (types[i].flags & TypeFlagsNumberLiteral) == 0 {
				return false
			}
		}
		return true
	}
	return false
}

func ResolvedTypeArguments(t *Type) []*Type {
	if t.alias != nil && t.alias.typeArguments != nil {
		return t.alias.typeArguments
	}
	d := t.AsTypeReference()
	return d.resolvedTypeArguments
}

func exprToString(node *ast.Node, c *Checker) string {
	var visit func(*ast.Node) bool
	p := c.newPrinter(TypeFormatFlagsNone)
	visit = func(node *ast.Node) bool {
		text, t, isDeclaration := p.c.getTextAndTypeOfNode(node)
		if text != "" && !strings.Contains(text, "\n") {
			p.print(">")
			p.print(text)
			p.print(" : ")
			p.printType(t)
			if isDeclaration && t.flags&TypeFlagsEnumLiteral != 0 && t.flags&(TypeFlagsStringLiteral|TypeFlagsNumberLiteral) != 0 {
				p.print(" = ")
				p.printValue(t.AsLiteralType().value)
			}
			p.print("\n")
		}
		return node.ForEachChild(visit)
	}
	visit(node)
	return p.string()
}

func isEqualLike(operator ast.Kind) bool {
	switch operator {
	case ast.KindEqualsEqualsToken, ast.KindExclamationEqualsToken, ast.KindEqualsEqualsEqualsToken, ast.KindExclamationEqualsEqualsToken:
		return true
	}
	return false
}

func isStringGroup(typ *Type) bool {
	if typ.flags&TypeFlagsStringLiteral != 0 {
		return true
	} else if typ.flags&TypeFlagsUnion != 0 {
		unionType := typ.AsUnionType()
		for _, t := range unionType.types {
			if t.flags&TypeFlagsStringLiteral == 0 {
				return false
			}
		}
		return true
	}
	return false
}
