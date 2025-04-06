package checker

import (
	"log"
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/jsnum"
)

func checkUnaryOpOverload(operator ast.Kind, exprType *Type) *Type {
	switch operator {
	case ast.KindPlusToken, ast.KindMinusToken, ast.KindTildeToken, ast.KindPlusPlusToken, ast.KindMinusMinusToken:
		if isBitType(exprType) && resolvedTypeArguments(exprType) != nil {
			return exprType
		} else if isRtlType(exprType) && resolvedTypeArguments(exprType) != nil {
			return exprType
		}
	case ast.KindExclamationToken:
		if (isBitType(exprType) || isRtlType(exprType)) && resolvedTypeArguments(exprType) != nil {
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
		if isBitType(leftType) && resolvedTypeArguments(leftType) != nil && isConstNumberExpression(right) {
			return leftType.checker.booleanType
		} else if isBitType(rightType) && resolvedTypeArguments(rightType) != nil && isConstNumberExpression(left) {
			return rightType.checker.booleanType
		}
		if isRtlType(leftType) && resolvedTypeArguments(leftType) != nil &&
			isRtlType(rightType) && resolvedTypeArguments(rightType) != nil {
			return makeRtlBase(leftType, leftType.checker.booleanType)
		}
		if isRtlType(leftType) && resolvedTypeArguments(leftType) != nil {
			if isConstNumberExpression(right) {
				return makeRtlBase(leftType, leftType.checker.booleanType)
			} else if isEqualLike(operator) && isStringGroup(resolvedTypeArguments(leftType)[0]) && isStringGroup(rightType) {
				return makeRtlBase(leftType, leftType.checker.booleanType)
			}
		}
		if isRtlType(rightType) && resolvedTypeArguments(rightType) != nil {
			if isConstNumberExpression(left) {
				return makeRtlBase(rightType, rightType.checker.booleanType)
			} else if isEqualLike(operator) && isStringGroup(leftType) && isStringGroup(resolvedTypeArguments(rightType)[0]) {
				return makeRtlBase(rightType, rightType.checker.booleanType)
			}
		}

	case ast.KindAmpersandAmpersandToken, ast.KindBarBarToken:
		if isBitType(leftType) && resolvedTypeArguments(leftType) != nil {
			return leftType.checker.booleanType
		} else if isBitType(rightType) && resolvedTypeArguments(rightType) != nil {
			return rightType.checker.booleanType
		}
		if isRtlType(leftType) && resolvedTypeArguments(leftType) != nil {
			return makeRtlBase(leftType, leftType.checker.booleanType)
		} else if isRtlType(rightType) && resolvedTypeArguments(rightType) != nil {
			return makeRtlBase(rightType, rightType.checker.booleanType)
		}

	case ast.KindLessThanLessThanToken, ast.KindGreaterThanGreaterThanToken:
		if isBitType(leftType) && resolvedTypeArguments(leftType) != nil &&
			isBitType(rightType) && resolvedTypeArguments(rightType) != nil {
			return leftType
		}
		if isBitType(leftType) && resolvedTypeArguments(leftType) != nil && isConstNumberExpression(right) {
			return leftType
		}
		if isRtlType(leftType) && resolvedTypeArguments(leftType) != nil &&
			isRtlType(rightType) && resolvedTypeArguments(rightType) != nil {
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
		if isBitType(leftType) && resolvedTypeArguments(leftType) != nil {
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
	if isBitType(leftType) && resolvedTypeArguments(leftType) != nil &&
		isBitType(rightType) && resolvedTypeArguments(rightType) != nil {
		w1 := getNumberLiteralValue(resolvedTypeArguments(leftType)[0])
		w2 := getNumberLiteralValue(resolvedTypeArguments(rightType)[0])
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
	} else if isBitType(leftType) && resolvedTypeArguments(leftType) != nil && isConstNumberExpression(right) {
		if carry {
			w1 := getNumberLiteralValue(resolvedTypeArguments(leftType)[0])
			return makeBinaryResultType(leftType, rightType, w1)
		}
		return leftType
	} else if isBitType(rightType) && resolvedTypeArguments(rightType) != nil && isConstNumberExpression(left) {
		if carry {
			w1 := getNumberLiteralValue(resolvedTypeArguments(rightType)[0])
			return makeBinaryResultType(leftType, rightType, w1)
		}
		return rightType
	}

	if isRtlType(leftType) && resolvedTypeArguments(leftType) != nil &&
		isRtlType(rightType) && resolvedTypeArguments(rightType) != nil {
		arg1 := resolvedTypeArguments(leftType)[0]
		arg2 := resolvedTypeArguments(rightType)[0]
		if isBitType(arg1) && resolvedTypeArguments(arg1) != nil &&
			isBitType(arg2) && resolvedTypeArguments(arg2) != nil {
			w1 := getNumberLiteralValue(resolvedTypeArguments(arg1)[0])
			w2 := getNumberLiteralValue(resolvedTypeArguments(arg2)[0])
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
	} else if isRtlType(leftType) && resolvedTypeArguments(leftType) != nil && isConstNumberExpression(right) {
		if carry {
			arg1 := resolvedTypeArguments(leftType)[0]
			if isBitType(arg1) && resolvedTypeArguments(arg1) != nil {
				w1 := getNumberLiteralValue(resolvedTypeArguments(arg1)[0])
				makeRtlBase(leftType, makeBinaryResultType(arg1, arg1, w1+jsnum.Number(1)))
			}
		} else {
			return toRtlBase(leftType)
		}
	} else if isRtlType(rightType) && resolvedTypeArguments(rightType) != nil && isConstNumberExpression(left) {
		if carry {
			arg2 := resolvedTypeArguments(rightType)[0]
			if isBitType(arg2) && resolvedTypeArguments(arg2) != nil {
				w1 := getNumberLiteralValue(resolvedTypeArguments(arg2)[0])
				makeRtlBase(rightType, makeBinaryResultType(arg2, arg2, w1+jsnum.Number(1)))
			}
		} else {
			return toRtlBase(rightType)
		}
	}
	return nil
}

func extendedMultiplyOverload(leftType *Type, left *ast.Node, rightType *Type, right *ast.Node) *Type {
	if isBitType(leftType) && resolvedTypeArguments(leftType) != nil &&
		isBitType(rightType) && resolvedTypeArguments(rightType) != nil {
		w1 := getNumberLiteralValue(resolvedTypeArguments(leftType)[0])
		w2 := getNumberLiteralValue(resolvedTypeArguments(rightType)[0])
		rw := w1 + w2
		return makeBinaryResultType(leftType, rightType, rw)
	}

	if isRtlType(leftType) && resolvedTypeArguments(leftType) != nil &&
		isRtlType(rightType) && resolvedTypeArguments(rightType) != nil {
		arg1 := resolvedTypeArguments(leftType)[0]
		arg2 := resolvedTypeArguments(rightType)[0]
		if isBitType(arg1) && resolvedTypeArguments(arg1) != nil &&
			isBitType(arg2) && resolvedTypeArguments(arg2) != nil {
			w1 := getNumberLiteralValue(resolvedTypeArguments(arg1)[0])
			w2 := getNumberLiteralValue(resolvedTypeArguments(arg2)[0])
			rw := w1 + w2
			return makeBinaryResultType(leftType, rightType, rw)
		}
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
	if isBitType(target) && resolvedTypeArguments(target) != nil {
		if isConstNumberExpression(initializer) {
			return true
		}
		okay := assignableToBitType(initType, target)
		if !okay {
			log.Println("checkInitializerOverload fail", target.checker.TypeToString(target), initType.checker.TypeToString(initType), exprToString(initializer, target.checker))
		}
		return okay
	} else if isRtlType(target) && resolvedTypeArguments(target) != nil {
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
	} else if resolvedTypeArguments(target) != nil {
		if isBitType(source) && resolvedTypeArguments(source) != nil {
			s1 := isSignedBitType(target)
			s2 := isSignedBitType(source)
			w1 := resolvedTypeArguments(target)[0].AsLiteralType().value
			w2 := resolvedTypeArguments(source)[0].AsLiteralType().value
			return s1 == s2 && w1 == w2
		} else if source.flags&TypeFlagsBooleanLike != 0 {
			w := resolvedTypeArguments(target)[0].AsLiteralType().value
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
	} else if resolvedTypeArguments(target) != nil {
		tgtArg := resolvedTypeArguments(target)[0]
		if isRtlType(source) && resolvedTypeArguments(source) != nil {
			srcArg := resolvedTypeArguments(source)[0]
			if isBitType(srcArg) && resolvedTypeArguments(srcArg) != nil {
				if isBitType(tgtArg) && resolvedTypeArguments(tgtArg) != nil {
					return assignableToBitType(srcArg, tgtArg)
				}
			} else if srcArg.flags&TypeFlagsBooleanLike != 0 {
				if isBitType(tgtArg) && resolvedTypeArguments(tgtArg) != nil {
					return assignableToBitType(srcArg, tgtArg)
				}
			}
		} else if isBitType(source) && resolvedTypeArguments(source) != nil {
			if isBitType(tgtArg) && resolvedTypeArguments(tgtArg) != nil {
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
	typeArgs := resolvedTypeArguments(t)
	if typeArgs != nil && len(typeArgs) > 0 {
		return makeRtlBase(t, typeArgs[0])
	}
	log.Fatal("Internal error: toRtlBase")
	return nil
}

func isBitType(t *Type) bool {
	if t.objectFlags&ObjectFlagsReference != 0 {
		data := t.AsTypeReference()
		return data.symbol != nil && (data.symbol.Name == "UInt" || data.symbol.Name == "Int")
	}
	return false
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

func isRtlType(t *Type) bool {
	_, ok := rtlCache[t.id]
	if ok && rtlCache[t.id].t == t {
		return true
	}
	var check func(*Type, *Type, int) bool
	check = func(t *Type, query *Type, depth int) bool {
		if depth > 10 {
			return false // run-away recursion
		}
		if depth > 0 && t == query {
			return false
		}
		if t.objectFlags&ObjectFlagsReference != 0 {
			data := t.AsTypeReference()
			if data.symbol.Name == "RtlExpr" {
				rtlCache[query.id] = rtl_table{query, data}
				return true
			}
		}
		if t.objectFlags&(ObjectFlagsClassOrInterface|ObjectFlagsReference) != 0 {
			target := getTargetType(t)
			return target != nil && t.checker != nil && core.Some(t.checker.getBaseTypes(target), func(t *Type) bool {
				return check(t, query, depth+1)
			})
		}
		if t.flags&TypeFlagsIntersection != 0 {
			return core.Some(t.AsIntersectionType().types, func(t *Type) bool {
				return check(t, query, depth+1)
			})
		}
		return false
	}
	return (t.objectFlags&ObjectFlagsReference) != 0 && check(t, t, 0)
}

func rtlBaseType(t *Type) *TypeReference {
	value, ok := rtlCache[t.id]
	if ok {
		return value.rtl
	}
	log.Fatal("rtlCache miss")
	return nil
}

func resolvedTypeArguments(t *Type) []*Type {
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
