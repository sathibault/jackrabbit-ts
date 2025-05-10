package checker

import (
	"fmt"
	"os"
)

func (c *Checker) equivalentMathTypes(ty1 *Type, ty2 *Type) bool {
	data1 := ty1.AsMathLiteralType()
	data2 := ty2.AsMathLiteralType()
	equiv := c.equivalenMathTerms(data1.term, data2.term)
	if !equiv {
		fmt.Fprintln(os.Stderr, "equivalentMathTypes fail", c.TypeToString(ty1), c.TypeToString(ty2))
	}
	return equiv
}

func (c *Checker) equivalenMathTerms(term1 MathTypeTerm, term2 MathTypeTerm) bool {
	if term1.Kind() != term2.Kind() {
		return false
	}
	switch t1 := term1.(type) {
	case IntrinsicMathTerm:
		t2 := term2.(IntrinsicMathTerm)
		if t1.intrinsic != t2.intrinsic {
			return false
		}
		return c.isTypeIdenticalTo(t1.argument, t2.argument)
	case BinaryMathTerm:
		t2 := term2.(BinaryMathTerm)
		if t1.operator != t2.operator {
			return false
		}
		return c.isTypeIdenticalTo(t1.leftType, t2.leftType) && c.isTypeIdenticalTo(t1.rightType, t2.rightType)
	default:
		panic("Unhandled case in equivalenMathTerms")
	}
}
