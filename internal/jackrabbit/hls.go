package jackrabbit

import (
	"fmt"
	"regexp"
	"runtime"
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/jsnum"
)

var castFunctionRe = regexp.MustCompile("^u?int[0-9]+$")

var cppTokenOp = map[ast.Kind]string{
	ast.KindLessThanToken:            "<",
	ast.KindGreaterThanToken:         ">",
	ast.KindLessThanEqualsToken:      "<=",
	ast.KindGreaterThanEqualsToken:   ">=",
	ast.KindEqualsEqualsToken:        "==",
	ast.KindExclamationEqualsToken:   "!=",
	ast.KindEqualsEqualsEqualsToken:    "==",
	ast.KindExclamationEqualsEqualsToken: "!=",
	ast.KindEqualsGreaterThanToken:   "=>",
	ast.KindPlusToken:                "+",
	ast.KindMinusToken:               "-",
	ast.KindAsteriskToken:            "*",
	ast.KindHashPlusToken:            "+",
	ast.KindHashMinusToken:           "-",
	ast.KindHashAsteriskToken:        "*",
	ast.KindSlashToken:               "/",
	ast.KindPercentToken:             "%",
	ast.KindLessThanLessThanToken:    "<<",
	ast.KindGreaterThanGreaterThanToken: ">>",
	ast.KindAmpersandToken:           "&",
	ast.KindBarToken:                 "|",
	ast.KindCaretToken:               "^",
	ast.KindExclamationToken:         "!",
	ast.KindTildeToken:               "~",
	ast.KindAmpersandAmpersandToken:  "&&",
	ast.KindBarBarToken:              "||",
}

type HlsProcGen struct {
	AsModule        bool
	Descriptor      *FunctionDescriptor
	Checker         *checker.Checker
	MemoryInit      map[string][]string
	Symtab          *Namer
	Decl            *ast.FunctionDeclaration
	Root            *XMLBuilder
	LastVar         *XMLBuilder
	ReturnDesc      *checker.TypeDescriptor
	StatementContext []*XMLBuilder
	FoldExpr map[ast.NodeId]bool
}

func NewHlsProcGen(
	node *ast.FunctionDeclaration,
	descriptor *FunctionDescriptor,
	tc *checker.Checker,
	asModule bool,
) *HlsProcGen {
	proc := &HlsProcGen{
		AsModule:        asModule,
		Decl:            node,
		Descriptor:      descriptor,
		Checker:         tc,
		Symtab:          NewNamer(),
		MemoryInit:      make(map[string][]string),
		StatementContext: []*XMLBuilder{},
		FoldExpr: make(map[ast.NodeId]bool),
	}

	attrs := map[string]string{
		"name": node.Name().Text(),
	}

	if asModule {
		attrs["spec"] = "_1"
		attrs["type"] = "void"
	} else {
		rt := checker.GetDeclarationReturnType(tc, node)
		ty := descriptor.GetReturnType(rt, tc)
		if ty != nil {
			desc := checker.RequireTypeDescriptor(tc, ty, nil)
			attrs["type"] = ternary(desc.IsSigned, "int", "unsigned")
			attrs["width"] = uint32Str(desc.Width)
			proc.ReturnDesc = desc
		} else {
			attrs["type"] = "void"
		}
	}

	if asModule {
		proc.Root = createXml().Ele("proc", attrs)
	} else {
		proc.Root = createXml().Ele("func", attrs)
	}

	return proc
}

func (p *HlsProcGen) Generate(xic *XicGenerator) *XMLBuilder {
	procName := p.Decl.Name().Text()
	var xsig *XMLBuilder

	if p.AsModule {
		xsig = xic.config.Ele("signature", map[string]string{
			"class": procName,
			"spec":  "_1",
		})
	} else {
		xsig = xic.config.Ele("primitive", map[string]string{
			"name": procName,
		})
	}

	for _, param := range p.Decl.Parameters.Nodes {
		name := param.Name().Text()
		t := p.Checker.GetTypeAtLocation(param)

		if p.AsModule {
			if checker.IsTypeReferenceOf(t, "StreamIn") || checker.IsTypeReferenceOf(t, "StreamOut") {
				role := "writer"
				if checker.IsTypeReferenceOf(t, "StreamIn") {
					role = "reader"
				}

				info := checker.RequireTypeDescriptor(p.Checker, checker.ResolvedTypeArguments(t)[0], nil)
				_, spec := xic.connectors.Specialize("HlsStream", []any{getTypeSpec(info)})

				p.Root.Ele("io", map[string]string{
					"name":  name,
					"class": "HlsStream",
					"role":  role,
					"spec":  spec,
				})
				xsig.Ele("io", map[string]string{
					"name":  name,
					"class": "HlsStream",
					"role":  role,
					"spec":  spec,
				})
			} else {
				panic("unsupported parameter type for module")
			}
		} else {
			reference := p.Descriptor.IsReferenceParameter(name, t)

			attrs := map[string]string{
				"name": name,
				"mode": func() string {
					if reference {
						return "inout"
					}
					return "in"
				}(),
			}

			desc := checker.RequireTypeDescriptor(p.Checker, t, nil)
			for k, v := range genVarType(desc) {
				attrs[k] = v
			}

			p.Root.Ele("variable", attrs)

			xsig.Ele("io", map[string]string{
				"name":  name,
				"type":  func() string { if reference { return "pointer" } else { return "input" } }(),
				"ctype": func() string { if desc.IsSigned { return "int" } else { return "unsigned" } }(),
				"width": uint32Str(desc.Width),
			})
		}
	}

	if !p.AsModule && p.ReturnDesc != nil {
		xsig.Ele("io", map[string]string{
			"name":  "r_e_t_u_r_n",
			"type":  "return",
			"ctype": func() string { if p.ReturnDesc.IsSigned { return "int" } else { return "unsigned" } }(),
			"width": uint32Str(p.ReturnDesc.Width),
		})
	}

	if p.AsModule {
		xic.config.Ele("process", map[string]string{
			"name":  procName + "_",
			"class": procName,
			"spec":  "_1",
			"type":  "hp",
			"loc":   "fpga0",
		})
	}

	if p.Decl.Body != nil {
		p.emitBlockVariables(p.Decl.Body.AsBlock(), p.Root)
	}

	for name, values := range p.MemoryInit {
		p.Root.Ele("init", map[string]string{
			"name": name,
		}).Dat(strings.Join(values, " "))
	}

	if p.Descriptor.Inlined {
		p.Root.Ele("option", map[string]string{
			"name":  "inline",
			"value": "1",
		})
	}

	p.Root.Ele("option", map[string]string{
		"name":  "defaultDelay",
		"value": "32",
	})

	if p.Decl.Body != nil {
		setNodeLoc(p.Decl.Body, p.Root)
		p.emitBlock(p.Decl.Body.AsBlock(), p.Root)
	}

	return p.Root
}

func (gen *HlsProcGen) emitStatement(node *ast.Node, xout *XMLBuilder) {
	switch node.Kind {
	case ast.KindVariableStatement:
		stmt := node.AsVariableStatement()
		gen.emitVariableDeclarationList(stmt.DeclarationList.AsVariableDeclarationList(), xout)

	case ast.KindExpressionStatement:
		stmt := node.AsExpressionStatement()
		gen.markEmitExpression(stmt.Expression, xout)

	case ast.KindForStatement:
		stmt := node.AsForStatement()
		if stmt.Initializer != nil {
			switch stmt.Initializer.Kind {
			case ast.KindVariableDeclarationList:
				init := stmt.Initializer.AsVariableDeclarationList()
				gen.emitVariableDeclarationList(init, xout)
			default:
				gen.markEmitExpression(stmt.Initializer, xout)
			}
		}

		xloop := xout.Ele("loop", nil)
		setNodeLoc(node, xloop)

		xcond := xloop.Ele("cond", nil)
		xcond.Ele("true", nil).Ele("const", map[string]string{
			"value":  "1",
			"width":  "1",
			"signed": "false",
		})

		xbody := xloop.Ele("body", nil)

		xbody.Ele("option", map[string]string{
			"name":  "stageDelay",
			"value": "32",
		})

		if stmt.Condition != nil {
			breakCond := gen.buildCondition(stmt.Condition, true, xbody)
			xbody.Ele("break", map[string]string{
				"cond": breakCond,
			})
		}

		gen.emitStatement(stmt.Statement, xbody)

		if stmt.Incrementor != nil {
			gen.markEmitExpression(stmt.Incrementor, xbody)
		}

	case ast.KindForInStatement, ast.KindForOfStatement:
		panic("ForIn/ForOf statements are not supported")

	case ast.KindWhileStatement:
		stmt := node.AsWhileStatement()
		xloop := xout.Ele("loop", nil)
		setNodeLoc(node, xloop)

		xcond := xloop.Ele("cond", nil)
		xbody := xloop.Ele("body", nil)

		xbody.Ele("option", map[string]string{
			"name":  "stageDelay",
			"value": "32",
		})

		breakCond := gen.buildCondition(stmt.Expression, true, xbody)

		if breakCond != "" {
			xcond.Ele("true", nil).Ele("const", map[string]string{
				"value":  "1",
				"width":  "1",
				"signed": "false",
			})
			xbody.Ele("break", map[string]string{
				"cond": breakCond,
			})
		} else {
			// Early return condition
			xloop.Delete()
			return
		}

		gen.emitStatement(stmt.Statement, xbody)

	case ast.KindIfStatement:
		stmt := node.AsIfStatement()
		xif := xout.Ele("if", nil)
		setNodeLoc(node, xif)

		gen.emitFlattenExpr("c_o_n_d", stmt.Expression, gen.Checker.GetTypeAtLocation(stmt.Expression), xif)

		xthen := xif.Ele("then", nil)
		setNodeLoc(stmt.ThenStatement, xthen)
		gen.emitStatement(stmt.ThenStatement, xthen)

		if stmt.ElseStatement != nil {
			xelse := xif.Ele("else", nil)
			setNodeLoc(stmt.ElseStatement, xelse)
			gen.emitStatement(stmt.ElseStatement, xelse)	
		}

	case ast.KindReturnStatement:
		stmt := node.AsReturnStatement()
		xret := xout.Ele("return", nil)
		setNodeLoc(node, xret)

		if stmt.Expression != nil {
			ret := gen.Descriptor.GetReturnExpression(stmt.Expression)
			if ret != nil {
				gen.emitExpression(ret, nil, xret)
			}
		}

	case ast.KindBlock:
		block := node.AsBlock()
		gen.emitBlock(block, xout)
	}
}

func (gen *HlsProcGen) emitBlock(block *ast.Block, xout *XMLBuilder) {
	for _, stmt := range block.Statements.Nodes {
		gen.emitStatement(stmt, xout)
	}
}

func (gen *HlsProcGen) emitVariableDeclarationList(lst *ast.VariableDeclarationList, xout *XMLBuilder) {
	for _, decl := range lst.Declarations.Nodes {
		if decl.Initializer() != nil {
			typ := gen.Checker.GetTypeAtLocation(decl)
			if !checker.IsArrayType(gen.Checker, typ) {
				sym := decl.Symbol()
				assign := xout.Ele("assign", map[string]string{
					"name": gen.Symtab.GetName(sym),
				})
				setNodeLoc(lst.Parent, assign)
				gen.emitExpression(decl.Initializer(), typ, assign)
			}
		}
	}
}

func (gen *HlsProcGen) markEmitExpression(expr *ast.Node, xout *XMLBuilder) {
	clear(gen.FoldExpr)
	fold := gen.markExpression(expr)
	if fold == 0 {
		gen.emitExpression(expr, nil, xout)
		} else {
		gen.foldExpression(expr, xout)
	}
}

func (gen *HlsProcGen) markExpression(expr *ast.Node) int {
	fold := 0

	switch expr.Kind {
	case ast.KindNumericLiteral:
		fold = 1

	case ast.KindIdentifier:
		_, value := checker.ResolveIdentifierExpr(gen.Checker, expr)
		if value != nil {
			fold = 1
		}

	case ast.KindPrefixUnaryExpression:
			unary := expr.AsPrefixUnaryExpression()
			if unary.Operator == ast.KindPlusToken || unary.Operator == ast.KindMinusToken {
				a := gen.markExpression(unary.Operand)
				if a > 0 {
					fold = a + 1
				}
			}

	case ast.KindBinaryExpression:
		binExpr := expr.AsBinaryExpression()
		a := gen.markExpression(binExpr.Left)
		b := gen.markExpression(binExpr.Right)
		if !isUpdateOp(binExpr.OperatorToken.Kind) {
			if a != 0 && b != 0 {
				if a >= b {
					fold = a + 1
				} else {
					fold = b + 1
				}
			} else if a > 1 {
				gen.markConstant(binExpr.Left)
			} else if b > 1 {
				gen.markConstant(binExpr.Right)
			}
		}

	case ast.KindParenthesizedExpression:
		paren := expr.AsParenthesizedExpression()
		fold = gen.markExpression(paren.Expression)
	}

	return fold
}

// We don't want to generate a node id for every node.  Here we reuse and existing
// flag and store it's original state so that it can be restored afterwards.
func (h *HlsProcGen) markConstant(expr *ast.Node) {
	hasConst := (expr.Flags & ast.NodeFlagsConst) != 0
	h.FoldExpr[ast.GetNodeId(expr)] = hasConst
	expr.Flags |= ast.NodeFlagsConst
}

func (h *HlsProcGen) foldExpression(expr *ast.Node, xout *XMLBuilder) {
	ty := h.Checker.GetTypeAtLocation(expr)
	tdesc := checker.RequireTypeDescriptor(h.Checker, ty, nil)
	value := checker.TsEvaluateExpr(expr, h.Checker)
	assert(value != nil)
	xout.Ele("const", mergeMaps(
		map[string]string{"value": fmt.Sprint(value)},
		genConstype(tdesc),
	))
}

func (h *HlsProcGen) emitExpression(expr *ast.Node, parentType *checker.Type, xout *XMLBuilder) {
	if (expr.Flags & ast.NodeFlagsConst) != 0 {
		if hadConst, ok := h.FoldExpr[ast.GetNodeId(expr)]; ok {
			if !hadConst {
				expr.Flags ^= ast.NodeFlagsConst
			}
			h.foldExpression(expr, xout)
			return
		}
	}

	switch expr.Kind {
	case ast.KindNumericLiteral:
		assert(parentType != nil)
		h.emitLiteral(expr, checker.RequireTypeDescriptor(h.Checker, parentType, nil), xout)
		return

	default:
		ty := h.Checker.GetTypeAtLocation(expr)

		switch expr.Kind {
		case ast.KindBinaryExpression:
			bin := expr.AsBinaryExpression()
			op := bin.OperatorToken.Kind
			left := bin.Left
			right := bin.Right

			if op == ast.KindEqualsToken {
				if left.Kind == ast.KindArrayLiteralExpression && right.Kind == ast.KindCallExpression {
					h.emitCallExpression(right.AsCallExpression(), left.AsArrayLiteralExpression(), xout)
					return
				}
				vt := h.Checker.GetTypeAtLocation(left)
				h.emitExpression(right, vt, h.emitAssignment(left, xout))
				return
			}

			if parentType != nil {
				xout = h.castIfNeeded(ty, parentType, xout)
			}

			if isRelationalOp(op) {
				h.emitRelationalOp(op, left, right, xout)
				return
			}

			if isUpdateOp(op) {
				xout = h.emitAssignment(left, xout)
				op = getUpdateOp(op)
			}

			tdesc := checker.RequireTypeDescriptor(h.Checker, ty, nil)
			cppOp, ok := cppTokenOp[op]
			assert(ok)

			xout = xout.Ele("binop", mergeMaps(
				map[string]string{"op": cppOp},
				genOpType(tdesc),
			))
			h.emitExpression(left, ty, xout)
			h.emitExpression(right, ty, xout)

		case ast.KindPostfixUnaryExpression:
			post := expr.AsPostfixUnaryExpression()
			xout = h.emitAssignment(post.Operand, xout)
			op := getIncrementOp(post.Operator)
			tdesc := checker.RequireTypeDescriptor(h.Checker, ty, nil)

			xout = xout.Ele("binop", mergeMaps(
				map[string]string{"op": cppTokenOp[op]},
				genOpType(tdesc),
			))
			h.emitExpression(post.Operand, ty, xout)
			xout.Ele("const", mergeMaps(
				map[string]string{"value": "1"},
				genConstype(tdesc),
			))

		case ast.KindIdentifier:
			sym, value := checker.ResolveIdentifierExpr(h.Checker, expr)
			if value == nil {
				if parentType != nil {
					xout = h.castIfNeeded(ty, parentType, xout)
				}
				name := h.Symtab.GetName(sym)
				xout.Ele("var", map[string]string{"name": name})
			} else {
				assert(parentType != nil)
				tdesc := checker.RequireTypeDescriptor(h.Checker, parentType, nil)
				sdesc := checker.RequireTypeDescriptor(h.Checker, ty, nil)
				valStr := fmt.Sprint(value)
				if canOverflow(sdesc, tdesc) {
					xout = castIfNeededDesc(sdesc, tdesc, xout)
					xout.Ele("const", mergeMaps(
						map[string]string{"value": valStr},
						genConstype(sdesc),
					))
				} else {
					xout.Ele("const", mergeMaps(
						map[string]string{"value": valStr},
						genConstype(tdesc),
					))
				}
			}
		case ast.KindElementAccessExpression:
			elem := expr.AsElementAccessExpression()
			tdesc := checker.RequireTypeDescriptor(h.Checker, ty, nil)
			aref := xout.Ele("arrayref", map[string]string{"tsize": uint32Str(tdesc.Width)})
			if elem.Expression.Kind == ast.KindIdentifier {
				sym := h.Checker.GetSymbolAtLocation(elem.Expression)
				assert(sym != nil)
				name := h.Symtab.GetName(sym)
				aref.Ele("const", map[string]string{"sym": name, "offset": "0"})
				h.emitFlattenExpr(name+"_a_d_r", elem.ArgumentExpression, ty, aref)
			} else {
				assert(false, "Expecting variable in array element access")
			}

		case ast.KindCallExpression:
			h.emitCallExpression(expr.AsCallExpression(), nil, xout)

		case ast.KindParenthesizedExpression:
			par := expr.AsParenthesizedExpression()
			h.emitExpression(par.Expression, parentType, xout)

		default:
			assert(false, "Unhandled expression kind: "+expr.Kind.String())
		}
	}
}

func (p *HlsProcGen) emitFlattenExpr(prefix string, expr *ast.Node, parentType *checker.Type, xout *XMLBuilder) {
	if expr.Kind != ast.KindIdentifier && expr.Kind != ast.KindNumericLiteral {
		ty := p.Checker.GetTypeAtLocation(expr)
		desc := checker.RequireTypeDescriptor(p.Checker, ty, expr)
		tmp := p.newVar(prefix, desc)

		ctx := xout
		cntr := ctx.Parent
		for cntr != nil {
			tag := cntr.TagName()
			if tag == "body" || tag == "then" || tag == "else" || tag == "func" || tag == "proc" {
				break
			}
			ctx = cntr
			cntr = cntr.Parent
		}

		xassign := createXml().Ele("assign", map[string]string{"name": tmp})
		p.emitExpression(expr, parentType, xassign)
		cntr.InsertBefore(xassign, ctx)
		xout.Ele("var", map[string]string{"name": tmp})
	} else {
		p.emitExpression(expr, parentType, xout)
	}
}

func (h *HlsProcGen) emitRelationalOp(op ast.Kind, left, right *ast.Expression, xout *XMLBuilder) {
	var isSigned bool
	var width uint32

	lt := h.Checker.GetTypeAtLocation(left)
	rt := h.Checker.GetTypeAtLocation(right)

	ldesc := checker.RequireTypeDescriptor(h.Checker, lt, left)
	rdesc := checker.RequireTypeDescriptor(h.Checker, rt, right)

	if ldesc.IsSigned == rdesc.IsSigned {
			isSigned = ldesc.IsSigned
			if ldesc.Width >= rdesc.Width {
					width = ldesc.Width
			} else {
					width = rdesc.Width
			}
	} else if ldesc.IsSigned {
			isSigned = true
			if ldesc.Width >= rdesc.Width {
					width = ldesc.Width
			} else {
					width = rdesc.Width + 1
			}
	} else {
			isSigned = true
			if ldesc.Width >= rdesc.Width {
					width = ldesc.Width + 1
			} else {
					width = rdesc.Width
			}
	}

	desc := checker.TypeDescriptor{
		IsSigned: isSigned,
		Width:    width,
		Shape:    []uint32{},
	}

	xml := xout.Ele("binop", mergeMaps(map[string]string{"op": cppTokenOp[op]}, genOpType(&desc)))

	h.emitExpressionForDesc(left, lt, &desc, xml)
	h.emitExpressionForDesc(right, rt, &desc, xml)
}

func (h *HlsProcGen) emitExpressionForDesc(expr *ast.Expression, exprT *checker.Type, desc *checker.TypeDescriptor, xout *XMLBuilder) {
	if ast.IsLiteralExpression(expr) {
			h.emitLiteral(expr, desc, xout)
			return
	}

	sdesc := checker.RequireTypeDescriptor(h.Checker, exprT, expr)

	if ast.IsIdentifier(expr) {
			_, value := checker.ResolveIdentifierExpr(h.Checker, expr)
			if value != nil {
					if canOverflow(sdesc, desc) {
							xout = castIfNeededDesc(sdesc, desc, xout)
							xout.Ele("const", mergeMaps(map[string]string{"value": valueString(value)}, genConstype(sdesc)))
					} else {
							xout.Ele("const", mergeMaps(map[string]string{"value": valueString(value)}, genConstype(desc)))
					}
					return
			}
	}

	xout = castIfNeededDesc(sdesc, desc, xout)
	h.emitExpression(expr, nil, xout)
}

func (h *HlsProcGen) emitCallExpression(expr *ast.CallExpression, lhs *ast.ArrayLiteralExpression, xout *XMLBuilder) {
	fun := expr.Expression

	if ast.IsIdentifier(fun) {
			ident := fun.AsIdentifier()
			name := ident.Text
			if castFunctionRe.MatchString(name) {
					arg := expr.Arguments.Nodes[0]
					at := h.Checker.GetTypeAtLocation(arg)
					sdesc := checker.RequireTypeDescriptor(h.Checker, at, nil)
					tdesc := parseTypeSpec(name)
					xml := castIfNeededDesc(sdesc, tdesc, xout)
					h.emitExpression(arg, at, xml)
					return
			}

			analysis := GetFunctionIdentDescriptor(ident, h.Checker)
			if analysis != nil {
					var rt *checker.Type

					if lhs != nil {
							newLhs := analysis.FilterCallerLhs(lhs.Elements.Nodes)
							if len(newLhs) > 1 {
									panic(fmt.Sprintf("Failed to reduce function return: %s", analysis.MethodName))
							} else if len(newLhs) == 1 {
									rt = h.Checker.GetTypeAtLocation(newLhs[0])
									xout = h.emitAssignment(newLhs[0], xout)
							}
					}

					attrs := map[string]string{"proc": ident.Text}
					argTypes := checker.GetDeclarationParameterTypes(h.Checker, analysis.Declaration)

					if rt != nil {
							rtDesc := checker.RequireTypeDescriptor(h.Checker, rt, nil)
							attrs["signed"] = boolStr(rtDesc.IsSigned)
							argTypes = append(argTypes, rt)
					}

					attrs["type"] = genFuncType(expr.Arguments.Nodes, argTypes, h.Checker)

					xml := xout.Ele("call", attrs)
					for idx, arg := range expr.Arguments.Nodes {
							h.emitExpression(arg, argTypes[idx], xml)
					}
			} else {
					panic(fmt.Sprintf("Undefined function: %s", ident.Text))
			}
	} else if ast.IsPropertyAccessExpression(fun) {
			propAcc := fun.AsPropertyAccessExpression()
			op := propAcc.Name().Text()
			if ast.IsIdentifier(propAcc.Expression) {
					obj := propAcc.Expression.AsIdentifier()
					name := obj.Text
					xml := xout.Ele("exec", map[string]string{"op": op})
					xml.Ele("var", map[string]string{"name": name})
					for _, arg := range expr.Arguments.Nodes {
							h.emitExpression(arg, nil, xml)
					}
			} else {
					panic("Object of property access is not an identifier")
			}
	} else {
			panic("Unsupported function type")
	}
}
func (h *HlsProcGen) emitLiteral(expr *ast.LiteralExpression, desc *checker.TypeDescriptor, xout *XMLBuilder) {
	xout.Ele("const", mergeMaps(map[string]string{
			"value": expr.Text(),
	}, genConstype(desc)))
}

func (h *HlsProcGen) emitAssignment(lhs *ast.Expression, xout *XMLBuilder) *XMLBuilder {
	switch lhs.Kind {
	case ast.KindIdentifier:
			sym := h.Checker.GetSymbolAtLocation(lhs)
			name := h.Symtab.GetName(sym)
			xa := xout.Ele("assign", map[string]string{
					"name": name,
			})
			setNodeLoc(lhs, xa)
			return xa
	case ast.KindElementAccessExpression:
			store := xout.Ele("store", nil)
			setNodeLoc(lhs, store)
			h.emitExpression(lhs, h.Checker.GetTypeAtLocation(lhs), store)
			return store
	default:
			panic("Unexpected LHS: " + lhs.Kind.String())
	}
}

func (h *HlsProcGen) emitStatementVariables(stmt *ast.Statement, xout *XMLBuilder) {
	switch stmt.Kind {
	case ast.KindVariableStatement:
			h.emitVariableDeclarationListVariables(stmt.AsVariableStatement().DeclarationList.AsVariableDeclarationList(), xout)
	case ast.KindForStatement:
			f := stmt.AsForStatement()
			if f.Initializer != nil {
					if ast.IsVariableDeclarationList(f.Initializer) {
							h.emitVariableDeclarationListVariables(f.Initializer.AsVariableDeclarationList(), xout)
					}
			}
			h.emitStatementVariables(f.Statement, xout)
	case ast.KindForInStatement, ast.KindForOfStatement:
			f := stmt.AsForInOrOfStatement()
			if f.Initializer != nil {
				if ast.IsVariableDeclarationList(f.Initializer) {
					h.emitVariableDeclarationListVariables(f.Initializer.AsVariableDeclarationList(), xout)
				}
			}
			h.emitStatementVariables(f.Statement, xout)
	case ast.KindIfStatement:
			h.emitStatementVariables(stmt.AsIfStatement().ThenStatement, xout)
			if stmt.AsIfStatement().ElseStatement != nil {
					h.emitStatementVariables(stmt.AsIfStatement().ElseStatement, xout)
			}
	case ast.KindBlock:
			h.emitBlockVariables(stmt.AsBlock(), xout)
	}
}

func (h *HlsProcGen) emitBlockVariables(block *ast.Block, xout *XMLBuilder) {
	for _, stmt := range block.Statements.Nodes {
			h.emitStatementVariables(stmt, xout)
	}
}

func (h *HlsProcGen) emitVariableDeclarationListVariables(lst *ast.VariableDeclarationList, xout *XMLBuilder) {
	for _, decl := range lst.Declarations.Nodes {
			name := h.Symtab.GenName(decl.AsVariableDeclaration())
			t := h.Checker.GetTypeAtLocation(decl)
			info := checker.RequireTypeDescriptor(h.Checker, t, decl.Initializer())
			tag := "variable"
			if checker.IsArrayType(h.Checker, t) {
					tag = "memory"
			}
			h.LastVar = xout.Ele(tag, mergeMaps(map[string]string{
					"name": name,
			}, genVarType(info)))
			if tag == "memory" && decl.Initializer() != nil && !checker.IsArrayConstructor(decl.Initializer()) {
					data := []string{}
					extractArrayData(decl.Initializer(), h.Checker, data)
					h.MemoryInit[name] = data
			}
	}
}

func (h *HlsProcGen) buildCondition(expr *ast.Node, inverted bool, xout *XMLBuilder) string {
	negate := inverted
	prefix := ""
	if negate {
		prefix = "!"
	}

	if expr.Kind == ast.KindPrefixUnaryExpression {
		prefixUnary := expr.AsPrefixUnaryExpression()
		if prefixUnary.Operator == ast.KindExclamationToken {
			negate = !negate
			if negate {
				prefix = "!"
			} else {
				prefix = ""
			}
			expr = prefixUnary.Operand
		}
	}

	if expr.Kind == ast.KindIdentifier {
		sym := h.Checker.GetSymbolAtLocation(expr)
		return prefix + h.Symtab.GetName(sym)
	} else if checker.IsConstExpression(expr) {
		cond := checker.GetConstExpression(expr)
		if negate {
			if b, ok := cond.(bool); ok {
				return valueString(!b)
			} else {
				return prefix + valueString(cond)
			}
		}
		return valueString(cond)
	}

	tmp := h.newVar("c_o_n_d", &checker.TypeDescriptor{ IsSigned: false, Width: 1 })
	assign := xout.Ele("assign", map[string]string{"name": tmp})
	h.markEmitExpression(expr, assign)
	return prefix + tmp
}

func (h *HlsProcGen) castIfNeeded(src *checker.Type, dst *checker.Type, xout *XMLBuilder) *XMLBuilder {
	si := checker.RequireTypeDescriptor(h.Checker, src, nil)
	di := checker.RequireTypeDescriptor(h.Checker, dst, nil)
	return castIfNeededDesc(si, di, xout)
}

func (h *HlsProcGen) newVar(basename string, info *checker.TypeDescriptor) string {
	name := h.Symtab.GenNewName(basename)
	xml := createXml().Ele("variable", mergeMaps(map[string]string{
			"name": name,
	}, genVarType(info)))
	if h.LastVar != nil {
			h.Root.InsertAfter(xml, h.LastVar)
	} else {
		h.Root.Append(xml)
	}
	h.LastVar = xml
	return name
}

func canOverflow(si, di *checker.TypeDescriptor) bool {
	if si.IsSigned == di.IsSigned {
			return di.Width < si.Width
	} else if si.IsSigned {
			return di.Width < si.Width
	}
	// casting unsigned to signed
	return di.Width <= si.Width
}

func castIfNeededDesc(si, di *checker.TypeDescriptor, xout *XMLBuilder) *XMLBuilder {
	if si.IsSigned != di.IsSigned || si.Width != di.Width {
			return xout.Ele("cast", map[string]string{
					"float": "0", 
					"fromfloat": "0", 
					"signed": fmt.Sprintf("%v", si.IsSigned),
					"tosigned": fmt.Sprintf("%v", di.IsSigned),
					"width": fmt.Sprintf("%d", di.Width),
			})
	}
	return xout
}

func extractArrayData(expr *ast.Node, c *checker.Checker, data []string) {
	if expr.Kind == ast.KindArrayLiteralExpression {
			arrayExpr := expr.AsArrayLiteralExpression()  // Assume AsArrayLiteralExpression is a helper
			if len(arrayExpr.Elements.Nodes) > 0 {
					firstElem := arrayExpr.Elements.Nodes[0]
					if firstElem.Kind == ast.KindArrayLiteralExpression {
							for _, a := range arrayExpr.Elements.Nodes {
									extractArrayData(a, c, data)
							}
					} else {
							for _, a := range arrayExpr.Elements.Nodes {
									v := checker.TsEvaluateExpr(a, c)
									if v == nil {
											fmt.Println(a)
											assert(false, "Non-constant array initializer")
									}
									data = append(data, valueString(v))
							}
					}
			}
	} else {
			assert(false, "Unexpected array initializer")
	}
}

func valueString(value any) string {
	if num, ok := value.(jsnum.Number); ok {
		x := float64(num)
		return fmt.Sprintf("%f", x)
	} else if b, ok := value.(bool); ok {
		if b {
			return "true"
		} else {
			return "false"
		}
	} else {
		panic("Unknown value in valueString")
	}
}

func setNodeLoc(node *ast.Node, xout *XMLBuilder) {
	xout.Att("srcpos", intStr(node.Loc.Pos()))
}

func genFuncType(actuals []*ast.Node, argTypes []*checker.Type, tc *checker.Checker) string {
	spec := make([]string, len(argTypes))
	for idx, t := range argTypes {
		desc := checker.RequireTypeDescriptor(tc, t, actuals[idx])
		if desc.IsSigned {
			spec[idx] = fmt.Sprintf("i%d", desc.Width)
		} else {
			spec[idx] = fmt.Sprintf("u%d", desc.Width)
		}
	}
	return strings.Join(spec, "_")
}

func genOpType(desc *checker.TypeDescriptor) map[string]string {
	return map[string]string{
		"signed":   boolStr(desc.IsSigned),
		"float":    "0",
	}
}

func genConstype(desc *checker.TypeDescriptor) map[string]string {
	return map[string]string{
		"signed":   boolStr(desc.IsSigned),
		"width":    uint32Str(desc.Width),
	}
}

func genVarType(desc *checker.TypeDescriptor) map[string]string {
	obj := map[string]string{
		"kind":     "int",
		"signed":   boolStr(desc.IsSigned),
		"width":    uint32Str(desc.Width),
	}

	if desc.Shape != nil {
		var depth uint32 = 1
		for _, dim := range desc.Shape {
			depth *= dim
		}
		obj["depth"] = uint32Str(depth)
		obj["bounds"] = uint32Str(depth)
		delete(obj, "kind")
	}

	return obj
}

func getTypeSpec(desc *checker.TypeDescriptor) string {
	if desc.IsSigned {
		return fmt.Sprintf("int%d", desc.Width)
	}
	return fmt.Sprintf("uint%d", desc.Width)
}

func isRelationalOp(kind ast.Kind) bool {
	switch kind {
	case ast.KindLessThanToken,
		ast.KindGreaterThanToken,
		ast.KindLessThanEqualsToken,
		ast.KindGreaterThanEqualsToken,
		ast.KindEqualsEqualsToken,
		ast.KindExclamationEqualsToken,
		ast.KindEqualsEqualsEqualsToken,
		ast.KindExclamationEqualsEqualsToken,
		ast.KindEqualsGreaterThanToken:
		return true
	default:
		return false
	}
}

func isUpdateOp(kind ast.Kind) bool {
	switch kind {
	case ast.KindPlusEqualsToken,
		ast.KindMinusEqualsToken,
		ast.KindAsteriskEqualsToken,
		ast.KindSlashEqualsToken,
		ast.KindBarEqualsToken,
		ast.KindCaretEqualsToken,
		ast.KindAmpersandEqualsToken:
		return true
	default:
		return false
	}
}

func getIncrementOp(op ast.Kind) ast.Kind {
	switch op {
	case ast.KindPlusPlusToken:
		return ast.KindPlusToken
	case ast.KindMinusMinusToken:
		return ast.KindMinusToken
	default:
		panic("invalid increment operator")
	}
}

func getUpdateOp(kind ast.Kind) ast.Kind {
	switch kind {
	case ast.KindPlusEqualsToken:
		return ast.KindPlusToken
	case ast.KindMinusEqualsToken:
		return ast.KindMinusToken
	case ast.KindAsteriskEqualsToken:
		return ast.KindAsteriskToken
	case ast.KindSlashEqualsToken:
		return ast.KindSlashToken
	case ast.KindBarEqualsToken:
		return ast.KindBarToken
	case ast.KindCaretEqualsToken:
		return ast.KindCaretToken
	case ast.KindAmpersandEqualsToken:
		return ast.KindAmpersandToken
	default:
		panic("invalid update operator")
	}
}

func mergeMaps(m1 map[string]string, m2 map[string]string) map[string]string {
	merged := make(map[string]string)

	for key, value := range m1 {
		merged[key] = value
	}

	for key, value := range m2 {
		merged[key] = value
	}

	return merged
}

func ternary(cond bool, a string, b string) string {
	if cond {
		return a
	} else {
		return b
	}
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
