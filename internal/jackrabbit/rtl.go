package jackrabbit

import (
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/jsnum"
)

type RtlGenerator struct {
	den     *RabbitDen
	tc      *checker.Checker
	outFile *os.File
}

func NewRtlGenerator(den *RabbitDen, sourceFile *ast.SourceFile, tc *checker.Checker) *RtlGenerator {
	base := filepath.Base(sourceFile.FileName())
	stem := strings.TrimSuffix(base, filepath.Ext(base))
	outPath := stem + ".sv"
	fmt.Println("Generate", outPath, "...")

	file, err := os.Create(outPath)
	if err != nil {
		panic(err)
	}

	return &RtlGenerator{
		den:     den,
		tc:      tc,
		outFile: file,
	}
}

func (r *RtlGenerator) getDescriptor(desc *FunctionDescriptor, call *ast.CallExpression) *RtlModDescriptor {
	if desc.rtlDescriptor != nil {
		return desc.rtlDescriptor
	}

	decl := desc.declaration
	isGeneric := len(decl.TypeParameters()) > 0
	name := decl.Name().Text()

	var sig []*checker.Type
	var rt *checker.Type
	if isGeneric {
		sig, rt = checker.GetResolvedParameterTypes(r.tc, call.AsNode())
	} else {
		sig, rt = checker.GetDeclarationParameterTypes(r.tc, desc.declaration)
	}

	isHls := desc.moduleType != nil && *desc.moduleType == "hls"

	var callParams []*RtlIo
	for i, param := range decl.Parameters() {
		pname := param.Name().Text()
		if isHls {
			pname = "p_" + pname
		}
		var typ *checker.Type
		if isGeneric && call != nil {
			typ = sig[i]
		} else {
			typ = r.tc.GetTypeAtLocation(param)
		}
		callParams = append(callParams, &RtlIo{
			name: pname,
			Type: buildRtlIo(Client, typ, param, isHls, r.tc),
		})
	}

	var returnParams []*RtlIo
	prefix := strings.ToLower(name)
	if checker.IsTypeReference(rt) && checker.IsTupleType(r.tc, rt) {
		elems := checker.ResolvedTypeArguments(rt)
		for i, el := range elems {
			returnParams = append(returnParams, &RtlIo{
				name: fmt.Sprintf("%s_%d", prefix, i),
				Type: buildRtlIo(Provider, el, decl, isHls, r.tc),
			})
		}
	} else if (rt.Flags() & checker.TypeFlagsVoid) == 0 {
		returnParams = append(returnParams, &RtlIo{
			name: fmt.Sprintf("%s_0", prefix),
			Type: buildRtlIo(Provider, rt, decl, isHls, r.tc),
		})
	}

	modDesc := &RtlModDescriptor{
		name:         name,
		callParams:   callParams,
		returnParams: returnParams,
	}

	if !isGeneric {
		desc.rtlDescriptor = modDesc
	}

	return modDesc
}

func (r *RtlGenerator) Generate(decl *ast.FunctionDeclaration, desc *FunctionDescriptor) {
	proc := NewRtlProcGen(r.den, decl, desc, r)
	proc.Generate()
}

type RtlProcGen struct {
	den        *RabbitDen
	decl       *ast.FunctionDeclaration
	desc       *FunctionDescriptor
	gen        *RtlGenerator
	tc         *checker.Checker
	outFile    *os.File
	symtab     *Namer
	signals    map[string]*SignalInfo
	interfaces map[string]RtlIoType
	bundles    map[string]map[string]RtlIoType
	orderMax   int
}

func NewRtlProcGen(den *RabbitDen, decl *ast.FunctionDeclaration, desc *FunctionDescriptor, gen *RtlGenerator) *RtlProcGen {
	return &RtlProcGen{
		den:        den,
		decl:       decl,
		desc:       desc,
		gen:        gen,
		tc:         gen.tc,
		outFile:    gen.outFile,
		symtab:     NewNamer(),
		signals:    map[string]*SignalInfo{},
		interfaces: map[string]RtlIoType{},
		bundles:    map[string]map[string]RtlIoType{},
	}
}

func (r *RtlProcGen) defineSignal(name string, info *SignalInfo) {
	_, ok := r.signals[name]
	assert(!ok, "Multiple definitions of ", name)
	r.signals[name] = info
}

func (r *RtlProcGen) addDriver(sig *SignalInfo, d *Driver) {
	if len(sig.drivers) == 0 {
		r.orderMax += 1
		sig.order = r.orderMax
	}
	sig.drivers = append(sig.drivers, d)
}

func (r *RtlProcGen) addStateDriver(name string, state []string, expr *ast.Expression) {
	sig, ok := r.signals[name]
	assert(ok, "Undesigned signal name:", name)
	r.addDriver(sig, &Driver{
		expr:    expr,
		state:   state,
		delayed: false,
	})
}

func (r *RtlProcGen) genExpression(expr *ast.Expression) string {
	return r.buildExpression(expr.AsNode(), false, nil, false)
}

func (r *RtlProcGen) Generate() {
	desc := r.gen.getDescriptor(r.desc, nil)
	name := desc.name
	callParams := desc.callParams
	retParams := desc.returnParams

	fmt.Fprintf(r.outFile, "module %s(\n  input clk,\n  input reset", name)

	ios := []*IRtlPort{}
	for _, io := range append(callParams, retParams...) {
		ios = append(ios, flattenIo(io)...) // flatten bundle or class
	}

	for _, port := range ios {
		rng := ""
		if port.desc.Width > 1 {
			rng = fmt.Sprintf(" [%d:0]", port.desc.Width-1)
		}
		fmt.Fprintf(r.outFile, ",\n  %s logic%s %s", string(port.mode), rng, port.name)
	}
	fmt.Fprint(r.outFile, ");\n\n")

	// Register non-input call parameters as signals
	for _, param := range callParams {
		if sig, ok := param.Type.(RtlSignalIo); ok && sig.mode != Input {
			r.signals[param.name] = &SignalInfo{name: param.name, desc: sig.desc, drivers: []*Driver{}}
		}
	}

	if r.decl.Body != nil {
		block := r.decl.Body.AsBlock()
		r.emitBlockVariables(block)
		fmt.Fprint(r.outFile, "\n")
		r.collectBlock(block)

		for _, stmt := range block.Statements.Nodes {
			if ast.IsExpressionStatement(stmt) {
				expr := stmt.AsExpressionStatement().Expression
				if ast.IsCallExpression(expr) {
					fun := expr.AsCallExpression().Expression
					if ast.IsIdentifier(fun) && fun.AsIdentifier().Text == "dfa" {
						dfa := NewDfaGen(r, r.outFile)
						dfa.Generate(expr)
						fmt.Fprint(r.outFile, "\n")
					}
				}
			}
			r.emitStatementInstances(stmt)
		}
	}

	r.emitSortedSignals()

	if r.decl.Body != nil {
		block := r.decl.Body.AsBlock()
		for _, stmt := range block.Statements.Nodes {
			if ast.IsReturnStatement(stmt) && stmt.AsReturnStatement().Expression != nil {
				fmt.Fprint(r.outFile, "\n")
				r.emitReturnIo(retParams[0], stmt.AsReturnStatement().Expression)
			}
		}
	}

	fmt.Fprintln(r.outFile, "endmodule")
}

func (r *RtlProcGen) emitSortedSignals() {
	lst := make([]*SignalInfo, len(r.signals))

	i := 0
	for _, v := range r.signals {
		lst[i] = v
		i++
	}

	cmp := func(a int, b int) bool {
		return lst[a].order < lst[b].order
	}

	sort.Slice(lst, cmp)

	para := false
	for _, sig := range lst {
		para = r.emitSignal(sig, para)
	}
}

func (r *RtlProcGen) emitSignal(sig *SignalInfo, para bool) bool {
	if len(sig.drivers) == 0 {
		return para
	}

	// Case 1: DFA driven signal with multiple states
	dfaDrivers := 0
	for _, d := range sig.drivers {
		if len(d.state) > 0 {
			dfaDrivers++
		}
	}
	if dfaDrivers != 0 && len(sig.drivers) > 1 {
		assert(sig.init == nil, "Continuous signal cannot have default value:", sig.name)
		assert(dfaDrivers == len(sig.drivers), "Cannot drive dfa output from outside dfa:", sig.name)

		stateVar := sig.drivers[0].state[0]
		for _, d := range sig.drivers {
			if d.state[0] != stateVar {
				assert(false, "Cannot drive output from multiple dfa:", sig.name)
			}
		}

		if para {
			fmt.Fprint(r.outFile, "\n")
		}

		fmt.Fprintln(r.outFile, "  always_comb")
		fmt.Fprintf(r.outFile, "    case (%s)\n", stateVar)
		for i, d := range sig.drivers {
			label := "default"
			if i < len(sig.drivers)-1 {
				label = d.state[1]
			}
			fmt.Fprintf(r.outFile, "      %s:\n", label)
			rhs := r.buildExpression(d.expr, false, nil, false)
			fmt.Fprintf(r.outFile, "        %s <= %s;\n", sig.name, rhs)
		}
		fmt.Fprintln(r.outFile, "    endcase")
		fmt.Fprintln(r.outFile, "")
		return false
	}

	// Case 2: Continuous assignment
	nonDelayed := 0
	for _, d := range sig.drivers {
		if !d.delayed {
			nonDelayed++
		}
	}
	if nonDelayed > 0 {
		assert(sig.init == nil, "Continuous signal cannot have default value:", sig.name)
		assert(nonDelayed == len(sig.drivers), "Continuous signal cannot have delayed driver:", sig.name)

		if len(sig.drivers) == 1 {
			rhs := r.buildExpression(sig.drivers[0].expr, false, nil, false)
			fmt.Fprintf(r.outFile, "  assign %s = %s;\n", sig.name, rhs)
			return true
		}

		if para {
			fmt.Fprint(r.outFile, "\n")
		}

		fmt.Fprintln(r.outFile, "  always_comb begin")
		for i, d := range sig.drivers {
			if i < len(sig.drivers)-1 {
				cond := r.buildExpression(d.cond, false, nil, false)
				if i == 0 {
					fmt.Fprintf(r.outFile, "    if (%s)\n", cond)
				} else {
					fmt.Fprintf(r.outFile, "    else if (%s)\n", cond)
				}
			} else {
				fmt.Fprintln(r.outFile, "    else")
			}
			rhs := r.buildExpression(d.expr, false, nil, false)
			fmt.Fprintf(r.outFile, "      %s <= %s;\n", sig.name, rhs)
		}
		fmt.Fprint(r.outFile, "  end\n\n")
		return false
	}

	if para {
		fmt.Fprint(r.outFile, "\n")
	}
	fmt.Fprint(r.outFile, "  always @(posedge clk)\n    ")
	if len(sig.drivers) == 1 && sig.drivers[0].cond == nil && sig.init == nil {
		rhs := r.buildExpression(sig.drivers[0].expr, true, nil, false)
		fmt.Fprintf(r.outFile, "%s <= %s;\n\n", sig.name, rhs)
		return false
	}
	first := true
	if sig.init != nil {
		fmt.Fprint(r.outFile, "if (reset)\n    ")
		val := buildConstant(sig.desc, sig.init, false)
		fmt.Fprintf(r.outFile, "  %s <= %s;\n", sig.name, val)
		first = false
	}
	for _, d := range sig.drivers {
		if !first {
			fmt.Fprint(r.outFile, "    ")
		}
		if d.cond != nil {
			if !first {
				fmt.Fprint(r.outFile, "else ")
			}
			cond := r.buildExpression(d.cond, false, nil, false)
			fmt.Fprintf(r.outFile, "if (%s)\n    ", cond)
		}
		rhs := r.buildExpression(d.expr, true, nil, false)
		addr := ""
		if d.addr != nil {
			addr = fmt.Sprintf("[%s]", r.buildExpression(d.addr, false, nil, true))
		}
		fmt.Fprintf(r.outFile, "  %s%s <= %s;\n", sig.name, addr, rhs)
		first = false
	}
	fmt.Fprintln(r.outFile, "")
	return false
}

func (r *RtlProcGen) emitReturnIo(io *RtlIo, expr *ast.Node) {
	if ast.IsCallExpression(expr) {
		fun := expr.AsCallExpression().Expression
		if ast.IsPropertyAccessExpression(fun) {
			obj := fun.AsPropertyAccessExpression().Expression
			method := fun.AsPropertyAccessExpression().Name().Text()

			if ast.IsIdentifier(obj) && obj.AsIdentifier().Text == "Axi" {
				args := expr.AsCallExpression().Arguments.Nodes
				switch method {
				case "createStreamIn":
					accept := args[0].AsIdentifier().Text
					valid := r.buildExpression(args[1], false, nil, false)
					data := r.buildExpression(args[2], false, nil, false)
					fmt.Fprintf(r.outFile, "  assign %s = %s_accept;\n", accept, io.name)
					fmt.Fprintf(r.outFile, "  assign %s_valid = %s;\n", io.name, valid)
					fmt.Fprintf(r.outFile, "  assign %s_data = %s;\n", io.name, data)
					return
				case "createStreamOut":
					accept := r.buildExpression(args[0], false, nil, false)
					valid := args[1].AsIdentifier().Text
					data := args[2].AsIdentifier().Text
					fmt.Fprintf(r.outFile, "  assign %s_accept = %s;\n", io.name, accept)
					fmt.Fprintf(r.outFile, "  assign %s = %s_valid;\n", valid, io.name)
					fmt.Fprintf(r.outFile, "  assign %s = %s_data;\n", data, io.name)
					return
				}
			}
		}
	}

	fmt.Fprintln(os.Stderr, expr)
	assert(false, "Unexpected logic module return value")
}

func (r *RtlProcGen) emitStatementInstances(stmt *ast.Node) {
	switch stmt.Kind {
	case ast.KindVariableStatement:
		r.emitVariableDeclarationInstances(stmt.AsVariableStatement())
	case ast.KindExpressionStatement:
		r.emitExpressionInstances(stmt.AsExpressionStatement().Expression)
	}
}

func (r *RtlProcGen) emitVariableDeclarationInstances(stmt *ast.VariableStatement) {
	lst := stmt.DeclarationList.AsVariableDeclarationList()
	for _, decl := range lst.Declarations.Nodes {
		if decl.Initializer() != nil && ast.IsCallExpression(decl.Initializer()) {
			call := decl.Initializer().AsCallExpression()
			fun := call.Expression
			if ast.IsIdentifier(fun) {

				callee := r.den.GetFunctionIdentDescriptor(fun.AsIdentifier(), r.tc)
				if callee != nil && callee.moduleType != nil {
					r.emitInstantiation(callee, call, decl.Name())
				}
			}
		}
	}
}

func (r *RtlProcGen) emitExpressionInstances(expr *ast.Node) {
	if !ast.IsCallExpression(expr) {
		return
	}

	call := expr.AsCallExpression()
	fun := call.Expression

	if ast.IsIdentifier(fun) {
		ident := fun.AsIdentifier()
		callee := r.den.GetFunctionIdentDescriptor(ident, r.tc)
		if callee != nil && callee.moduleType != nil {
			r.emitInstantiation(callee, call, nil)
		}
	}
}

func (r *RtlProcGen) emitInstantiation(desc *FunctionDescriptor, call *ast.CallExpression, lhs *ast.BindingName) {
	if desc.moduleType != nil && *desc.moduleType == "test" {
		fmt.Fprintf(r.outFile, "  // TODO %s module %s\n\n", *desc.moduleType, desc.methodName)
		return
	}

	rtlDesc := r.gen.getDescriptor(desc, call)
	assert(rtlDesc != nil)

	name := rtlDesc.name
	params := rtlDesc.callParams

	if len(params) != len(call.Arguments.Nodes) {
		panic(fmt.Sprintf("Wrong number of arguments to %s", name))
	}

	inst := r.symtab.GenNewName("i_" + name)
	fmt.Fprintf(r.outFile, "  %s %s(\n    .clk(clk),\n    .reset(reset)", name, inst)

	for i, param := range params {
		arg := call.Arguments.Nodes[i]
		if _, ok := param.Type.(RtlSignalIo); ok {
			expr := r.buildExpression(arg, false, nil, false)
			fmt.Fprintf(r.outFile, ",\n    .%s(%s)", param.name, expr)
		} else {
			if ast.IsIdentifier(arg) {
				r.emitVariableMap(rtlDesc.callParams[i], arg.AsIdentifier().Text, false)
			} else if ast.IsPropertyAccessExpression(arg) {
				prop := call.Expression.AsPropertyAccessExpression()
				obj := prop.Expression
				field := prop.Name().Text()
				if ast.IsIdentifier(obj) {
					local := obj.AsIdentifier().Text
					bundle := r.bundles[local]
					if bundle == nil {
						panic(fmt.Sprintf("Unexpected actual expression for %s parameter %s", name, rtlDesc.callParams[i].name))
					}
					r.emitBundleMap(rtlDesc.callParams[i], fmt.Sprintf("%s_%s", local, field), bundle, false)
				} else {
					panic(fmt.Sprintf("Unexpected actual expression for %s parameter %s", name, rtlDesc.callParams[i].name))
				}
			}
		}
	}

	if lhs != nil && lhs.Kind == ast.KindIdentifier {
		target := lhs.AsIdentifier().Text
		if len(rtlDesc.returnParams) == 1 {
			r.emitVariableMap(rtlDesc.returnParams[0], target, false)
		} else {
			panic(fmt.Sprintf("Expected single return from %s", name))
		}
	} else if len(rtlDesc.returnParams) > 0 {
		panic(fmt.Sprintf("Module %s with return must be used in declaration", name))
	}

	fmt.Fprint(r.outFile, ");\n\n")
}

func (r *RtlProcGen) emitVariableMap(formal *RtlIo, actual string, first bool) {
	if io, ok := r.interfaces[actual]; ok {
		r.emitInterfaceMap(formal, actual, io, first)
	} else if bundle, ok := r.bundles[actual]; ok {
		r.emitBundleMap(formal, actual, bundle, first)
	} else {
		panic(fmt.Sprintf("Unexpected variable %s in module call", actual))
	}
}

func (r *RtlProcGen) emitInterfaceMap(formal *RtlIo, actual string, actualType RtlIoType, first bool) {
	switch at := actualType.(type) {
	case RtlSignalIo:
		if !first {
			fmt.Fprint(r.outFile, ",")
		}
		fmt.Fprintf(r.outFile, "\n    .%s(%s)", formal.name, actual)
	case RtlClassIo:
		fclass := formal.Type.(RtlClassIo)
		for i, fsig := range fclass.signals {
			asig := at.signals[i]
			if i > 0 || !first {
				fmt.Fprint(r.outFile, ",")
			}
			fmt.Fprintf(r.outFile, "\n    .%s_%s(%s_%s)", formal.name, fsig.name, actual, asig.name)
		}
	default:
		panic("Unsupported interface map kind")
	}
}

func (r *RtlProcGen) emitBundleMap(formal *RtlIo, actual string, actualType map[string]RtlIoType, first bool) {
	fbundle := formal.Type.(RtlBundleIo)
	for _, member := range fbundle.members {
		aio, ok := actualType[member.name]
		if !ok {
			panic(fmt.Sprintf("Missing bundle member %s in actual %s", member.name, actual))
		}
		switch aio := aio.(type) {
		case RtlSignalIo:
			if !first {
				fmt.Fprint(r.outFile, ",")
			}
			fmt.Fprintf(r.outFile, "\n    .%s_%s(%s_%s)", formal.name, member.name, actual, member.name)
		case RtlClassIo:
			for _, io := range aio.signals {
				if !first {
					fmt.Fprint(r.outFile, ",")
				}
				fmt.Fprintf(r.outFile, "\n    .%s_%s_%s(%s_%s_%s)", formal.name, member.name, io.name, actual, member.name, io.name)
				first = false
			}
		default:
			panic("Unsupported bundle member kind")
		}
		first = false
	}
}

func (r *RtlProcGen) buildExpression(expr *ast.Node, delayed bool, tt *checker.Type, decimal bool) string {
	switch expr.Kind {
	case ast.KindIdentifier:
		ty := r.tc.GetTypeAtLocation(expr)
		if isSignalType(ty) || isPortType(ty) {
			return expr.AsIdentifier().Text
		}

	case ast.KindBinaryExpression:
		bin := expr.AsBinaryExpression()
		op, ok := verilogTokenOp[bin.OperatorToken.Kind]
		if !ok {
			panic(fmt.Sprintf("Unsupported binary operator: %s", bin.OperatorToken.Kind))
		}
		left := r.buildExpression(bin.Left, delayed, nil, false)
		right := r.buildExpression(bin.Right, delayed, nil, false)
		return fmt.Sprintf("%s %s %s", left, op, right)

	case ast.KindPrefixUnaryExpression:
		pre := expr.AsPrefixUnaryExpression()
		op, ok := verilogTokenOp[pre.Operator]
		if !ok {
			panic(fmt.Sprintf("Unsupported prefix operator: %s", pre.Operator))
		}
		return fmt.Sprintf("%s%s", op, r.buildExpression(pre.Operand, delayed, nil, false))

	case ast.KindParenthesizedExpression:
		par := expr.AsParenthesizedExpression()
		return fmt.Sprintf("(%s)", r.buildExpression(par.Expression, delayed, nil, false))

	case ast.KindCallExpression:
		call := expr.AsCallExpression()
		if ast.IsPropertyAccessExpression(call.Expression) {
			prop := call.Expression.AsPropertyAccessExpression()
			obj := prop.Expression
			method := prop.Name().Text()
			if ast.IsIdentifier(obj) {
				name := obj.AsIdentifier().Text
				typ := r.tc.GetTypeAtLocation(obj)
				if method == "bit" && isSignalType(typ) {
					arg := r.buildExpression(call.Arguments.Nodes[0], delayed, nil, true)
					return fmt.Sprintf("%s[%s]", name, arg)
				} else if method == "read" && isBlockRamType(typ) {
					if !delayed {
						panic("Cannot read memory in continuous assignment")
					}
					arg := r.buildExpression(call.Arguments.Nodes[0], delayed, nil, true)
					return fmt.Sprintf("%s[%s]", name, arg)
				}
			}
		}

	case ast.KindStringLiteral:
		return expr.Text()

	case ast.KindNumericLiteral:
		var desc *checker.TypeDescriptor
		if tt != nil {
			desc = checker.GetTypeDescriptor(r.tc, tt, expr, false)
		} else {
			desc = checker.GetTypeDescriptor(r.tc, r.tc.GetTypeAtLocation(expr), expr, false)
		}
		if desc != nil {
			return buildConstant(desc, expr, decimal)
		}

		value := checker.GetConstExpression(expr)
		if num, ok := value.(jsnum.Number); ok {
			f := float64(num)
			if decimal {
				return fmt.Sprintf("%d", int64(f))
			}

			x := f
			if x < 0 {
				x = -x
			}
			width := 1
			mx := 2
			for x >= float64(mx) {
				width++
				mx = 1 << width
			}
			u := uint64(x)
			if f < 0 {
				width++
				u = (1 << width) - u
			}
			if width%4 == 0 {
				return fmt.Sprintf("%d'h%x", width, u)
			}
			return fmt.Sprintf("%d'd%d", width, u)
		}
	}

	fmt.Fprintf(os.Stderr, "Unhandled expression kind: %v\n", expr.Kind)
	panic("Unhandled expression in buildExpression")
}

var verilogTokenOp = map[ast.Kind]string{
	ast.KindLessThanToken:                "<",
	ast.KindGreaterThanToken:             ">",
	ast.KindLessThanEqualsToken:          "<=",
	ast.KindGreaterThanEqualsToken:       ">=",
	ast.KindEqualsEqualsToken:            "==",
	ast.KindExclamationEqualsToken:       "!=",
	ast.KindEqualsEqualsEqualsToken:      "==",
	ast.KindExclamationEqualsEqualsToken: "!=",
	ast.KindEqualsGreaterThanToken:       "=>",
	ast.KindPlusToken:                    "+",
	ast.KindMinusToken:                   "-",
	ast.KindAsteriskToken:                "*",
	ast.KindHashPlusToken:                "+",
	ast.KindHashMinusToken:               "-",
	ast.KindHashAsteriskToken:            "*",
	ast.KindSlashToken:                   "/",
	ast.KindPercentToken:                 "%",
	ast.KindLessThanLessThanToken:        "<<",
	ast.KindGreaterThanGreaterThanToken:  ">>",
	ast.KindAmpersandToken:               "&",
	ast.KindBarToken:                     "|",
	ast.KindCaretToken:                   "^",
	ast.KindExclamationToken:             "!",
	ast.KindTildeToken:                   "~",
	ast.KindAmpersandAmpersandToken:      "&&",
	ast.KindBarBarToken:                  "||",
}

func buildConstant(desc *checker.TypeDescriptor, expr *ast.Node, decimal bool) string {
	value := checker.GetConstExpression(expr)
	if value == nil {
		panic("buildConstant: expression did not evaluate to constant")
	}

	switch v := value.(type) {
	case jsnum.Number:
		n := float64(v)

		if decimal {
			return fmt.Sprintf("%d", int64(n))
		}

		negative := n < 0
		x := n
		if negative {
			x = -x
		}

		u := uint64(x)

		width := 1
		for (1 << width) <= u {
			width++
		}
		if desc.IsSigned || negative {
			width++
		}
		if width < int(desc.Width) {
			width = int(desc.Width)
		}

		if negative {
			u = (1 << width) - u
		}

		if width%4 == 0 {
			return fmt.Sprintf("%d'h%x", width, u)
		}
		return fmt.Sprintf("%d'd%d", width, u)
	default:
		panic("buildConstant: unsupported constant type")
	}
}

func (r *RtlProcGen) collectBlock(block *ast.Block) {
	for _, stmt := range block.Statements.Nodes {
		r.collectStatement(stmt)
	}
}

func (r *RtlProcGen) collectStatement(stmt *ast.Node) {
	if ast.IsExpressionStatement(stmt) {
		r.collectExpression(stmt.AsExpressionStatement().Expression, nil)
	}
}

func (r *RtlProcGen) collectExpression(expr *ast.Node, parentType *checker.Type) {
	var cond *ast.Node

	if ast.IsCallExpression(expr) {
		call := expr.AsCallExpression()
		fun := call.Expression
		if ast.IsPropertyAccessExpression(fun) {
			pa := fun.AsPropertyAccessExpression()
			op := pa.Name().Text()
			if ast.IsIdentifier(pa.Expression) {
				name := pa.Expression.AsIdentifier().Text
				typ := r.tc.GetTypeAtLocation(pa.Expression)
				if op == "is" && (isSignalType(typ) || isPortType(typ)) {
					sig, ok := r.signals[name]
					if ok {
						if len(call.Arguments.Nodes) == 2 {
							cond = call.Arguments.Nodes[1]
							assert(ast.IsArrowFunction(cond))
							cond = cond.AsArrowFunction().Body
						}
						r.addDriver(sig, &Driver{
							expr:    call.Arguments.Nodes[0],
							cond:    cond,
							delayed: false,
						})
					}
				} else if op == "write" && isBlockRamType(typ) {
					sig, ok := r.signals[name]
					if ok {
						if len(call.Arguments.Nodes) == 3 {
							cond = call.Arguments.Nodes[2]
						}
						r.addDriver(sig, &Driver{
							addr:    call.Arguments.Nodes[0],
							expr:    call.Arguments.Nodes[1],
							cond:    cond,
							delayed: true,
						})
					}
				}
			} else if ast.IsPropertyAccessExpression(pa.Expression) {
				pa0 := pa.Expression.AsPropertyAccessExpression()
				op0 := pa0.Name().Text()
				if ast.IsIdentifier(pa0.Expression) {
					name := pa0.Expression.AsIdentifier().Text
					if op0 == "next" && op == "is" {
						sig, ok := r.signals[name]
						if ok {
							if len(call.Arguments.Nodes) == 2 {
								cond = call.Arguments.Nodes[1]
								assert(ast.IsArrowFunction(cond))
								cond = cond.AsArrowFunction().Body
							}
							r.addDriver(sig, &Driver{
								expr:    call.Arguments.Nodes[0],
								cond:    cond,
								delayed: true,
							})
						}
					}
				}
			}
		}
	}
}

func (r *RtlProcGen) emitBlockVariables(block *ast.Block) {
	for _, stmt := range block.Statements.Nodes {
		r.emitStatementVariables(stmt)
	}
}

func (r *RtlProcGen) emitStatementVariables(stmt *ast.Node) {
	switch stmt.Kind {
	case ast.KindVariableStatement:
		vs := stmt.AsVariableStatement()
		r.emitVariableDeclarationListVariables(vs.DeclarationList.AsVariableDeclarationList())
	case ast.KindTypeAliasDeclaration:
		td := stmt.AsTypeAliasDeclaration()
		if td.Type.Kind == ast.KindUnionType {
			enumType := td.Type.AsUnionTypeNode()
			enums := []string{}
			for _, t := range enumType.Types.Nodes {
				if ast.IsLiteralTypeNode(t) && ast.IsLiteralExpression(t.AsLiteralTypeNode().Literal) {
					enums = append(enums, t.AsLiteralTypeNode().Literal.Text())
				}
			}
			fmt.Fprintf(r.outFile, "  typedef enum {%s} %s_t;\n\n", strings.Join(enums, ", "), td.Name().Text())
		}
	}
}

func (r *RtlProcGen) emitVariableDeclarationListVariables(lst *ast.VariableDeclarationList) {
	for _, decl := range lst.Declarations.Nodes {
		if !ast.IsIdentifier(decl.Name()) {
			continue // skip non-simple bindings for now
		}

		name := r.symtab.GenName(decl.AsVariableDeclaration())
		typ := r.tc.GetTypeAtLocation(decl)

		if isSignalType(typ) && len(checker.ResolvedTypeArguments(typ)) > 0 {
			elem := checker.ResolvedTypeArguments(typ)[0]
			if elem.Flags()&checker.TypeFlagsUnion != 0 {
				fmt.Fprintf(r.outFile, "  %s_t %s;\n", checker.GetReferenceTypeSymbol(elem).Name, name)
			} else {
				desc := checker.RequireTypeDescriptor(r.tc, elem, decl.Initializer())
				init := getSignalInitializer(decl.Initializer())
				r.defineSignal(name, &SignalInfo{name: name, desc: desc, init: init, drivers: []*Driver{}})
				rng := ""
				if desc.Width > 1 {
					rng = fmt.Sprintf(" [%d:0]", desc.Width-1)
				}
				fmt.Fprintf(r.outFile, "  logic%s %s;\n", rng, name)
			}
		} else if isBlockRamType(typ) && len(checker.ResolvedTypeArguments(typ)) > 0 {
			elem := checker.ResolvedTypeArguments(typ)[0]
			init := decl.Initializer()
			assert(init != nil, "Missing BlockRam initializer")
			assert(ast.IsCallExpression(init), "Missing BlockRam constructor")

			opts := init.AsCallExpression().Arguments.Nodes
			assert(checker.IsConstExpression(opts[0]), "BlockRam constructor must have constant depth")
			depth, ok := checker.NumberToUint32(checker.GetConstExpression(opts[0]))
			assert(ok, "BlockRam depth is not a number")

			desc := checker.RequireTypeDescriptor(r.tc, elem, init)

			rng := ""
			if desc.Width > 1 {
				rng = fmt.Sprintf(" [%d:0]", desc.Width-1)
			}

			r.defineSignal(name, &SignalInfo{name: name, desc: desc, depth: &depth, drivers: []*Driver{}})
			fmt.Fprintf(r.outFile, "  logic%s %s[%d];\n", rng, name, depth)
		} else if isStreamType(typ) {
			io, ok := getPortType("client", typ, r.tc, false)
			assert(ok, "Unspected logic module IO type")
			r.emitIoSignals(name, io)
			r.interfaces[name] = io
		} else if checker.IsObjectType(typ) {
			bundle, ok := buildBundle(typ, decl, r.tc)
			if ok {
				r.bundles[name] = bundle
				for field, sig := range bundle {
					r.emitIoSignals(fmt.Sprintf("%s_%s", name, field), sig)
				}
			} else {
				panic("Unrecognized logic declaration: " + name)
			}
		}
	}
}

func (r *RtlProcGen) emitIoSignals(name string, io RtlIoType) {
	switch t := io.(type) {
	case RtlSignalIo:
		rng := ""
		if t.desc.Width > 1 {
			rng = fmt.Sprintf(" [%d:0]", t.desc.Width-1)
		}
		fmt.Fprintf(r.outFile, "  logic%s %s;\n", rng, name)
	case RtlClassIo:
		for _, sig := range t.signals {
			r.emitIoSignals(fmt.Sprintf("%s_%s", name, sig.name), sig.Type)
		}
	default:
		panic("Unhandled IO signal type")
	}
}

type SignalInfo struct {
	name    string
	desc    *checker.TypeDescriptor
	drivers []*Driver
	depth   *uint32
	init    *ast.Node
	order   int
}

type Driver struct {
	expr    *ast.Node
	addr    *ast.Node
	cond    *ast.Node
	state   []string
	delayed bool
}

// PortMode represents the direction of a port
// input, output, inout

type PortMode string

const (
	Input  PortMode = "input"
	Output PortMode = "output"
	InOut  PortMode = "inout"
)

type IoRole string

const (
	Provider IoRole = "provider"
	Client   IoRole = "client"
)

type RtlModDescriptor struct {
	name         string
	callParams   []*RtlIo
	returnParams []*RtlIo
}

type RtlIo struct {
	name string
	Type RtlIoType
}

type RtlIoType interface {
	isRtlIoType()
}

// Signal

type RtlSignalIo struct {
	kind string // "signal"
	mode PortMode
	desc *checker.TypeDescriptor
}

func (RtlSignalIo) isRtlIoType() {}

// Class

type RtlClassIo struct {
	kind    string // "class"
	name    string
	signals []*RtlIo
	proto   *IoInterface
}

func (RtlClassIo) isRtlIoType() {}

// Bundle

type RtlBundleIo struct {
	kind    string // "bundle"
	members []*RtlIo
}

func (RtlBundleIo) isRtlIoType() {}

type IRtlPort struct {
	name string
	mode PortMode
	desc *checker.TypeDescriptor
}

type IoInterface struct {
	params []*checker.TypeDescriptor
	cls    *IoInterfaceClass
}

type IoInterfaceClass struct {
	name    string
	hls     *HlsInterface
	signals []string
}

type HlsInterface struct {
	connector string
	role      string
	aliases   map[string]string
}

func flattenIo(io *RtlIo) []*IRtlPort {
	switch typ := io.Type.(type) {
	case RtlSignalIo:
		return []*IRtlPort{{
			name: io.name,
			mode: typ.mode,
			desc: typ.desc,
		}}
	case RtlClassIo:
		var ports []*IRtlPort
		for _, sig := range typ.signals {
			for _, p := range flattenIo(sig) {
				p.name = io.name + "_" + p.name
				ports = append(ports, p)
			}
		}
		return ports
	case RtlBundleIo:
		var ports []*IRtlPort
		for _, member := range typ.members {
			for _, p := range flattenIo(member) {
				p.name = io.name + "_" + p.name
				ports = append(ports, p)
			}
		}
		return ports
	default:
		panic("unknown RtlIoType")
	}
}

// Entry point to build RTL IO from a TypeScript type
func buildRtlIo(role IoRole, ty *checker.Type, node *ast.Node, hlsStyle bool, tc *checker.Checker) RtlIoType {
	if pt, ok := getPortType(role, ty, tc, hlsStyle); ok {
		return pt
	}

	if checker.IsObjectType(ty) {
		if bundle, ok := buildBundle(ty, node, tc); ok {
			members := make([]*RtlIo, 0, len(bundle))
			for name, typ := range bundle {
				members = append(members, &RtlIo{name: name, Type: typ})
			}
			return RtlBundleIo{
				kind:    "bundle",
				members: members,
			}
		}
	}

	panic(fmt.Sprintf("Unrecognized logic module IO type: %+v", ty))
}

func getPortType(role IoRole, ty *checker.Type, tc *checker.Checker, hlsStyle bool) (RtlIoType, bool) {
	if isPortType(ty) && len(checker.ResolvedTypeArguments(ty)) > 0 {
		desc := checker.RequireTypeDescriptor(tc, checker.ResolvedTypeArguments(ty)[0], nil)
		var mode PortMode
		switch checker.GetReferenceTypeSymbol(ty).Name { // e.g., "IPort", "OPort"
		case "IPort":
			mode = Input
		case "OPort":
			mode = Output
		case "IOPort":
			mode = InOut
		default:
			return nil, false
		}
		return RtlSignalIo{
			kind: "signal",
			mode: mode,
			desc: desc,
		}, true
	}

	if isStreamType(ty) && len(checker.ResolvedTypeArguments(ty)) > 0 {
		sym := checker.GetReferenceTypeSymbol(ty).Name
		cls := InterfacesLib[sym]
		desc := checker.RequireTypeDescriptor(tc, checker.ResolvedTypeArguments(ty)[0], nil)

		var signals []*RtlIo
		if hlsStyle {
			signals = getHlsSignals(cls, []*checker.TypeDescriptor{desc})
		} else {
			dir := core.IfElse(sym == "StreamIn", core.IfElse(role == Client, "input", "output"), core.IfElse(role == Client, "output", "input"))
			if dir == "input" {
				signals = []*RtlIo{
					{name: "valid", Type: RtlSignalIo{"signal", Input, &checker.TypeDescriptor{IsSigned: false, Width: 1}}},
					{name: "accept", Type: RtlSignalIo{"signal", Output, &checker.TypeDescriptor{IsSigned: false, Width: 1}}},
					{name: "data", Type: RtlSignalIo{"signal", Input, desc}},
				}
			} else {
				signals = []*RtlIo{
					{name: "valid", Type: RtlSignalIo{"signal", Output, &checker.TypeDescriptor{IsSigned: false, Width: 1}}},
					{name: "accept", Type: RtlSignalIo{"signal", Input, &checker.TypeDescriptor{IsSigned: false, Width: 1}}},
					{name: "data", Type: RtlSignalIo{"signal", Output, desc}},
				}
			}
		}

		return RtlClassIo{
			kind:    "class",
			name:    sym,
			signals: signals,
			proto: &IoInterface{
				params: []*checker.TypeDescriptor{desc},
				cls:    cls,
			},
		}, true
	}

	return nil, false
}

func getHlsSignals(cls *IoInterfaceClass, args []*checker.TypeDescriptor) []*RtlIo {
	if cls.hls == nil {
		panic("not an HLS interface")
	}
	conn := hlsConnectors[cls.hls.connector]
	if conn == nil {
		panic("missing connector: " + cls.hls.connector)
	}

	typeMap := map[string]string{}
	for i, arg := range args {
		var spec string
		if arg.IsSigned {
			spec = fmt.Sprintf("int%d", arg.Width)
		} else {
			spec = fmt.Sprintf("uint%d", arg.Width)
		}
		typeMap[conn.Parameters[i].Name] = spec
	}

	signalMap := map[string]*RtlIo{}
	for _, meth := range conn.Methods {
		if !contains(meth.Roles, cls.hls.role) {
			continue
		}

		// method-level IOs
		req := &RtlIo{
			name: meth.Name + "_req",
			Type: RtlSignalIo{
				kind: "signal",
				mode: Output,
				desc: &checker.TypeDescriptor{IsSigned: false, Width: 1},
			},
		}
		signalMap[mapName(req.name, cls.hls.aliases)] = req

		if meth.Ready != nil && *meth.Ready != 0 {
			rdy := &RtlIo{
				name: meth.Name + "_rdy",
				Type: RtlSignalIo{
					kind: "signal",
					mode: Input,
					desc: &checker.TypeDescriptor{IsSigned: false, Width: 1},
				},
			}
			signalMap[mapName(rdy.name, cls.hls.aliases)] = rdy
		}

		if meth.Cycles != nil && strings.Contains(*meth.Cycles, "*") {
			ack := &RtlIo{
				name: meth.Name + "_ack",
				Type: RtlSignalIo{
					kind: "signal",
					mode: Input,
					desc: &checker.TypeDescriptor{IsSigned: false, Width: 1},
				},
			}
			signalMap[mapName(ack.name, cls.hls.aliases)] = ack
		}

		for _, param := range meth.Parameters {
			ptype := param.Type
			if t, ok := typeMap[ptype]; ok {
				ptype = t
			}
			signal := &RtlIo{
				name: param.Name,
				Type: RtlSignalIo{
					kind: "signal",
					mode: Output,
					desc: parseTypeSpec(ptype),
				},
			}
			signalMap[mapName(signal.name, cls.hls.aliases)] = signal
		}

		if meth.Type != "void" {
			rtype := meth.Type
			if t, ok := typeMap[rtype]; ok {
				rtype = t
			}
			ret := &RtlIo{
				name: meth.Name + "_r_e_t_u_r_n",
				Type: RtlSignalIo{
					kind: "signal",
					mode: Input,
					desc: parseTypeSpec(rtype),
				},
			}
			signalMap[mapName(ret.name, cls.hls.aliases)] = ret
		}
	}

	signals := make([]*RtlIo, len(cls.signals))
	for i, name := range cls.signals {
		signals[i] = signalMap[name]
	}
	return signals
}

func mapName(name string, subst map[string]string) string {
	if v, ok := subst[name]; ok {
		return v
	}
	return name
}

func getSignalInitializer(expr *ast.Expression) *ast.Expression {
	if expr != nil {
		if ast.IsCallExpression(expr) {
			call := expr.AsCallExpression()
			if ast.IsIdentifier(call.Expression) && call.Expression.AsIdentifier().Text == "signal" {
				if len(call.Arguments.Nodes) == 1 {
					return call.Arguments.Nodes[0]
				}
			}
		}
	}
	return nil
}

func buildBundle(ty *checker.Type, node *ast.Node, tc *checker.Checker) (map[string]RtlIoType, bool) {
	bundle := map[string]RtlIoType{}
	props := checker.GetObjectTypeProperties(tc, ty)
	for _, prop := range props {
		pt := tc.GetTypeOfSymbolAtLocation(prop, node)
		if ioType, ok := getPortType(Client, pt, tc, false); ok {
			bundle[prop.Name] = ioType
		} else {
			return nil, false
		}
	}
	return bundle, true
}

func isPortType(t *checker.Type) bool {
	return checker.IsTypeReferenceOf(t, "IPort") || checker.IsTypeReferenceOf(t, "OPort")
}

func isSignalType(t *checker.Type) bool {
	return checker.IsTypeReferenceOf(t, "Signal")
}

func isBlockRamType(t *checker.Type) bool {
	return checker.IsTypeReferenceOf(t, "BlockRam")
}

func isStreamType(t *checker.Type) bool {
	return checker.IsTypeReferenceOf(t, "StreamIn") || checker.IsTypeReferenceOf(t, "StreamOut")
}
