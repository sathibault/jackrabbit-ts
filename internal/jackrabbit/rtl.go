package jackrabbit

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/jsnum"
)

type RtlGenerator struct {
	den     *RabbitDen
	Tc      *checker.Checker
	OutFile *os.File
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
		Tc:      tc,
		OutFile: file,
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
		sig, rt = checker.GetResolvedParameterTypes(r.Tc, call.AsNode())
	} else {
		sig, rt = checker.GetDeclarationParameterTypes(r.Tc, desc.declaration)
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
			typ = r.Tc.GetTypeAtLocation(param)
		}
		callParams = append(callParams, &RtlIo{
			Name: pname,
			Type: buildRtlIo(Client, typ, param, isHls, r.Tc),
		})
	}

	var returnParams []*RtlIo
	prefix := strings.ToLower(name)
	if checker.IsTypeReference(rt) && checker.IsTupleType(r.Tc, rt) {
		elems := checker.ResolvedTypeArguments(rt)
		for i, el := range elems {
			returnParams = append(returnParams, &RtlIo{
				Name: fmt.Sprintf("%s_%d", prefix, i),
				Type: buildRtlIo(Provider, el, decl, isHls, r.Tc),
			})
		}
	} else if (rt.Flags() & checker.TypeFlagsVoid) == 0 {
		returnParams = append(returnParams, &RtlIo{
			Name: fmt.Sprintf("%s_0", prefix),
			Type: buildRtlIo(Provider, rt, decl, isHls, r.Tc),
		})
	}

	modDesc := &RtlModDescriptor{
		Name:         name,
		CallParams:   callParams,
		ReturnParams: returnParams,
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
	Decl       *ast.FunctionDeclaration
	Desc       *FunctionDescriptor
	Gen        *RtlGenerator
	Tc         *checker.Checker
	OutFile    *os.File
	Symtab     *Namer
	Signals    map[string]*SignalInfo
	Interfaces map[string]RtlIoType
	Bundles    map[string]map[string]RtlIoType
}

func NewRtlProcGen(den *RabbitDen, decl *ast.FunctionDeclaration, desc *FunctionDescriptor, gen *RtlGenerator) *RtlProcGen {
	return &RtlProcGen{
		den:        den,
		Decl:       decl,
		Desc:       desc,
		Gen:        gen,
		Tc:         gen.Tc,
		OutFile:    gen.OutFile,
		Symtab:     NewNamer(),
		Signals:    map[string]*SignalInfo{},
		Interfaces: map[string]RtlIoType{},
		Bundles:    map[string]map[string]RtlIoType{},
	}
}

func (r *RtlProcGen) addStateDriver(name string, state []string, expr *ast.Expression) {
	sig, ok := r.Signals[name]
	assert(ok, "Undesigned signal name:", name)
	sig.Drivers = append(sig.Drivers, &Driver{
		Expr:    expr,
		State:   state,
		Delayed: false,
	})
}

func (r *RtlProcGen) genExpression(expr *ast.Expression) string {
	return r.buildExpression(expr.AsNode(), false, nil, false)
}

func (r *RtlProcGen) Generate() {
	desc := r.Gen.getDescriptor(r.Desc, nil)
	name := desc.Name
	callParams := desc.CallParams
	retParams := desc.ReturnParams

	fmt.Fprintf(r.OutFile, "module %s(\n  input clk,\n  input reset", name)

	ios := []*IRtlPort{}
	for _, io := range append(callParams, retParams...) {
		ios = append(ios, flattenIo(io)...) // flatten bundle or class
	}

	for _, port := range ios {
		rng := ""
		if port.Desc.Width > 1 {
			rng = fmt.Sprintf(" [%d:0]", port.Desc.Width-1)
		}
		fmt.Fprintf(r.OutFile, ",\n  %s logic%s %s", string(port.Mode), rng, port.Name)
	}
	fmt.Fprint(r.OutFile, "\n);\n\n")

	// Register non-input call parameters as signals
	for _, param := range callParams {
		if sig, ok := param.Type.(RtlSignalIo); ok && sig.Mode != Input {
			r.Signals[param.Name] = &SignalInfo{Name: param.Name, Desc: sig.Desc, Drivers: []*Driver{}}
		}
	}

	if r.Decl.Body != nil {
		block := r.Decl.AsBlock()
		r.emitBlockVariables(block)
		fmt.Fprint(r.OutFile, "\n")
		r.collectBlock(block)

		for _, stmt := range block.Statements.Nodes {
			if ast.IsExpressionStatement(stmt) {
				expr := stmt.AsExpressionStatement().Expression
				if ast.IsCallExpression(expr) {
					fun := expr.AsCallExpression().Expression
					if ast.IsIdentifier(fun) && fun.AsIdentifier().Text == "dfa" {
						dfa := NewDfaGen(r, r.OutFile)
						dfa.Generate(expr)
						fmt.Fprint(r.OutFile, "\n")
					}
				}
			}
			r.emitStatementInstances(stmt)
		}
	}

	para := false
	for _, sig := range r.Signals {
		para = r.emitSignal(sig, para)
	}

	if r.Decl.Body != nil {
		block := r.Decl.AsBlock()
		for _, stmt := range block.Statements.Nodes {
			if ast.IsReturnStatement(stmt) && stmt.AsReturnStatement().Expression != nil {
				fmt.Fprint(r.OutFile, "\n")
				r.emitReturnIo(retParams[0], stmt.AsReturnStatement().Expression)
			}
		}
	}

	fmt.Fprintln(r.OutFile, "endmodule")
}

func (r *RtlProcGen) emitSignal(sig *SignalInfo, para bool) bool {
	if len(sig.Drivers) == 0 {
		return para
	}

	// Case 1: DFA driven signal with multiple states
	dfaDrivers := 0
	for _, d := range sig.Drivers {
		if len(d.State) > 0 {
			dfaDrivers++
		}
	}
	if dfaDrivers != 0 && len(sig.Drivers) > 1 {
		assert(sig.Init == nil, "Continuous signal cannot have default value:", sig.Name)
		assert(dfaDrivers == len(sig.Drivers), "Cannot drive dfa output from outside dfa:", sig.Name)

		stateVar := sig.Drivers[0].State[0]
		for _, d := range sig.Drivers {
			if d.State[0] != stateVar {
				assert(false, "Cannot drive output from multiple dfa:", sig.Name)
			}
		}

		if para {
			fmt.Fprint(r.OutFile, "\n")
		}

		fmt.Fprintln(r.OutFile, "  always_comb")
		fmt.Fprintf(r.OutFile, "    case (%s)\n", stateVar)
		for i, d := range sig.Drivers {
			label := "default"
			if i < len(sig.Drivers)-1 {
				label = d.State[1]
			}
			fmt.Fprintf(r.OutFile, "      %s:\n", label)
			rhs := r.buildExpression(d.Expr, false, nil, false)
			fmt.Fprintf(r.OutFile, "        %s <= %s;\n", sig.Name, rhs)
		}
		fmt.Fprintln(r.OutFile, "    endcase\n")
		fmt.Fprintln(r.OutFile, "")
		return false
	}

	// Case 2: Continuous assignment
	nonDelayed := 0
	for _, d := range sig.Drivers {
		if !d.Delayed {
			nonDelayed++
		}
	}
	if nonDelayed > 0 {
		assert(sig.Init == nil, "Continuous signal cannot have default value:", sig.Name)
		assert(nonDelayed == len(sig.Drivers), "Continuous signal cannot have delayed driver:", sig.Name)

		if len(sig.Drivers) == 1 {
			rhs := r.buildExpression(sig.Drivers[0].Expr, false, nil, false)
			fmt.Fprintf(r.OutFile, "  assign %s = %s;\n", sig.Name, rhs)
			return true
		}

		if para {
			fmt.Fprint(r.OutFile, "\n")
		}

		fmt.Fprintln(r.OutFile, "  always_comb begin")
		for i, d := range sig.Drivers {
			if i < len(sig.Drivers)-1 {
				cond := r.buildExpression(d.Cond, false, nil, false)
				if i == 0 {
					fmt.Fprintf(r.OutFile, "    if (%s)\n", cond)
				} else {
					fmt.Fprintf(r.OutFile, "    else if (%s)\n", cond)
				}
			} else {
				fmt.Fprintln(r.OutFile, "    else")
			}
			rhs := r.buildExpression(d.Expr, false, nil, false)
			fmt.Fprintf(r.OutFile, "      %s <= %s;\n", sig.Name, rhs)
		}
		fmt.Fprintln(r.OutFile, "  end\n")
		return false
	}

	if para {
		fmt.Fprint(r.OutFile, "\n")
	}
	fmt.Fprint(r.OutFile, "  always @(posedge clk)\n    ")
	if len(sig.Drivers) == 1 && sig.Drivers[0].Cond == nil && sig.Init == nil {
		rhs := r.buildExpression(sig.Drivers[0].Expr, true, nil, false)
		fmt.Fprintf(r.OutFile, "%s <= %s;\n\n", sig.Name, rhs)
		return false
	}
	first := true
	if sig.Init != nil {
		fmt.Fprint(r.OutFile, "if (reset)\n    ")
		val := buildConstant(sig.Desc, sig.Init)
		fmt.Fprintf(r.OutFile, "  %s <= %s;\n", sig.Name, val)
		first = false
	}
	for _, d := range sig.Drivers {
		if !first {
			fmt.Fprint(r.OutFile, "    ")
		}
		if d.Cond != nil {
			if !first {
				fmt.Fprint(r.OutFile, "else ")
			}
			cond := r.buildExpression(d.Cond, false, nil, false)
			fmt.Fprintf(r.OutFile, "if (%s)\n    ", cond)
		}
		rhs := r.buildExpression(d.Expr, true, nil, false)
		addr := ""
		if d.Addr != nil {
			addr = fmt.Sprintf("[%s]", r.buildExpression(d.Addr, false, nil, true))
		}
		fmt.Fprintf(r.OutFile, "  %s%s <= %s;\n", sig.Name, addr, rhs)
		first = false
	}
	fmt.Fprintln(r.OutFile, "")
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
					fmt.Fprintf(r.OutFile, "  assign %s = %s_accept;\n", accept, io.Name)
					fmt.Fprintf(r.OutFile, "  assign %s_valid = %s;\n", io.Name, valid)
					fmt.Fprintf(r.OutFile, "  assign %s_data = %s;\n", io.Name, data)
					return
				case "createStreamOut":
					accept := r.buildExpression(args[0], false, nil, false)
					valid := args[1].AsIdentifier().Text
					data := args[2].AsIdentifier().Text
					fmt.Fprintf(r.OutFile, "  assign %s_accept = %s;\n", io.Name, accept)
					fmt.Fprintf(r.OutFile, "  assign %s = %s_valid;\n", valid, io.Name)
					fmt.Fprintf(r.OutFile, "  assign %s = %s_data;\n", data, io.Name)
					return
				}
			}
		}
	}

	fmt.Fprint(r.OutFile, "  // TODO: handle emitReturnIo fallback case\n")
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

				callee := r.den.GetFunctionIdentDescriptor(fun.AsIdentifier(), r.Tc)
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
		callee := r.den.GetFunctionIdentDescriptor(ident, r.Tc)
		if callee != nil && callee.moduleType != nil {
			r.emitInstantiation(callee, call, nil)
		}
	}
}

func (r *RtlProcGen) emitInstantiation(desc *FunctionDescriptor, call *ast.CallExpression, lhs *ast.BindingName) {
	if desc.moduleType != nil && *desc.moduleType == "test" {
		fmt.Fprintf(r.OutFile, "  // TODO %s module %s\n\n", desc.moduleType, desc.methodName)
		return
	}

	rtlDesc := r.Gen.getDescriptor(desc, call)
	assert(rtlDesc != nil)

	name := rtlDesc.Name
	params := rtlDesc.CallParams

	if len(params) != len(call.Arguments.Nodes) {
		panic(fmt.Sprintf("Wrong number of arguments to %s", name))
	}

	inst := r.Symtab.GenNewName("i_" + name)
	fmt.Fprintf(r.OutFile, "  %s %s(\n    .clk(clk),\n    .reset(reset)", name, inst)

	for i, param := range params {
		arg := call.Arguments.Nodes[i]
		if _, ok := param.Type.(RtlSignalIo); ok {
			expr := r.buildExpression(arg, false, nil, false)
			fmt.Fprintf(r.OutFile, ",\n    .%s(%s)", param.Name, expr)
		} else {
			if ast.IsIdentifier(arg) {
				r.emitVariableMap(rtlDesc.CallParams[i], arg.AsIdentifier().Text, false)
			} else if ast.IsPropertyAccessExpression(arg) {
				prop := call.Expression.AsPropertyAccessExpression()
				obj := prop.Expression
				field := prop.Name().Text()
				if ast.IsIdentifier(obj) {
					local := obj.AsIdentifier().Text
					bundle := r.Bundles[local]
					if bundle == nil {
						panic(fmt.Sprintf("Unexpected actual expression for %s parameter %s", name, rtlDesc.CallParams[i].Name))
					}
					r.emitBundleMap(rtlDesc.CallParams[i], fmt.Sprintf("%s_%s", local, field), bundle, false)
				} else {
					panic(fmt.Sprintf("Unexpected actual expression for %s parameter %s", name, rtlDesc.CallParams[i].Name))
				}
			}
		}
	}

	if lhs != nil && lhs.Kind == ast.KindIdentifier {
		target := lhs.AsIdentifier().Text
		if len(rtlDesc.ReturnParams) == 1 {
			r.emitVariableMap(rtlDesc.ReturnParams[0], target, false)
		} else {
			panic(fmt.Sprintf("Expected single return from %s", name))
		}
	} else if len(rtlDesc.ReturnParams) > 0 {
		panic(fmt.Sprintf("Module %s with return must be used in declaration", name))
	}

	fmt.Fprint(r.OutFile, ");\n\n")
}

func (r *RtlProcGen) emitVariableMap(formal *RtlIo, actual string, first bool) {
	if io, ok := r.Interfaces[actual]; ok {
		r.emitInterfaceMap(formal, actual, io, first)
	} else if bundle, ok := r.Bundles[actual]; ok {
		r.emitBundleMap(formal, actual, bundle, first)
	} else {
		panic(fmt.Sprintf("Unexpected variable %s in module call", actual))
	}
}

func (r *RtlProcGen) emitInterfaceMap(formal *RtlIo, actual string, actualType RtlIoType, first bool) {
	switch at := actualType.(type) {
	case RtlSignalIo:
		if !first {
			fmt.Fprint(r.OutFile, ",")
		}
		fmt.Fprintf(r.OutFile, "\n    .%s(%s)", formal.Name, actual)
	case RtlClassIo:
		fclass := formal.Type.(RtlClassIo)
		for i, fsig := range fclass.Signals {
			asig := at.Signals[i]
			if i > 0 || !first {
				fmt.Fprint(r.OutFile, ",")
			}
			fmt.Fprintf(r.OutFile, "\n    .%s_%s(%s_%s)", formal.Name, fsig.Name, actual, asig.Name)
		}
	default:
		panic("Unsupported interface map kind")
	}
}

func (r *RtlProcGen) emitBundleMap(formal *RtlIo, actual string, actualType map[string]RtlIoType, first bool) {
	fbundle := formal.Type.(RtlBundleIo)
	for _, member := range fbundle.Members {
		aio, ok := actualType[member.Name]
		if !ok {
			panic(fmt.Sprintf("Missing bundle member %s in actual %s", member.Name, actual))
		}
		switch aio := aio.(type) {
		case RtlSignalIo:
			if !first {
				fmt.Fprint(r.OutFile, ",")
			}
			fmt.Fprintf(r.OutFile, "\n    .%s_%s(%s_%s)", formal.Name, member.Name, actual, member.Name)
		case RtlClassIo:
			for _, io := range aio.Signals {
				if !first {
					fmt.Fprint(r.OutFile, ",")
				}
				fmt.Fprintf(r.OutFile, "\n    .%s_%s_%s(%s_%s_%s)", formal.Name, member.Name, io.Name, actual, member.Name, io.Name)
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
		ty := r.Tc.GetTypeAtLocation(expr)
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
				typ := r.Tc.GetTypeAtLocation(obj)
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

	case ast.KindNumericLiteral:
		if expr.Kind == ast.KindStringLiteral {
			return expr.Text()
		}
		var desc *checker.TypeDescriptor
		if tt != nil {
			desc = checker.GetTypeDescriptor(r.Tc, tt, expr, false)
		} else {
			desc = checker.GetTypeDescriptor(r.Tc, r.Tc.GetTypeAtLocation(expr), expr, false)
		}
		if desc != nil {
			return buildConstant(desc, expr)
		}

		value := checker.GetConstExpression(expr)
		if num, ok := value.(jsnum.Number); ok {
			f := float64(num)
			if decimal {
				return fmt.Sprintf("%f", f)
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
	ast.KindPlusToken:                    "+",
	ast.KindMinusToken:                   "-",
	ast.KindAsteriskToken:                "*",
	ast.KindSlashToken:                   "/",
	ast.KindPercentToken:                 "%",
	ast.KindAmpersandToken:               "&",
	ast.KindBarToken:                     "|",
	ast.KindCaretToken:                   "^",
	ast.KindExclamationToken:             "!",
	ast.KindTildeToken:                   "~",
}

func buildConstant(desc *checker.TypeDescriptor, expr *ast.Node) string {
	value := checker.GetConstExpression(expr)
	if value == nil {
		panic("buildConstant: expression did not evaluate to constant")
	}

	switch v := value.(type) {
	case jsnum.Number:
		n := float64(v)
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
			op := pa.Text()
			if ast.IsIdentifier(pa.Expression) {
				name := pa.Expression.AsIdentifier().Text
				typ := r.Tc.GetTypeAtLocation(pa.Expression)
				if op == "is" && (isSignalType(typ) || isPortType(typ)) {
					sig, ok := r.Signals[name]
					if ok {
						if len(call.Arguments.Nodes) == 2 {
							cond = call.Arguments.Nodes[1]
							assert(ast.IsArrowFunction(cond))
							cond = cond.AsArrowFunction().Body
						}
						sig.Drivers = append(sig.Drivers, &Driver{
							Expr:    call.Arguments.Nodes[0],
							Cond:    cond,
							Delayed: false,
						})
					}
				} else if op == "write" && isBlockRamType(typ) {
					sig, ok := r.Signals[name]
					if ok {
						if len(call.Arguments.Nodes) == 3 {
							cond = call.Arguments.Nodes[2]
						}
						sig.Drivers = append(sig.Drivers, &Driver{
							Addr:    call.Arguments.Nodes[0],
							Expr:    call.Arguments.Nodes[1],
							Cond:    cond,
							Delayed: true,
						})
					}
				}
			} else if ast.IsPropertyAccessExpression(pa.Expression) {
				pa0 := fun.AsPropertyAccessExpression()
				op0 := pa0.Text()
				if ast.IsIdentifier(pa0.Expression) {
					name := pa0.Expression.AsIdentifier().Text
					if op0 == "next" && op == "is" {
						sig, ok := r.Signals[name]
						if ok {
							if len(call.Arguments.Nodes) == 2 {
								cond = call.Arguments.Nodes[1]
								assert(ast.IsArrowFunction(cond))
								cond = cond.AsArrowFunction().Body
							}
							sig.Drivers = append(sig.Drivers, &Driver{
								Expr:    call.Arguments.Nodes[0],
								Cond:    cond,
								Delayed: true,
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
		if td.Kind == ast.KindUnionType {
			enumType := td.Type.AsUnionTypeNode()
			enums := []string{}
			for _, t := range enumType.Types.Nodes {
				if ast.IsLiteralTypeNode(t) && ast.IsLiteralExpression(t.AsLiteralTypeNode().Literal) {
					enums = append(enums, t.AsLiteralTypeNode().Literal.Text())
				}
			}
			fmt.Fprintf(r.OutFile, "  typedef enum {%s} %s_t;\n\n", strings.Join(enums, ", "), td.Text())
		}
	}
}

func (r *RtlProcGen) emitVariableDeclarationListVariables(lst *ast.VariableDeclarationList) {
	for _, decl := range lst.Declarations.Nodes {
		if !ast.IsIdentifier(decl.Name()) {
			continue // skip non-simple bindings for now
		}

		name := r.Symtab.GenName(decl.AsVariableDeclaration())
		typ := r.Tc.GetTypeAtLocation(decl)

		if isSignalType(typ) && len(checker.ResolvedTypeArguments(typ)) > 0 {
			elem := checker.ResolvedTypeArguments(typ)[0]
			if elem.Flags()&checker.TypeFlagsUnion != 0 {
				fmt.Fprintf(r.OutFile, "  %s_t %s;\n", checker.GetReferenceTypeSymbol(elem).Name, name)
			} else {
				desc := checker.RequireTypeDescriptor(r.Tc, elem, decl.Initializer())
				r.Signals[name] = &SignalInfo{Name: name, Desc: desc, Drivers: []*Driver{}}
				rng := ""
				if desc.Width > 1 {
					rng = fmt.Sprintf(" [%d:0]", desc.Width-1)
				}
				fmt.Fprintf(r.OutFile, "  logic%s %s;\n", rng, name)
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

			desc := checker.RequireTypeDescriptor(r.Tc, elem, init)

			rng := ""
			if desc.Width > 1 {
				rng = fmt.Sprintf(" [%d:0]", desc.Width-1)
			}

			r.Signals[name] = &SignalInfo{Name: name, Desc: desc, Depth: &depth, Drivers: []*Driver{}}
			fmt.Fprintf(r.OutFile, "  logic%s %s[%d];\n", rng, name, depth)
		} else if isStreamType(typ) {
			io, ok := getPortType("client", typ, r.Tc, false)
			assert(ok, "Unspected logic module IO type")
			r.emitIoSignals(name, io)
			r.Interfaces[name] = io
		} else if checker.IsObjectType(typ) {
			bundle, ok := buildBundle(typ, decl, r.Tc)
			if ok {
				r.Bundles[name] = bundle
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
		if t.Desc.Width > 1 {
			rng = fmt.Sprintf(" [%d:0]", t.Desc.Width-1)
		}
		fmt.Fprintf(r.OutFile, "  logic%s %s;\n", rng, name)
	case RtlClassIo:
		for _, sig := range t.Signals {
			r.emitIoSignals(fmt.Sprintf("%s_%s", name, sig.Name), sig.Type)
		}
	default:
		panic("Unhandled IO signal type")
	}
}

type SignalInfo struct {
	Name    string
	Desc    *checker.TypeDescriptor
	Drivers []*Driver
	Depth   *uint32
	Init    *ast.Node
}

type Driver struct {
	Expr    *ast.Node
	Addr    *ast.Node
	Cond    *ast.Node
	State   []string
	Delayed bool
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
	Name         string
	CallParams   []*RtlIo
	ReturnParams []*RtlIo
}

type RtlIo struct {
	Name string
	Type RtlIoType
}

type RtlIoType interface {
	isRtlIoType()
}

// Signal

type RtlSignalIo struct {
	Kind string // "signal"
	Mode PortMode
	Desc *checker.TypeDescriptor
}

func (RtlSignalIo) isRtlIoType() {}

// Class

type RtlClassIo struct {
	Kind    string // "class"
	Name    string
	Signals []*RtlIo
	Proto   *IoInterface
}

func (RtlClassIo) isRtlIoType() {}

// Bundle

type RtlBundleIo struct {
	Kind    string // "bundle"
	Members []*RtlIo
}

func (RtlBundleIo) isRtlIoType() {}

type IRtlPort struct {
	Name string
	Mode PortMode
	Desc *checker.TypeDescriptor
}

type IoInterface struct {
	Params []*checker.TypeDescriptor
	Cls    *IoInterfaceClass
}

type IoInterfaceClass struct {
	Name    string
	Hls     *HlsInterface
	Signals []string
}

type HlsInterface struct {
	Connector string
	Role      string
	Aliases   map[string]string
}

func flattenIos(ios []*RtlIo) []*IRtlPort {
	var flat []*IRtlPort
	for _, io := range ios {
		flat = append(flat, flattenIo(io)...)
	}
	return flat
}

func flattenIo(io *RtlIo) []*IRtlPort {
	switch typ := io.Type.(type) {
	case RtlSignalIo:
		return []*IRtlPort{{
			Name: io.Name,
			Mode: typ.Mode,
			Desc: typ.Desc,
		}}
	case RtlClassIo:
		var ports []*IRtlPort
		for _, sig := range typ.Signals {
			for _, p := range flattenIo(sig) {
				p.Name = io.Name + "_" + p.Name
				ports = append(ports, p)
			}
		}
		return ports
	case RtlBundleIo:
		var ports []*IRtlPort
		for _, member := range typ.Members {
			for _, p := range flattenIo(member) {
				p.Name = io.Name + "_" + p.Name
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
				members = append(members, &RtlIo{Name: name, Type: typ})
			}
			return RtlBundleIo{
				Kind:    "bundle",
				Members: members,
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
			Kind: "signal",
			Mode: mode,
			Desc: desc,
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
					{Name: "valid", Type: RtlSignalIo{"signal", Input, &checker.TypeDescriptor{IsSigned: false, Width: 1}}},
					{Name: "accept", Type: RtlSignalIo{"signal", Output, &checker.TypeDescriptor{IsSigned: false, Width: 1}}},
					{Name: "data", Type: RtlSignalIo{"signal", Input, desc}},
				}
			} else {
				signals = []*RtlIo{
					{Name: "valid", Type: RtlSignalIo{"signal", Output, &checker.TypeDescriptor{IsSigned: false, Width: 1}}},
					{Name: "accept", Type: RtlSignalIo{"signal", Input, &checker.TypeDescriptor{IsSigned: false, Width: 1}}},
					{Name: "data", Type: RtlSignalIo{"signal", Output, desc}},
				}
			}
		}

		return RtlClassIo{
			Kind:    "class",
			Name:    sym,
			Signals: signals,
			Proto: &IoInterface{
				Params: []*checker.TypeDescriptor{desc},
				Cls:    cls,
			},
		}, true
	}

	return nil, false
}

func getHlsSignals(cls *IoInterfaceClass, args []*checker.TypeDescriptor) []*RtlIo {
	if cls.Hls == nil {
		panic("not an HLS interface")
	}
	conn := hlsConnectors[cls.Hls.Connector]
	if conn == nil {
		panic("missing connector: " + cls.Hls.Connector)
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
		if !contains(meth.Roles, cls.Hls.Role) {
			continue
		}

		// method-level IOs
		req := &RtlIo{
			Name: meth.Name + "_req",
			Type: RtlSignalIo{
				Kind: "signal",
				Mode: Output,
				Desc: &checker.TypeDescriptor{IsSigned: false, Width: 1},
			},
		}
		signalMap[mapName(req.Name, cls.Hls.Aliases)] = req

		if meth.Ready != nil && *meth.Ready != 0 {
			rdy := &RtlIo{
				Name: meth.Name + "_rdy",
				Type: RtlSignalIo{
					Kind: "signal",
					Mode: Input,
					Desc: &checker.TypeDescriptor{IsSigned: false, Width: 1},
				},
			}
			signalMap[mapName(rdy.Name, cls.Hls.Aliases)] = rdy
		}

		if meth.Cycles != nil && strings.Contains(*meth.Cycles, "*") {
			ack := &RtlIo{
				Name: meth.Name + "_ack",
				Type: RtlSignalIo{
					Kind: "signal",
					Mode: Input,
					Desc: &checker.TypeDescriptor{IsSigned: false, Width: 1},
				},
			}
			signalMap[mapName(ack.Name, cls.Hls.Aliases)] = ack
		}

		for _, param := range meth.Parameters {
			ptype := param.Type
			if t, ok := typeMap[ptype]; ok {
				ptype = t
			}
			signal := &RtlIo{
				Name: param.Name,
				Type: RtlSignalIo{
					Kind: "signal",
					Mode: Output,
					Desc: parseTypeSpec(ptype),
				},
			}
			signalMap[mapName(signal.Name, cls.Hls.Aliases)] = signal
		}

		if meth.Type != "void" {
			rtype := meth.Type
			if t, ok := typeMap[rtype]; ok {
				rtype = t
			}
			ret := &RtlIo{
				Name: meth.Name + "_r_e_t_u_r_n",
				Type: RtlSignalIo{
					Kind: "signal",
					Mode: Input,
					Desc: parseTypeSpec(rtype),
				},
			}
			signalMap[mapName(ret.Name, cls.Hls.Aliases)] = ret
		}
	}

	signals := make([]*RtlIo, len(cls.Signals))
	for i, name := range cls.Signals {
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
