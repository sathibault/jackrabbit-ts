package jackrabbit

import (
	"fmt"
	"os"
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
)

type SourceDescriptor struct {
	sourceName  string
	funcNames   map[string]struct{}
	methodNames map[string]struct{}
	globalUses  map[string]map[string]struct{}      // module type -> names
	externUses  map[string]struct{}                 // our globals referenced externally
	fileRefs    map[string]map[string]struct{}      // module type -> filenames
	varDefs     map[string]*ast.VariableDeclaration // name -> declaration
	moduleTypes map[string]struct{}
}

func NewSourceDescriptor(sourceName string) *SourceDescriptor {
	return &SourceDescriptor{
		sourceName:  sourceName,
		funcNames:   make(map[string]struct{}),
		methodNames: make(map[string]struct{}),
		globalUses: map[string]map[string]struct{}{
			"main":  {},
			"logic": {},
			"hls":   {},
			"test":  {},
		},
		externUses: make(map[string]struct{}),
		fileRefs: map[string]map[string]struct{}{
			"main":  {},
			"logic": {},
			"hls":   {},
			"test":  {},
		},
		varDefs:     make(map[string]*ast.VariableDeclaration),
		moduleTypes: make(map[string]struct{}),
	}
}

func (sd *SourceDescriptor) HasModuleType(t string) bool {
	_, ok := sd.moduleTypes[t]
	return ok
}

func (sd *SourceDescriptor) LocalFunctionList() []string {
	var funcs []string
	for name := range sd.funcNames {
		funcs = append(funcs, name)
	}
	for name := range sd.methodNames {
		funcs = append(funcs, name)
	}

	prefix := sd.sourceName + "::"
	var stripped []string
	for _, name := range funcs {
		if len(name) >= len(prefix) && name[:len(prefix)] == prefix {
			stripped = append(stripped, name[len(prefix):])
		} else {
			stripped = append(stripped, name)
		}
	}
	return stripped
}

func AnalyzeSourceFile(
	den *RabbitDen,
	sourceFile *ast.SourceFile,
	tc *checker.Checker,
) {
	sd := NewSourceDescriptor(sourceFile.FileName())
	den.sourceAnalysis[sourceFile.FileName()] = sd

	globalVisitor := func(node *ast.Node) bool {
		if ast.IsVariableStatement(node) {
			stmt := node.AsVariableStatement()
			lst := stmt.DeclarationList.AsVariableDeclarationList()
			for _, decl := range lst.Declarations.Nodes {
				varDecl := decl.AsVariableDeclaration()
				name := varDecl.Symbol.Name
				sd.varDefs[name] = varDecl
			}
		}
		return false
	}

	sourceFile.ForEachChild(globalVisitor)

	analyzer := NewAnalysis(den, sourceFile, tc, sd)

	if strings.Contains(sourceFile.FileName(), "jpeg") {
		fmt.Fprintf(os.Stderr, "========== %s %d\n", sourceFile.FileName(), len(sourceFile.Statements.Nodes))
	}

	sourceFile.ForEachChild(analyzer.Visit)

	for {
		dirty := false
		var funcs []string
		for name := range sd.funcNames {
			funcs = append(funcs, name)
		}
		for name := range sd.methodNames {
			funcs = append(funcs, name)
		}

		for _, name := range funcs {
			if den.functionAnalysis[name].dirty {
				dirty = true
			}
			den.functionAnalysis[name].dirty = false
		}

		if dirty {
			sourceFile.ForEachChild(analyzer.Visit)
		} else {
			break
		}
	}

	for name := range sd.funcNames {
		den.functionAnalysis[name].FinalizeAnalysis()
	}
	for name := range sd.methodNames {
		den.functionAnalysis[name].FinalizeAnalysis()
	}
}

// Need all functions before flow analysis
func AnalyzeSourcePass2(
	den *RabbitDen,
	sourceFile *ast.SourceFile,
	checker *checker.Checker,
) {
	source := den.sourceAnalysis[sourceFile.FileName()]
	funcs := make([]string, 0, len(source.funcNames)+len(source.methodNames))
	for name := range source.funcNames {
		funcs = append(funcs, name)
	}
	for name := range source.methodNames {
		funcs = append(funcs, name)
	}
	for _, method := range funcs {
		if analysis, ok := den.functionAnalysis[method]; ok {
			analysis.FlowAnalysis(den, checker)
		}
	}
}

func FinalizeAnalysis(den *RabbitDen) {
	// Run resolveShapes
	for _, source := range den.sourceAnalysis {
		first := true
		funcs := make([]string, 0, len(source.funcNames)+len(source.methodNames))
		for name := range source.funcNames {
			funcs = append(funcs, name)
		}
		for name := range source.methodNames {
			funcs = append(funcs, name)
		}
		for _, method := range funcs {
			if analysis, ok := den.functionAnalysis[method]; ok {
				analysis.ResolveShapes()
				if analysis.moduleType != nil || len(analysis.moduleUsers) > 0 {
					if first {
						fmt.Fprintf(os.Stderr, "%s:\n", source.sourceName)
						first = false
					}
					if analysis.moduleType != nil {
						fmt.Fprintf(os.Stderr, "\t%s: %s %s\n", method, *analysis.moduleType, strings.Join(analysis.moduleUsers, ", "))
					} else {
						fmt.Fprintf(os.Stderr, "\t%s: nil %s\n", method, strings.Join(analysis.moduleUsers, ", "))
					}
				}
			}
		}
	}

	// Transfer global usage from use files to def files
	for _, source := range den.sourceAnalysis {
		funcs := make([]string, 0, len(source.funcNames)+len(source.methodNames))
		for name := range source.funcNames {
			funcs = append(funcs, name)
		}
		for name := range source.methodNames {
			funcs = append(funcs, name)
		}
		for _, method := range funcs {
			if analysis, ok := den.functionAnalysis[method]; ok {
				for fileName, refs := range analysis.globalRefs {
					if target, exists := den.sourceAnalysis[fileName]; exists {
						if analysis.moduleType != nil && *analysis.moduleType != "logic" {
							extendSetMap(target.globalUses, *analysis.moduleType, refs)
						}
						for _, moduleType := range analysis.moduleUsers {
							extendSetMap(target.globalUses, moduleType, refs)
						}
					}
				}
			}
		}
	}

	// Collect all module types for source files
	for sourceFile, source := range den.sourceAnalysis {
		for moduleType, uses := range source.globalUses {
			if len(uses) > 0 {
				source.moduleTypes[moduleType] = struct{}{}
			}
		}

		funcs := make([]string, 0, len(source.funcNames)+len(source.methodNames))
		for name := range source.funcNames {
			funcs = append(funcs, name)
		}
		for name := range source.methodNames {
			funcs = append(funcs, name)
		}
		for _, method := range funcs {
			if analysis, ok := den.functionAnalysis[method]; ok {
				if analysis.moduleType != nil {
					source.moduleTypes[*analysis.moduleType] = struct{}{}
				}
				for _, moduleType := range analysis.moduleUsers {
					source.moduleTypes[moduleType] = struct{}{}
				}

				for fileName := range analysis.globalRefs {
					if fileName != sourceFile {
						if analysis.moduleType != nil {
							addToSetMap(source.fileRefs, *analysis.moduleType, fileName)
						}
						for _, moduleType := range analysis.moduleUsers {
							addToSetMap(source.fileRefs, moduleType, fileName)
						}
					}
				}
			}
		}
	}
}

type Analysis struct {
	den              *RabbitDen
	sourceFile       *ast.SourceFile
	tc               *checker.Checker
	sourceDescriptor *SourceDescriptor
}

func NewAnalysis(
	den *RabbitDen,
	sourceFile *ast.SourceFile,
	checker *checker.Checker,
	sourceDescriptor *SourceDescriptor,
) *Analysis {
	return &Analysis{
		den:              den,
		sourceFile:       sourceFile,
		tc:               checker,
		sourceDescriptor: sourceDescriptor,
	}
}

func (a *Analysis) Visit(node *ast.Node) bool {
	if ast.IsClassDeclaration(node) {
		classDecl := node.AsClassDeclaration()
		if classDecl.Name() != nil {
			analyzer := NewClassAnalysis(a.den, node, a.sourceFile, a.tc, a.sourceDescriptor)
			node.ForEachChild(analyzer.Visit)
			return false
		}
	} else if ast.IsFunctionDeclaration(node) {
		funcDecl := node.AsFunctionDeclaration()
		if funcDecl.Name() != nil {
			qualified := checker.QualifiedFuncName(funcDecl)
			method := funcDecl.Name().Text()
			if _, ok := a.den.functionAnalysis[*qualified]; !ok {
				a.sourceDescriptor.funcNames[*qualified] = struct{}{}
				a.den.functionAnalysis[*qualified] = newFunctionDescriptor(method, node, a.tc)

				globals := NewGlobalsAnalysis(a.den.functionAnalysis[*qualified], a.tc)
				node.ForEachChild(globals.Visit)
			}

			analyzer := NewFunctionAnalysis(a.den, a.den.functionAnalysis[*qualified], node, a.sourceFile, a.tc)
			node.ForEachChild(analyzer.Visit)
			return false
		}
	}
	node.ForEachChild(a.Visit)
	return false
}

type ClassAnalysis struct {
	den              *RabbitDen
	ClassName        string
	qualifiedClass   string
	sourceFile       *ast.SourceFile
	tc               *checker.Checker
	sourceDescriptor *SourceDescriptor
}

func NewClassAnalysis(
	den *RabbitDen,
	node *ast.Node, // Should be a ClassDeclaration
	sourceFile *ast.SourceFile,
	tc *checker.Checker,
	sourceDescriptor *SourceDescriptor,
) *ClassAnalysis {
	classDecl := node.AsClassDeclaration()
	className := classDecl.Name().Text()
	qualifiedClass := *checker.QualifiedClassName(classDecl)

	return &ClassAnalysis{
		den:              den,
		ClassName:        className,
		qualifiedClass:   qualifiedClass,
		sourceFile:       sourceFile,
		tc:               tc,
		sourceDescriptor: sourceDescriptor,
	}
}

func (ca *ClassAnalysis) Visit(node *ast.Node) bool {
	if ast.IsMethodDeclaration(node) {
		method := node.AsMethodDeclaration()
		name := method.Name().Text()
		qualified := ca.qualifiedClass + "." + name
		methodKey := ca.ClassName + "." + name

		if _, ok := ca.den.functionAnalysis[qualified]; !ok {
			ca.sourceDescriptor.methodNames[qualified] = struct{}{}
			ca.den.functionAnalysis[qualified] = newFunctionDescriptor(methodKey, node, ca.tc)

			globals := NewGlobalsAnalysis(ca.den.functionAnalysis[qualified], ca.tc)
			node.ForEachChild(globals.Visit)
		}

		analyzer := NewFunctionAnalysis(ca.den, ca.den.functionAnalysis[qualified], node, ca.sourceFile, ca.tc)
		node.ForEachChild(analyzer.Visit)
		return false
	}
	node.ForEachChild(ca.Visit)
	return false
}

type FunctionAnalysis struct {
	den        *RabbitDen
	SourceFile *ast.SourceFile
	tc         *checker.Checker
	descriptor *FunctionDescriptor
	dfaDepth   int
}

func NewFunctionAnalysis(
	den *RabbitDen,
	descriptor *FunctionDescriptor,
	node *ast.Node,
	sourceFile *ast.SourceFile,
	checker *checker.Checker,
) *FunctionAnalysis {
	fa := &FunctionAnalysis{
		den:        den,
		descriptor: descriptor,
		SourceFile: sourceFile,
		tc:         checker,
		dfaDepth:   0,
	}

	if ast.IsFunctionDeclaration(node) {
		decl := node.AsFunctionDeclaration()
		if decl.Modifiers() != nil {
			for _, dec := range decl.ModifierNodes() {
				if ast.IsDecorator(dec) {
					d := dec.AsDecorator()
					if ast.IsCallExpression(d.Expression) {
						expr := d.Expression.AsCallExpression()
						if ast.IsIdentifier(expr.Expression) {
							id := expr.Expression.AsIdentifier()
							if id.Text == "module" && len(expr.Arguments.Nodes) == 1 {
								if ast.IsStringLiteral(expr.Arguments.Nodes[0]) {
									arg := expr.Arguments.Nodes[0].AsStringLiteral()
									fa.descriptor.moduleType = &arg.Text
									fmt.Fprintln(os.Stderr, "MODULE", fa.descriptor.methodName, fa.descriptor.moduleType)
								} else {
									panic("Module type must be string literal")
								}
							} else if id.Text == "inline" {
								fa.descriptor.inlined = true
							}
						}
					}
				}
			}
		}
	}

	return fa
}

func (fa *FunctionAnalysis) Visit(node *ast.Node) bool {
	if fa.descriptor.moduleType != nil && *fa.descriptor.moduleType == "logic" {
		if ast.IsCallExpression(node) {
			call := node.AsCallExpression()
			callee := fa.den.getCalleeDescriptor(call, fa.tc)
			if callee != nil && callee.moduleType != nil && *callee.moduleType != "test" {
				callee.UpdateUsers(fa.descriptor)
			}
		}
		return false
	}

	if ast.IsCallExpression(node) && fa.dfaDepth == 0 {
		call := node.AsCallExpression()
		if ast.IsIdentifier(call.Expression) {
			id := call.Expression.AsIdentifier()
			if ASYNC_RUNNERS[id.Text] {
				fa.dfaDepth++
				node.ForEachChild(func(n *ast.Node) bool {
					return fa.Visit(n)
				})
				fa.dfaDepth--
				return false
			}
			if id.Text == "waitFor" && len(call.Arguments.Nodes) == 1 {
				if !fa.descriptor.needAsync {
					fa.descriptor.needAsync = true
					fa.descriptor.dirty = true
					fmt.Fprintln(os.Stderr, fa.descriptor.methodName, "+ async")
				}
			}
		}

		if callee := fa.den.getCalleeDescriptor(call, fa.tc); callee != nil {
			if callee.moduleType != nil && *callee.moduleType != "test" {
				callee.UpdateUsers(fa.descriptor)
			}

			if callee.moduleType != nil && !contains(fa.descriptor.calledTypes, *callee.moduleType) {
				fa.descriptor.calledTypes = append(fa.descriptor.calledTypes, *callee.moduleType)
			}

			if fa.descriptor.moduleType != nil {
				if *fa.descriptor.moduleType != "logic" || (*callee.moduleType != "hls" && *callee.moduleType != "test") {
					if !fa.descriptor.needAsync && callee.needAsync {
						fa.descriptor.needAsync = true
						fa.descriptor.dirty = true
						fmt.Fprintln(os.Stderr, fa.descriptor.methodName, "+ async via", callee.methodName)
						if *fa.descriptor.moduleType == "logic" {
							panic(fmt.Sprintf("Unexpected async call %s in logic module %s", callee.methodName, fa.descriptor.methodName))
						}
					}
				}
			}
		}
	}

	node.ForEachChild(fa.Visit)
	return false
}

type GlobalsAnalysis struct {
	Descriptor *FunctionDescriptor
	Checker    *checker.Checker
}

func NewGlobalsAnalysis(descriptor *FunctionDescriptor, checker *checker.Checker) *GlobalsAnalysis {
	return &GlobalsAnalysis{
		Descriptor: descriptor,
		Checker:    checker,
	}
}

func (ga *GlobalsAnalysis) Visit(node *ast.Node) bool {
	if ast.IsIdentifier(node) {
		prnt := node.Parent
		if prnt != nil && !ast.IsVariableDeclaration(prnt) {
			sym := ga.Checker.GetSymbolAtLocation(node)
			sym = checker.GetDealiasedSymbol(ga.Checker, sym)

			if sym != nil && len(sym.Declarations) > 0 &&
				(sym.Flags&(ast.SymbolFlagsVariable|ast.SymbolFlagsEnum|ast.SymbolFlagsAlias)) != 0 {

				// module := checker.ResolveIdentifierModule(ga.Checker, node)
				var module *string = nil

				cur := sym.Declarations[0]
				for cur != nil {
					if ast.IsImportDeclaration(cur) {
						decl := cur.AsImportDeclaration()
						if ast.IsStringLiteral(decl.ModuleSpecifier) {
							module = &decl.ModuleSpecifier.AsStringLiteral().Text
						}
						break
					} else if ast.IsVariableDeclaration(cur) {
						container := getNodeContainer(cur)
						if ast.IsSourceFile(container) {
							file := container.AsSourceFile().FileName()
							module = &file
						}
						break
					}
					cur = cur.Parent
				}

				if module != nil {
					if ga.Descriptor.globalRefs[*module] == nil {
						ga.Descriptor.globalRefs[*module] = map[string]struct{}{}
					}
					ga.Descriptor.globalRefs[*module][node.AsIdentifier().Text] = struct{}{}
				}
			}
		}
	}

	node.ForEachChild(ga.Visit)
	return false
}

func getNodeContainer(node *ast.Node) *ast.Node {
	if ast.IsSourceFile(node) {
		panic("getNodeContainer called on SourceFile node")
	}

	for {
		node = node.Parent
		if node == nil {
			panic("getNodeContainer: reached root without finding container")
		}

		switch node.Kind {
		case ast.KindComputedPropertyName:
			if node.Parent != nil && node.Parent.Parent != nil && ast.IsClassLike(node.Parent.Parent) {
				return node
			}
			node = node.Parent

		case ast.KindDecorator:
			if node.Parent != nil && node.Parent.Kind == ast.KindParameter && ast.IsClassElement(node.Parent.Parent) {
				node = node.Parent.Parent
			} else if ast.IsClassElement(node.Parent) {
				node = node.Parent
			}

		case ast.KindArrowFunction,
			ast.KindFunctionDeclaration,
			ast.KindFunctionExpression,
			ast.KindModuleDeclaration,
			ast.KindClassStaticBlockDeclaration,
			ast.KindPropertyDeclaration,
			ast.KindPropertySignature,
			ast.KindMethodDeclaration,
			ast.KindMethodSignature,
			ast.KindConstructor,
			ast.KindGetAccessor,
			ast.KindSetAccessor,
			ast.KindCallSignature,
			ast.KindConstructSignature,
			ast.KindIndexSignature,
			ast.KindEnumDeclaration,
			ast.KindSourceFile:
			return node
		}
	}
}

func extendSetMap(m map[string]map[string]struct{}, key string, values map[string]struct{}) {
	if m[key] == nil {
		m[key] = make(map[string]struct{})
	}
	for v := range values {
		m[key][v] = struct{}{}
	}
}

func addToSetMap(m map[string]map[string]struct{}, key, value string) {
	if m[key] == nil {
		m[key] = make(map[string]struct{})
	}
	m[key][value] = struct{}{}
}
