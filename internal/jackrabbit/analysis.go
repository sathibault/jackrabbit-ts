package jackrabbit

import (
	"fmt"
	"os"
	"strings"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
)

var sourceAnalysis = make(map[string]*SourceDescriptor)

type SourceDescriptor struct {
	SourceName  string
	FuncNames   map[string]struct{}
	MethodNames map[string]struct{}
	GlobalUses  map[string]map[string]struct{}      // module type -> names
	ExternUses  map[string]struct{}                 // our globals referenced externally
	FileRefs    map[string]map[string]struct{}      // module type -> filenames
	VarDefs     map[string]*ast.VariableDeclaration // name -> declaration
	ModuleTypes map[string]struct{}
}

func NewSourceDescriptor(sourceName string) *SourceDescriptor {
	return &SourceDescriptor{
		SourceName:  sourceName,
		FuncNames:   make(map[string]struct{}),
		MethodNames: make(map[string]struct{}),
		GlobalUses: map[string]map[string]struct{}{
			"main":  {},
			"logic": {},
			"hls":   {},
			"test":  {},
		},
		ExternUses: make(map[string]struct{}),
		FileRefs: map[string]map[string]struct{}{
			"main":  {},
			"logic": {},
			"hls":   {},
			"test":  {},
		},
		VarDefs:     make(map[string]*ast.VariableDeclaration),
		ModuleTypes: make(map[string]struct{}),
	}
}

func (sd *SourceDescriptor) HasModuleType(t string) bool {
	_, ok := sd.ModuleTypes[t]
	return ok
}

func (sd *SourceDescriptor) LocalFunctionList() []string {
	var funcs []string
	for name := range sd.FuncNames {
		funcs = append(funcs, name)
	}
	for name := range sd.MethodNames {
		funcs = append(funcs, name)
	}

	prefix := sd.SourceName + "::"
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

func GetSourceDescriptor(sourceFile *ast.SourceFile) *SourceDescriptor {
	if sd, ok := sourceAnalysis[sourceFile.FileName()]; ok {
		return sd
	}
	return nil
}

func AnalyzeSourceFile(
	sourceFile *ast.SourceFile,
	checker *checker.Checker,
) {
	sd := NewSourceDescriptor(sourceFile.FileName())
	sourceAnalysis[sourceFile.FileName()] = sd

	globalVisitor := func(node *ast.Node) bool {
		if ast.IsVariableStatement(node) {
			stmt := node.AsVariableStatement()
			lst := stmt.DeclarationList.AsVariableDeclarationList()
			for _, decl := range lst.Declarations.Nodes {
				varDecl := decl.AsVariableDeclaration()
				name := varDecl.Symbol.Name
				sd.VarDefs[name] = varDecl
			}
		}
		return false
	}

	sourceFile.ForEachChild(globalVisitor)

	analyzer := NewAnalysis(sourceFile, checker, sd)

	if strings.Contains(sourceFile.FileName(), "jpeg") {
		fmt.Fprintf(os.Stderr, "========== %s %d\n", sourceFile.FileName(), len(sourceFile.Statements.Nodes))
	}

	sourceFile.ForEachChild(analyzer.Visit)

	for {
		dirty := false
		var funcs []string
		for name := range sd.FuncNames {
			funcs = append(funcs, name)
		}
		for name := range sd.MethodNames {
			funcs = append(funcs, name)
		}

		for _, name := range funcs {
			if functionAnalysis[name].Dirty {
				dirty = true
			}
			functionAnalysis[name].Dirty = false
		}

		if dirty {
			sourceFile.ForEachChild(analyzer.Visit)
		} else {
			break
		}
	}

	for name := range sd.FuncNames {
		functionAnalysis[name].FinalizeAnalysis()
	}
	for name := range sd.MethodNames {
		functionAnalysis[name].FinalizeAnalysis()
	}
}

// Need all functions before flow analysis
func AnalyzeSourcePass2(
	sourceFile *ast.SourceFile,
	checker *checker.Checker,
) {
	source := sourceAnalysis[sourceFile.FileName()]
	funcs := make([]string, 0, len(source.FuncNames)+len(source.MethodNames))
	for name := range source.FuncNames {
		funcs = append(funcs, name)
	}
	for name := range source.MethodNames {
		funcs = append(funcs, name)
	}
	for _, method := range funcs {
		if analysis, ok := functionAnalysis[method]; ok {
			analysis.FlowAnalysis(checker)
		}
	}
}

func FinalizeAnalysis() {
	// Run resolveShapes
	for _, source := range sourceAnalysis {
		first := true
		funcs := make([]string, 0, len(source.FuncNames)+len(source.MethodNames))
		for name := range source.FuncNames {
			funcs = append(funcs, name)
		}
		for name := range source.MethodNames {
			funcs = append(funcs, name)
		}
		for _, method := range funcs {
			if analysis, ok := functionAnalysis[method]; ok {
				analysis.ResolveShapes()
				if analysis.ModuleType != nil || len(analysis.ModuleUsers) > 0 {
					if first {
						fmt.Fprintf(os.Stderr, "%s:\n", source.SourceName)
						first = false
					}
					if analysis.ModuleType != nil {
						fmt.Fprintf(os.Stderr, "\t%s: %s %s\n", method, *analysis.ModuleType, strings.Join(analysis.ModuleUsers, ", "))
					} else {
						fmt.Fprintf(os.Stderr, "\t%s: nil %s\n", method, strings.Join(analysis.ModuleUsers, ", "))
					}
				}
			}
		}
	}

	// Transfer global usage from use files to def files
	for _, source := range sourceAnalysis {
		funcs := make([]string, 0, len(source.FuncNames)+len(source.MethodNames))
		for name := range source.FuncNames {
			funcs = append(funcs, name)
		}
		for name := range source.MethodNames {
			funcs = append(funcs, name)
		}
		for _, method := range funcs {
			if analysis, ok := functionAnalysis[method]; ok {
				for fileName, refs := range analysis.GlobalRefs {
					if target, exists := sourceAnalysis[fileName]; exists {
						if analysis.ModuleType != nil && *analysis.ModuleType != "logic" {
							extendSetMap(target.GlobalUses, *analysis.ModuleType, refs)
						}
						for _, moduleType := range analysis.ModuleUsers {
							extendSetMap(target.GlobalUses, moduleType, refs)
						}
					}
				}
			}
		}
	}

	// Collect all module types for source files
	for sourceFile, source := range sourceAnalysis {
		for moduleType, uses := range source.GlobalUses {
			if len(uses) > 0 {
				source.ModuleTypes[moduleType] = struct{}{}
			}
		}

		funcs := make([]string, 0, len(source.FuncNames)+len(source.MethodNames))
		for name := range source.FuncNames {
			funcs = append(funcs, name)
		}
		for name := range source.MethodNames {
			funcs = append(funcs, name)
		}
		for _, method := range funcs {
			if analysis, ok := functionAnalysis[method]; ok {
				if analysis.ModuleType != nil {
					source.ModuleTypes[*analysis.ModuleType] = struct{}{}
				}
				for _, moduleType := range analysis.ModuleUsers {
					source.ModuleTypes[moduleType] = struct{}{}
				}

				for fileName := range analysis.GlobalRefs {
					if fileName != sourceFile {
						if analysis.ModuleType != nil {
							addToSetMap(source.FileRefs, *analysis.ModuleType, fileName)
						}
						for _, moduleType := range analysis.ModuleUsers {
							addToSetMap(source.FileRefs, moduleType, fileName)
						}
					}
				}
			}
		}
	}
}

type Analysis struct {
	SourceFile       *ast.SourceFile
	Checker          *checker.Checker
	SourceDescriptor *SourceDescriptor
}

func NewAnalysis(
	sourceFile *ast.SourceFile,
	checker *checker.Checker,
	sourceDescriptor *SourceDescriptor,
) *Analysis {
	return &Analysis{
		SourceFile:       sourceFile,
		Checker:          checker,
		SourceDescriptor: sourceDescriptor,
	}
}

func (a *Analysis) Visit(node *ast.Node) bool {
	if ast.IsClassDeclaration(node) {
		classDecl := node.AsClassDeclaration()
		if classDecl.Name() != nil {
			analyzer := NewClassAnalysis(node, a.SourceFile, a.Checker, a.SourceDescriptor)
			node.ForEachChild(analyzer.Visit)
			return false
		}
	} else if ast.IsFunctionDeclaration(node) {
		funcDecl := node.AsFunctionDeclaration()
		if funcDecl.Name() != nil {
			qualified := checker.QualifiedFuncName(funcDecl)
			method := funcDecl.Name().Text()
			if _, ok := functionAnalysis[*qualified]; !ok {
				a.SourceDescriptor.FuncNames[*qualified] = struct{}{}
				functionAnalysis[*qualified] = newFunctionDescriptor(method, node, a.Checker)

				globals := NewGlobalsAnalysis(functionAnalysis[*qualified], a.Checker)
				node.ForEachChild(globals.Visit)
			}

			analyzer := NewFunctionAnalysis(functionAnalysis[*qualified], node, a.SourceFile, a.Checker)
			node.ForEachChild(analyzer.Visit)
			return false
		}
	}
	node.ForEachChild(a.Visit)
	return false
}

type ClassAnalysis struct {
	ClassName        string
	QualifiedClass   string
	SourceFile       *ast.SourceFile
	Checker          *checker.Checker
	SourceDescriptor *SourceDescriptor
}

func NewClassAnalysis(
	node *ast.Node, // Should be a ClassDeclaration
	sourceFile *ast.SourceFile,
	tc *checker.Checker,
	sourceDescriptor *SourceDescriptor,
) *ClassAnalysis {
	classDecl := node.AsClassDeclaration()
	className := classDecl.Name().Text()
	qualifiedClass := *checker.QualifiedClassName(classDecl)

	return &ClassAnalysis{
		ClassName:        className,
		QualifiedClass:   qualifiedClass,
		SourceFile:       sourceFile,
		Checker:          tc,
		SourceDescriptor: sourceDescriptor,
	}
}

func (ca *ClassAnalysis) Visit(node *ast.Node) bool {
	if ast.IsMethodDeclaration(node) {
		method := node.AsMethodDeclaration()
		name := method.Name().Text()
		qualified := ca.QualifiedClass + "." + name
		methodKey := ca.ClassName + "." + name

		if _, ok := functionAnalysis[qualified]; !ok {
			ca.SourceDescriptor.MethodNames[qualified] = struct{}{}
			functionAnalysis[qualified] = newFunctionDescriptor(methodKey, node, ca.Checker)

			globals := NewGlobalsAnalysis(functionAnalysis[qualified], ca.Checker)
			node.ForEachChild(globals.Visit)
		}

		analyzer := NewFunctionAnalysis(functionAnalysis[qualified], node, ca.SourceFile, ca.Checker)
		node.ForEachChild(analyzer.Visit)
		return false
	}
	node.ForEachChild(ca.Visit)
	return false
}

type FunctionAnalysis struct {
	SourceFile *ast.SourceFile
	Checker    *checker.Checker
	Descriptor *FunctionDescriptor
	dfaDepth   int
}

func NewFunctionAnalysis(
	descriptor *FunctionDescriptor,
	node *ast.Node,
	sourceFile *ast.SourceFile,
	checker *checker.Checker,
) *FunctionAnalysis {
	fa := &FunctionAnalysis{
		Descriptor: descriptor,
		SourceFile: sourceFile,
		Checker:    checker,
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
									fa.Descriptor.ModuleType = &arg.Text
									fmt.Println("MODULE", fa.Descriptor.MethodName, fa.Descriptor.ModuleType)
								} else {
									panic("Module type must be string literal")
								}
							} else if id.Text == "inline" {
								fa.Descriptor.Inlined = true
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
	if fa.Descriptor.ModuleType != nil && *fa.Descriptor.ModuleType == "logic" {
		if call := node.AsCallExpression(); call != nil {
			callee := getCalleeDescriptor(call, fa.Checker)
			if callee != nil && callee.ModuleType != nil && *callee.ModuleType != "test" {
				callee.UpdateUsers(fa.Descriptor)
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
				if !fa.Descriptor.NeedAsync {
					fa.Descriptor.NeedAsync = true
					fa.Descriptor.Dirty = true
					fmt.Println(fa.Descriptor.MethodName, "+ async")
				}
			}
		}

		if callee := getCalleeDescriptor(call, fa.Checker); callee != nil {
			if callee.ModuleType != nil && *callee.ModuleType != "test" {
				callee.UpdateUsers(fa.Descriptor)
			}

			if callee.ModuleType != nil && !contains(fa.Descriptor.CalledTypes, *callee.ModuleType) {
				fa.Descriptor.CalledTypes = append(fa.Descriptor.CalledTypes, *callee.ModuleType)
			}

			if fa.Descriptor.ModuleType != nil {
				if *fa.Descriptor.ModuleType != "logic" || (*callee.ModuleType != "hls" && *callee.ModuleType != "test") {
					if !fa.Descriptor.NeedAsync && callee.NeedAsync {
						fa.Descriptor.NeedAsync = true
						fa.Descriptor.Dirty = true
						fmt.Println(fa.Descriptor.MethodName, "+ async via", callee.MethodName)
						if *fa.Descriptor.ModuleType == "logic" {
							panic(fmt.Sprintf("Unexpected async call %s in logic module %s", callee.MethodName, fa.Descriptor.MethodName))
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
					if ga.Descriptor.GlobalRefs[*module] == nil {
						ga.Descriptor.GlobalRefs[*module] = map[string]struct{}{}
					}
					ga.Descriptor.GlobalRefs[*module][node.AsIdentifier().Text] = struct{}{}
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
