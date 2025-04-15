package project

import (
	"fmt"
	"strings"
	"sync"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/collections"
	"github.com/microsoft/typescript-go/internal/compiler"
	"github.com/microsoft/typescript-go/internal/core"
	"github.com/microsoft/typescript-go/internal/jackrabbit"
	"github.com/microsoft/typescript-go/internal/ls"
	"github.com/microsoft/typescript-go/internal/tspath"
	"github.com/microsoft/typescript-go/internal/vfs"
)

//go:generate go tool golang.org/x/tools/cmd/stringer -type=Kind -output=project_stringer_generated.go

var projectNamer = &namer{}

var _ ls.Host = (*Project)(nil)

type Kind int

const (
	KindInferred Kind = iota
	KindConfigured
	KindAutoImportProvider
	KindAuxiliary
)

type Project struct {
	projectService *Service
	mu             sync.Mutex

	name string
	kind Kind

	initialLoadPending        bool
	dirty                     bool
	version                   int
	hasAddedOrRemovedFiles    bool
	hasAddedOrRemovedSymlinks bool
	deferredClose             bool
	reloadConfig              bool

	currentDirectory string
	// Inferred projects only
	rootPath tspath.Path

	configFileName string
	configFilePath tspath.Path
	// rootFileNames was a map from Path to { NormalizedPath, ScriptInfo? } in the original code.
	// But the ProjectService owns script infos, so it's not clear why there was an extra pointer.
	rootFileNames   *collections.OrderedMap[tspath.Path, string]
	compilerOptions *core.CompilerOptions
	languageService *ls.LanguageService
	program         *compiler.Program

	den             *jackrabbit.RabbitDen
	synthesisByPath map[tspath.Path]*jackrabbit.Synthesis
}

func NewConfiguredProject(configFileName string, configFilePath tspath.Path, projectService *Service) *Project {
	project := NewProject(configFileName, KindConfigured, tspath.GetDirectoryPath(configFileName), projectService)
	project.configFileName = configFileName
	project.configFilePath = configFilePath
	project.initialLoadPending = true
	return project
}

func NewInferredProject(compilerOptions *core.CompilerOptions, currentDirectory string, projectRootPath tspath.Path, projectService *Service) *Project {
	project := NewProject(projectNamer.next("/dev/null/inferredProject"), KindInferred, currentDirectory, projectService)
	project.rootPath = projectRootPath
	project.compilerOptions = compilerOptions
	return project
}

func NewProject(name string, kind Kind, currentDirectory string, projectService *Service) *Project {
	projectService.log(fmt.Sprintf("Creating %sProject: %s, currentDirectory: %s", kind.String(), name, currentDirectory))
	project := &Project{
		projectService:   projectService,
		name:             name,
		kind:             kind,
		currentDirectory: currentDirectory,
		rootFileNames:    &collections.OrderedMap[tspath.Path, string]{},
	}
	project.languageService = ls.NewLanguageService(project)
	project.markAsDirty()
	return project
}

// FS implements LanguageServiceHost.
func (p *Project) FS() vfs.FS {
	return p.projectService.host.FS()
}

// DefaultLibraryPath implements LanguageServiceHost.
func (p *Project) DefaultLibraryPath() string {
	return p.projectService.host.DefaultLibraryPath()
}

// GetCompilerOptions implements LanguageServiceHost.
func (p *Project) GetCompilerOptions() *core.CompilerOptions {
	return p.compilerOptions
}

// GetCurrentDirectory implements LanguageServiceHost.
func (p *Project) GetCurrentDirectory() string {
	return p.currentDirectory
}

// GetProjectVersion implements LanguageServiceHost.
func (p *Project) GetProjectVersion() int {
	return p.version
}

// GetRootFileNames implements LanguageServiceHost.
func (p *Project) GetRootFileNames() []string {
	fileNames := make([]string, 0, p.rootFileNames.Size())
	for path, fileName := range p.rootFileNames.Entries() {
		if p.projectService.getScriptInfo(path) != nil {
			fileNames = append(fileNames, fileName)
		}
	}
	return fileNames
}

// GetSourceFile implements LanguageServiceHost.
func (p *Project) GetSourceFile(fileName string, path tspath.Path, languageVersion core.ScriptTarget) *ast.SourceFile {
	scriptKind := p.getScriptKind(fileName)
	if scriptInfo := p.getOrCreateScriptInfoAndAttachToProject(fileName, scriptKind); scriptInfo != nil {
		var (
			oldSourceFile      *ast.SourceFile
			oldCompilerOptions *core.CompilerOptions
		)
		if p.program != nil {
			oldSourceFile = p.program.GetSourceFileByPath(scriptInfo.path)
			oldCompilerOptions = p.program.GetCompilerOptions()
		}
		return p.projectService.documentRegistry.acquireDocument(scriptInfo, p.GetCompilerOptions(), oldSourceFile, oldCompilerOptions)
	}
	return nil
}

// GetProgram implements LanguageServiceHost. Updates the program if needed.
func (p *Project) GetProgram() *compiler.Program {
	p.updateIfDirty()
	return p.program
}

// NewLine implements LanguageServiceHost.
func (p *Project) NewLine() string {
	return p.projectService.host.NewLine()
}

// Trace implements LanguageServiceHost.
func (p *Project) Trace(msg string) {
	p.log(msg)
}

// GetDefaultLibraryPath implements ls.Host.
func (p *Project) GetDefaultLibraryPath() string {
	return p.projectService.options.DefaultLibraryPath
}

func (p *Project) Name() string {
	return p.name
}

func (p *Project) Kind() Kind {
	return p.kind
}

func (p *Project) CurrentProgram() *compiler.Program {
	return p.program
}

func (p *Project) LanguageService() *ls.LanguageService {
	return p.languageService
}

func (p *Project) GetSynthesis(fileName string) *jackrabbit.Synthesis {
	path := tspath.ToPath(fileName, p.GetCurrentDirectory(), p.FS().UseCaseSensitiveFileNames())
	if p.synthesisByPath != nil {
		if s, ok := p.synthesisByPath[path]; ok {
			return s
		}
	}
	return nil
}

func (p *Project) GetOrCreateSynthesis(architecturesPath string, fileName string) *jackrabbit.Synthesis {
	path := tspath.ToPath(fileName, p.GetCurrentDirectory(), p.FS().UseCaseSensitiveFileNames())
	if p.synthesisByPath == nil {
		p.synthesisByPath = make(map[tspath.Path]*jackrabbit.Synthesis)
	}
	if _, ok := p.synthesisByPath[path]; !ok {
		p.synthesisByPath[path] = jackrabbit.NewSynthesis(architecturesPath)
	}
	return p.synthesisByPath[path]
}

func (p *Project) getDen() *jackrabbit.RabbitDen {
	if p.den == nil {
		p.den = jackrabbit.NewRabbitDen()
		p.program.GlobalAnalysis(p.den)
	}
	return p.den
}

func (p *Project) IsFunctionInHls(decl *ast.FunctionDeclaration) bool {
	desc := p.getDen().GetFunctionDeclDescriptor(decl)
	if desc != nil {
		return desc.InHlsSet()
	}
	return false
}

func (p *Project) GetHlsFunctionSummary(archPath string, fileName string, decl *ast.FunctionDeclaration, tc *checker.Checker) []jackrabbit.HlsBlockSummary {
	synthesis := p.GetOrCreateSynthesis(archPath, fileName)
	synthesis.EnsureHlsDescriptor(p.getDen(), decl, tc)
	desc := synthesis.GetHlsDescriptor(decl)
	return desc.GetSummary()
}

func (p *Project) getOrCreateScriptInfoAndAttachToProject(fileName string, scriptKind core.ScriptKind) *ScriptInfo {
	if scriptInfo := p.projectService.getOrCreateScriptInfoNotOpenedByClient(fileName, p.projectService.toPath(fileName), scriptKind); scriptInfo != nil {
		scriptInfo.attachToProject(p)
		return scriptInfo
	}
	return nil
}

func (p *Project) getScriptKind(fileName string) core.ScriptKind {
	// Customizing script kind per file extension is a common plugin / LS host customization case
	// which can probably be replaced with static info in the future
	return core.GetScriptKindFromFileName(fileName)
}

// for synthesis
func (p *Project) applyChangesToFile(path tspath.Path, edits []jackrabbit.TextEdit) {
	if p.synthesisByPath != nil {
		if s, ok := p.synthesisByPath[path]; ok {
			s.ApplyEdit(edits)
		}
	}
}

func (p *Project) markFileAsDirty(path tspath.Path) {
	p.markAsDirty()
}

func (p *Project) markAsDirty() {
	p.mu.Lock()
	defer p.mu.Unlock()
	if !p.dirty {
		p.dirty = true
		p.version++
	}
}

func (p *Project) updateIfDirty() bool {
	// !!! p.invalidateResolutionsOfFailedLookupLocations()
	return p.dirty && p.updateGraph()
}

func (p *Project) onFileAddedOrRemoved(isSymlink bool) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.hasAddedOrRemovedFiles = true
	if isSymlink {
		p.hasAddedOrRemovedSymlinks = true
	}
}

// updateGraph updates the set of files that contribute to the project.
// Returns true if the set of files in has changed. NOTE: this is the
// opposite of the return value in Strada, which was frequently inverted,
// as in `updateProjectIfDirty()`.
func (p *Project) updateGraph() bool {
	// !!!
	p.log("Starting updateGraph: Project: " + p.name)
	oldProgram := p.program
	hasAddedOrRemovedFiles := p.hasAddedOrRemovedFiles
	p.initialLoadPending = false

	if p.kind == KindConfigured && p.reloadConfig {
		p.projectService.loadConfiguredProject(p)
		p.reloadConfig = false
	}

	p.hasAddedOrRemovedFiles = false
	p.hasAddedOrRemovedSymlinks = false
	p.updateProgram()
	p.dirty = false
	p.log(fmt.Sprintf("Finishing updateGraph: Project: %s version: %d", p.name, p.version))
	if hasAddedOrRemovedFiles {
		p.log(p.print(true /*writeFileNames*/, true /*writeFileExplanation*/, false /*writeFileVersionAndText*/))
	} else if p.program != oldProgram {
		p.log("Different program with same set of files")
	}

	if p.program != oldProgram && oldProgram != nil {
		for _, oldSourceFile := range oldProgram.GetSourceFiles() {
			if p.program.GetSourceFileByPath(oldSourceFile.Path()) == nil {
				p.projectService.documentRegistry.releaseDocument(oldSourceFile, oldProgram.GetCompilerOptions())
			}
		}
	}

	return true
}

func (p *Project) updateProgram() {
	rootFileNames := p.GetRootFileNames()
	compilerOptions := p.GetCompilerOptions()

	p.program = compiler.NewProgram(compiler.ProgramOptions{
		RootFiles: rootFileNames,
		Host:      p,
		Options:   compilerOptions,
	})

	p.program.BindSourceFiles()
}

func (p *Project) isOrphan() bool {
	switch p.kind {
	case KindInferred:
		return p.rootFileNames.Size() == 0
	case KindConfigured:
		return p.deferredClose
	default:
		panic("unhandled project kind")
	}
}

func (p *Project) toPath(fileName string) tspath.Path {
	return tspath.ToPath(fileName, p.GetCurrentDirectory(), p.FS().UseCaseSensitiveFileNames())
}

func (p *Project) isRoot(info *ScriptInfo) bool {
	return p.rootFileNames.Has(info.path)
}

func (p *Project) removeFile(info *ScriptInfo, fileExists bool, detachFromProject bool) {
	if p.isRoot(info) {
		switch p.kind {
		case KindInferred:
			p.rootFileNames.Delete(info.path)
		case KindConfigured:
			p.reloadConfig = true
		}
	}

	// !!!
	// if (fileExists) {
	// 	// If file is present, just remove the resolutions for the file
	// 	this.resolutionCache.removeResolutionsOfFile(info.path);
	// } else {
	// 	this.resolutionCache.invalidateResolutionOfFile(info.path);
	// }
	// this.cachedUnresolvedImportsPerFile.delete(info.path);
	if detachFromProject {
		info.detachFromProject(p)
	}
	p.markAsDirty()
}

func (p *Project) addRoot(info *ScriptInfo) {
	// !!!
	// if p.kind == KindInferred {
	// 	p.projectService.startWatchingConfigFilesForInferredProjectRoot(info.path);
	//  // handle JS toggling
	// }
	if p.isRoot(info) {
		panic("script info is already a root")
	}
	p.rootFileNames.Set(info.path, info.fileName)
	info.attachToProject(p)
	p.markAsDirty()
}

func (p *Project) clearSourceMapperCache() {
	// !!!
}

func (p *Project) print(writeFileNames bool, writeFileExplanation bool, writeFileVersionAndText bool) string {
	var builder strings.Builder
	builder.WriteString(fmt.Sprintf("Project '%s' (%s)\n", p.name, p.kind.String()))
	if p.initialLoadPending {
		builder.WriteString("\tFiles (0) InitialLoadPending\n")
	} else if p.program == nil {
		builder.WriteString("\tFiles (0) NoProgram\n")
	} else {
		sourceFiles := p.program.GetSourceFiles()
		builder.WriteString(fmt.Sprintf("\tFiles (%d)\n", len(sourceFiles)))
		if writeFileNames {
			for _, sourceFile := range sourceFiles {
				builder.WriteString("\t\t" + sourceFile.FileName())
				if writeFileVersionAndText {
					builder.WriteString(fmt.Sprintf(" %d %s", sourceFile.Version, sourceFile.Text))
				}
				builder.WriteRune('\n')
			}
			// !!!
			// if writeFileExplanation {}
		}
	}
	builder.WriteString("-----------------------------------------------")
	return builder.String()
}

func (p *Project) log(s string) {
	p.projectService.log(s)
}
