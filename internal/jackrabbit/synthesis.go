package jackrabbit

import (
	"os"
	"path"
	"slices"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
	"github.com/microsoft/typescript-go/internal/core"
)

type Synthesizer struct {
	sm *SmContext
}

func NewSynthesizer() *Synthesizer {
	exePath := path.Dir(os.Args[0])
	libPath := path.Join(exePath, "hls")
	sm := CreateSmContext(libPath)

	return &Synthesizer{
		sm: sm,
	}
}

func (syn *Synthesizer) Synthesize(sourceFile *ast.SourceFile, tc *checker.Checker) {
	sd := GetSourceDescriptor(sourceFile)
	if sd != nil {
		visit := func(node *ast.Node) bool {
			if ast.IsVariableStatement(node) {
				// generate globals
				// stmt := node.AsVariableStatement()
			} else if ast.IsFunctionDeclaration(node) {
				decl := node.AsFunctionDeclaration()
				fd := GetFunctionDeclDescriptor(decl)
				if fd != nil {
					if fd.UsedByType("hls") {
						hlsGen := NewHlsProcGen(decl, fd, tc, true)
						syn.sm.Xic.Generate(hlsGen)
					}
				}
			}
			return false
		}

		sourceFile.ForEachChild(visit)
	}
}

func (syn *Synthesizer) Finalize() {
	f, err := os.Create("jackrabit.xic")
	if err != nil {
		panic(err)
	}
	syn.sm.Xic.Dump(f)
	f.Sync()
}

type TextEdit struct {
	pos    int
	end    int
	newLen int
}

func NewTextEdit(pos int, end int, newLen int) TextEdit {
	return TextEdit{
		pos:    pos,
		end:    end,
		newLen: newLen,
	}
}

type HlsDescriptor struct {
	sm        *SmContext
	blocks    []HlsBlockSummary
	textRange core.TextRange
	dirty     bool
}

func (h *HlsDescriptor) GetSummary() []HlsBlockSummary {
	return h.blocks
}

func (h *HlsDescriptor) RenderBlock(no uint) (string, int) {
	for i := range h.blocks {
		if h.blocks[i].BlockNo == no {
			html := h.sm.RenderBlock(no)
			return html, h.blocks[i].Position
		}
	}
	return "", -1
}

type Synthesis struct {
	architecturesPath string
	hlsModules        map[string]*HlsDescriptor
}

func NewSynthesis(architecturesPath string) *Synthesis {
	return &Synthesis{
		architecturesPath: architecturesPath,
		hlsModules:        make(map[string]*HlsDescriptor),
	}
}

func (s *Synthesis) EnsureHlsDescriptor(fun *ast.FunctionDeclaration, checker *checker.Checker) {
	name := fun.Name().Text()
	if _, exists := s.hlsModules[name]; !exists {
		desc := FunctionDescriptor{}
		sm := CreateSmContext(s.architecturesPath)
		proc := NewHlsProcGen(fun, &desc, checker, true)
		sm.Generate(proc)
		blocks := sm.Analysis()
		s.hlsModules[name] = &HlsDescriptor{
			sm:        sm,
			blocks:    blocks,
			textRange: fun.AsNode().Loc,
			dirty:     false,
		}
	}
}

func (s *Synthesis) GetHlsDescriptor(fun *ast.FunctionDeclaration) *HlsDescriptor {
	return s.hlsModules[fun.Name().Text()]
}

func (s *Synthesis) ApplyEdit(edits []TextEdit) {
	for _, desc := range s.hlsModules {
		for _, e := range edits {
			delta := e.newLen - (e.end - e.pos)
			// mark all functions touched as dirty
			if e.overlaps(desc.textRange.Pos(), desc.textRange.End()) {
				desc.dirty = true
			}
			if delta < 0 {
				// deletes
				for i := len(desc.blocks) - 1; i >= 0; i-- {
					if e.contains(desc.blocks[i].Position) {
						desc.blocks = slices.Delete(desc.blocks, i, i+1)
					}
				}
			}
			for i := range desc.blocks {
				blk := &desc.blocks[i]
				if delta > 0 && e.pos == e.end && e.pos == blk.Position {
					// special case, include the insert in the block
					blk.End += delta
				} else if e.end <= blk.Position {
					blk.Position += delta
				} else if e.end <= blk.End {
					// deleted above if edit spans start position, so must be complete inside
					blk.End += delta
				} else if e.pos < blk.End {
					// spans the end position, no way to know where the new end is
					blk.End = e.pos
				}
			}
		}
	}
}

func (e *TextEdit) contains(x int) bool {
	return e.pos <= x && x < e.end
}

func (e *TextEdit) overlaps(x int, y int) bool {
	if y < e.pos || x >= e.end {
		return false
	}
	return true
}
