package jackrabbit

import (
	"errors"

	"github.com/microsoft/typescript-go/internal/ast"
)

const MAX_FIXPOINT = 1000

type AbstractState interface {
	Enter(stmt *ast.Node)
	Leave(stmt *ast.Node)
	Apply(stmt *ast.Node)
	Merge(other AbstractState) bool
	Clone() AbstractState
	Dump()
}

type AbstractRunner[T AbstractState] struct {
	State T
}

func NewAbstractRunner[T AbstractState](state T) *AbstractRunner[T] {
	return &AbstractRunner[T]{State: state}
}

func (r *AbstractRunner[T]) Run(block *ast.Block) {
	r.walk(&block.Node)
}

func (r *AbstractRunner[T]) walk(stmt *ast.Node) {
	switch stmt.Kind {
	case ast.KindIfStatement:
		r.State.Enter(stmt)
		s0 := r.State.Clone().(T)
		r.walk(stmt.AsIfStatement().ThenStatement)
		if stmt.AsIfStatement().ElseStatement != nil {
			ts := r.State
			r.State = s0
			r.walk(stmt.AsIfStatement().ElseStatement)
			r.State.Merge(ts)
		}

	case ast.KindWhileStatement:
		r.State.Enter(stmt)
		count := 0
		iterate := true
		for iterate {
			s0 := r.State.Clone().(T)
			r.walk(stmt.AsWhileStatement().Statement)
			iterate = r.State.Merge(s0)
			count++
			if count == MAX_FIXPOINT {
				r.State.Dump()
				panic(errors.New("Max fixpoint"))
			}
		}

	case ast.KindDoStatement:
		r.State.Enter(stmt)
		count := 0
		iterate := true
		for iterate {
			s0 := r.State.Clone().(T)
			r.walk(stmt.AsDoStatement().Statement)
			iterate = r.State.Merge(s0)
			count++
			if count == MAX_FIXPOINT {
				r.State.Dump()
				panic(errors.New("Max fixpoint"))
			}
		}

	case ast.KindForStatement:
		r.State.Enter(stmt)
		count := 0
		iterate := true
		for iterate {
			s0 := r.State.Clone().(T)
			r.walk(stmt.AsForStatement().Statement)
			iterate = r.State.Merge(s0)
			count++
			if count == MAX_FIXPOINT {
				r.State.Dump()
				panic(errors.New("Max fixpoint"))
			}
		}

	case ast.KindForInStatement, ast.KindForOfStatement:
		r.State.Enter(stmt)
		count := 0
		iterate := true
		for iterate {
			s0 := r.State.Clone().(T)
			r.walk(stmt.AsForInOrOfStatement().Statement)
			iterate = r.State.Merge(s0)
			count++
			if count == MAX_FIXPOINT {
				r.State.Dump()
				panic(errors.New("Max fixpoint"))
			}
		}

	case ast.KindWithStatement:
		r.State.Enter(stmt)
		r.walk(stmt.AsWithStatement().Statement)

	case ast.KindSwitchStatement:
		r.State.Enter(stmt)
		s0 := r.State
		merged := s0.Clone().(T)
		block := stmt.AsSwitchStatement().CaseBlock.AsCaseBlock()
		for _, clause0 := range block.Clauses.Nodes {
			clause := clause0.AsCaseOrDefaultClause()
			r.State = s0.Clone().(T)
			for _, cs := range clause.Statements.Nodes {
				r.walk(cs)
			}
			merged.Merge(r.State)
		}
		r.State = merged

	case ast.KindBlock:
		r.State.Enter(stmt)
		for _, stmt1 := range stmt.AsBlock().Statements.Nodes {
			r.walk(stmt1)
		}

	case ast.KindLabeledStatement:
		r.State.Enter(stmt)
		r.walk(stmt.AsLabeledStatement().Statement)

	default:
		r.State.Apply(stmt)
		return
	}
	r.State.Leave(stmt)
}
