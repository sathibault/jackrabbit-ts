package jackrabbit

import (
	"fmt"
	"os"

	"github.com/microsoft/typescript-go/internal/ast"
	"github.com/microsoft/typescript-go/internal/checker"
)

type RtlHost interface {
	genExpression(expr *ast.Expression) string
	addStateDriver(name string, state []string, expr *ast.Expression)
}

type DfaGen struct {
	host     RtlHost
	outFile  *os.File
	stateVar string
	stack    []*StatementContext
}

type StatementContext struct {
	Position   int
	Statements []*ast.Statement
}

func NewDfaGen(host RtlHost, outFile *os.File) *DfaGen {
	return &DfaGen{
		host:    host,
		outFile: outFile,
	}
}

func (gen *DfaGen) Generate(dfa *ast.Node) {
	gen.outFile.WriteString("  always @(posedge clk)\n")

	if !ast.IsCallExpression(dfa) {
		panic("expected CallExpression")
	}
	call := dfa.AsCallExpression()

	args := call.Arguments.Nodes
	if len(args) != 2 {
		panic("expected 2 arguments to DFA")
	}

	builder := args[0]
	if !ast.IsArrowFunction(builder) {
		panic("dfa argument must be an arrow function")
	}
	arrow := builder.AsArrowFunction()

	if !ast.IsIdentifier(args[1]) {
		panic("dfa state argument must be a signal name")
	}
	gen.stateVar = args[1].AsIdentifier().Text

	gen.outFile.WriteString(fmt.Sprintf("    case (%s)\n", gen.stateVar))

	if ast.IsBlock(arrow.Body) {
		gen.emitBlock(arrow.Body.AsBlock())
	}

	gen.outFile.WriteString("    endcase\n")
}

func (gen *DfaGen) emitBlock(block *ast.Block) {
	statements := block.Statements.Nodes
	for i, stmt := range block.Statements.Nodes {
		gen.emitStatement(i, stmt, statements)
	}
}

func (gen *DfaGen) emitStatement(position int, stmt *ast.Node, statements []*ast.Node) {
	if ast.IsExpressionStatement(stmt) {
		exprStmt := stmt.AsExpressionStatement()
		inst := dfaInstruction(exprStmt.Expression)
		if inst == "STATE" {
			args := dfaInstructionArguments(exprStmt.Expression)
			assert(len(args) != 0, "STATE instruction must have at least one argument")
			arg := args[0]

			if !ast.IsLiteralExpression(arg) || arg.Kind != ast.KindStringLiteral {
				panic("STATE argument must be a string literal")
			}
			lit := arg.AsStringLiteral()

			gen.outFile.WriteString(fmt.Sprintf("      %s: begin\n", lit.Text))

			assert(statements != nil, "statements list is required for STATE")

			gen.emitCase(4, lit.Text, &StatementContext{
				Position:   position + 1,
				Statements: statements,
			})

			gen.outFile.WriteString("      end\n")
		}
	} else if ast.IsIfStatement(stmt) {
		ifStmt := stmt.AsIfStatement()
		gen.stack = append(gen.stack, &StatementContext{Position: position, Statements: statements})
		gen.emitStatement(0, ifStmt.ThenStatement, nil)
		if ifStmt.ElseStatement != nil {
			gen.emitStatement(0, ifStmt.ElseStatement, nil)
		}
		gen.stack = gen.stack[:len(gen.stack)-1]
	} else if stmt.Kind == ast.KindWhileStatement {
		whileStmt := stmt.AsWhileStatement()
		gen.stack = append(gen.stack, &StatementContext{Position: position, Statements: statements})
		gen.emitStatement(0, whileStmt.Statement, nil)
		gen.stack = gen.stack[:len(gen.stack)-1]
	} else if ast.IsForStatement(stmt) {
		forStmt := stmt.AsForStatement()
		gen.stack = append(gen.stack, &StatementContext{Position: position, Statements: statements})
		gen.emitStatement(0, forStmt.Statement, nil)
		gen.stack = gen.stack[:len(gen.stack)-1]
	} else if ast.IsForInOrOfStatement(stmt) {
		forInStmt := stmt.AsForInOrOfStatement()
		gen.stack = append(gen.stack, &StatementContext{Position: position, Statements: statements})
		gen.emitStatement(0, forInStmt.Statement, nil)
		gen.stack = gen.stack[:len(gen.stack)-1]
	} else if ast.IsBlock(stmt) {
		block := stmt.AsBlock()
		gen.emitBlock(block)
	}
}

func (g *DfaGen) emitCase(indent int, name string, cur *StatementContext) {
	var preCondition string
	context := make([]*StatementContext, len(g.stack))
	copy(context, g.stack)

	position := cur.Position
	statements := cur.Statements

	for position < len(statements) {
		stmt := statements[position]
		if ast.IsExpressionStatement(stmt) {
			exprStmt := stmt.AsExpressionStatement()
			expr := exprStmt.Expression

			if dfaInstruction(expr) == "UNTIL" {
				args := dfaInstructionArguments(expr)
				if len(args) > 0 && dfaInstruction(args[0]) != "nextCycle" {
					preCondition = g.host.genExpression(args[0])
				}
				break
			} else if ast.IsCallExpression(expr) {
				call := expr.AsCallExpression()
				fun := call.Expression
				if ast.IsPropertyAccessExpression(fun) {
					prop := fun.AsPropertyAccessExpression()
					op := prop.Name().Text()
					if op == "is" {
						if ast.IsIdentifier(prop.Expression) {
							sig := prop.Expression.AsIdentifier().Text
							g.host.addStateDriver(sig, []string{g.stateVar, name}, call.Arguments.Nodes[0])
						}
					}
				}
			}
		}
		position++
	}

	if position == len(statements) {
		panic(fmt.Sprintf("DFA state %s is missing UNTIL", name))
	}

	if preCondition != "" {
		g.writeIndent(indent)
		g.outFile.WriteString(fmt.Sprintf("if (%s) begin\n", preCondition))
		indent++
	}

	elseLevel := 0

	for {
		var follow FollowState
		follow, elseLevel = g.emitTransitions(indent, &StatementContext{position, statements}, elseLevel, preCondition)
		if follow == "NoFollow" || len(context) == 0 {
			break
		}

		prnt := context[len(context)-1]
		context = context[:len(context)-1]
		stmt := prnt.Statements[prnt.Position]

		if stmt.Kind == ast.KindWhileStatement {
			whileStmt := stmt.AsWhileStatement()
			body := whileStmt.Statement
			assert(ast.IsBlock(body), "Expecting block in dfa loop")

			if checker.IsConstExpression(whileStmt.Expression) {
				value := checker.GetConstExpression(whileStmt.Expression)
				if cond, ok := value.(bool); ok {
					if cond {
						follow, elseLevel = g.emitTransitions(indent, &StatementContext{0, body.AsBlock().Statements.Nodes}, elseLevel, preCondition)
						break
					}
				}
			} else {
				cond := g.host.genExpression(whileStmt.Expression)
				g.writeIndent(indent + elseLevel)
				g.outFile.WriteString(fmt.Sprintf("if (%s) begin\n", cond))
				follow, elseLevel = g.emitTransitions(indent+1, &StatementContext{0, body.AsBlock().Statements.Nodes}, elseLevel, preCondition)
				g.writeIndent(indent + elseLevel)
				g.outFile.WriteString("end else begin\n")
				elseLevel++
			}
		}

		position = prnt.Position + 1
		statements = prnt.Statements
	}

	g.closeElse(indent, elseLevel)

	if preCondition != "" {
		indent--
		g.writeIndent(indent)
		g.outFile.WriteString("end\n")
	}
}

type FollowState string

const (
	NoFollow   FollowState = "NoFollow"
	ElseFollow FollowState = "ElseFollow"
	Follow     FollowState = "Follow"
)

func (e *DfaGen) emitTransitions(indent int, cursor *StatementContext, elseLevel int, preCondition string) (FollowState, int) {
	position := cursor.Position
	statements := cursor.Statements

	for position < len(statements) {
		stmt := statements[position]

		if ast.IsExpressionStatement(stmt) {
			exprStmt := stmt.AsExpressionStatement()
			if dfaInstruction(exprStmt.Expression) == "STATE" {
				args := dfaInstructionArguments(exprStmt.Expression)
				arg := args[0]
				if ast.IsLiteralExpression(arg) && arg.Kind == ast.KindStringLiteral {
					lit := arg.AsStringLiteral()
					e.writeIndent(indent + elseLevel)
					e.outFile.WriteString(fmt.Sprintf("%s <= %s;\n", e.stateVar, lit.Text))
					return NoFollow, elseLevel
				}
			}
		} else if stmt.Kind == ast.KindWhileStatement {
			whileStmt := stmt.AsWhileStatement()
			conditioned := false

			if checker.IsConstExpression(whileStmt.Expression) {
				value := checker.GetConstExpression(whileStmt.Expression)
				if cond, ok := value.(bool); ok {
					if !cond {
						position++
						continue
					}
				}
			} else {
				cond := e.host.genExpression(whileStmt.Expression)
				if cond != preCondition {
					e.writeIndent(indent + elseLevel)
					e.outFile.WriteString(fmt.Sprintf("if (%s) begin\n", cond))
					indent++
					conditioned = true
				}
			}

			assert(ast.IsBlock(whileStmt.Statement), "Expecting block in dfa loop")
			block := whileStmt.Statement.AsBlock()
			state, newElseLevel := e.emitTransitions(indent, &StatementContext{Position: 0, Statements: block.Statements.Nodes}, elseLevel, preCondition)
			assert(state == NoFollow)
			elseLevel = newElseLevel

			if conditioned {
				indent--
				e.writeIndent(indent + elseLevel)
				e.outFile.WriteString("end else begin\n")
				elseLevel++
			} else {
				return state, elseLevel
			}

		} else if ast.IsIfStatement(stmt) {
			ifStmt := stmt.AsIfStatement()
			cond := e.host.genExpression(ifStmt.Expression)

			assert(ast.IsBlock(ifStmt.ThenStatement), "Expecting block in dfa if")
			thenBlock := ifStmt.ThenStatement.AsBlock()

			if cond != preCondition {
				e.writeIndent(indent + elseLevel)
				e.outFile.WriteString(fmt.Sprintf("if (%s) begin\n", cond))

				state1, newElseLevel := e.emitTransitions(indent+1, &StatementContext{Position: 0, Statements: thenBlock.Statements.Nodes}, elseLevel, preCondition)
				assert(state1 == NoFollow)
				elseLevel = newElseLevel

				if ifStmt.ElseStatement != nil {
					e.writeIndent(indent + elseLevel)
					e.outFile.WriteString("end else begin\n")

					assert(ast.IsBlock(ifStmt.ElseStatement), "Expecting block in dfa if")
					elseBlock := ifStmt.ElseStatement.AsBlock()

					state2, _ := e.emitTransitions(indent+1, &StatementContext{Position: 0, Statements: elseBlock.Statements.Nodes}, elseLevel, preCondition)
					assert(state2 == NoFollow)

					e.writeIndent(indent + elseLevel)
					e.outFile.WriteString("end\n")
					return state2, elseLevel
				}

				e.writeIndent(indent + elseLevel)
				e.outFile.WriteString("end else begin\n")
				elseLevel++

			} else {
				return e.emitTransitions(indent, &StatementContext{Position: 0, Statements: thenBlock.Statements.Nodes}, elseLevel, preCondition)
			}
		} else {
			assert(ast.IsExpressionStatement(stmt), "Unexpected dfa statement")
		}

		position++
	}

	return Follow, elseLevel
}

func (g *DfaGen) writeIndent(level int) {
	i := 0
	for i < level {
		g.outFile.WriteString("  ")
		i += 1
	}
}

func (g *DfaGen) closeElse(indent int, level int) {
	if level > 0 {
		g.closeElse(indent+1, level-1)
		g.writeIndent(indent)
		g.outFile.WriteString("end\n")
	}
}

func dfaInstruction(expr *ast.Node) string {
	if !ast.IsCallExpression(expr) {
		return ""
	}
	call := expr.AsCallExpression()

	fun := call.Expression
	if ast.IsIdentifier(fun) {
		name := fun.AsIdentifier().Text
		if name == "STATE" || name == "UNTIL" || name == "nextCycle" {
			return name
		}
	}

	return ""
}

func dfaInstructionArguments(expr *ast.Node) []*ast.Node {
	if !ast.IsCallExpression(expr) {
		panic("dfaInstructionArguments: expected CallExpression")
	}
	call := expr.AsCallExpression()
	return call.Arguments.Nodes
}
