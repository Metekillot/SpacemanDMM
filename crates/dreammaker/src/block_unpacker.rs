//! Block unpacker - renders Block AST structures to UTF-8 text
//!
//! This module provides functionality to unpack and render the Block type
//! from the DreamMaker AST into human-readable code.

use std::fmt::Write;
use crate::ast::*;
use crate::lexer::{Quote, FormatFloat};

/// Unpacks a Block into UTF-8 text representation
pub fn unpack_block(block: &Block) -> String {
    let mut output = String::new();
    let mut renderer = BlockRenderer::new(&mut output);
    renderer.render_block(block, 0);
    output
}

/// Renders AST structures to text
struct BlockRenderer<'a> {
    output: &'a mut String,
}

impl<'a> BlockRenderer<'a> {
    fn new(output: &'a mut String) -> Self {
        BlockRenderer { output }
    }

    fn render_block(&mut self, block: &Block, indent_level: usize) {
        for statement in block.iter() {
            self.render_statement(&statement.elem, indent_level);
        }
    }

    fn indent(&mut self, level: usize) {
        for _ in 0..level {
            self.output.push_str("    ");
        }
    }

    fn render_statement(&mut self, statement: &Statement, indent_level: usize) {
        self.indent(indent_level);
        match statement {
            Statement::Expr(expr) => {
                self.render_expression(expr);
                self.output.push('\n');
            }
            Statement::Return(expr) => {
                self.output.push_str("return");
                if let Some(expr) = expr {
                    self.output.push(' ');
                    self.render_expression(expr);
                }
                self.output.push('\n');
            }
            Statement::Throw(expr) => {
                self.output.push_str("throw ");
                self.render_expression(expr);
                self.output.push('\n');
            }
            Statement::While { condition, block } => {
                self.output.push_str("while(");
                self.render_expression(condition);
                self.output.push_str(")\n");
                self.render_indented_block(block, indent_level);
            }
            Statement::DoWhile { block, condition } => {
                self.output.push_str("do\n");
                self.render_indented_block(block, indent_level);
                self.indent(indent_level);
                self.output.push_str("while(");
                self.render_expression(&condition.elem);
                self.output.push_str(")\n");
            }
            Statement::If { arms, else_arm } => {
                let mut first = true;
                for (condition, block) in arms {
                    if !first {
                        self.indent(indent_level);
                        self.output.push_str("else ");
                    }
                    first = false;
                    self.output.push_str("if(");
                    self.render_expression(&condition.elem);
                    self.output.push_str(")\n");
                    self.render_indented_block(block, indent_level);
                }
                if let Some(else_block) = else_arm {
                    self.indent(indent_level);
                    self.output.push_str("else\n");
                    self.render_indented_block(else_block, indent_level);
                }
            }
            Statement::ForInfinite { block } => {
                self.output.push_str("for()\n");
                self.render_indented_block(block, indent_level);
            }
            Statement::ForLoop { init, test, inc, block } => {
                self.output.push_str("for(");
                if let Some(init) = init {
                    self.render_statement_inline(init);
                }
                self.output.push_str("; ");
                if let Some(test) = test {
                    self.render_expression(test);
                }
                self.output.push_str("; ");
                if let Some(inc) = inc {
                    self.render_statement_inline(inc);
                }
                self.output.push_str(")\n");
                self.render_indented_block(block, indent_level);
            }
            Statement::ForList(for_list) => {
                self.output.push_str("for(");
                if let Some(var_type) = &for_list.var_type {
                    self.render_var_type(var_type);
                }
                self.output.push_str(for_list.name.as_str());
                if let Some(input_type) = &for_list.input_type {
                    write!(self.output, " as {}", input_type).unwrap();
                }
                self.output.push_str(" in ");
                if let Some(in_list) = &for_list.in_list {
                    self.render_expression(in_list);
                } else {
                    self.output.push_str("world");
                }
                self.output.push_str(")\n");
                self.render_indented_block(&for_list.block, indent_level);
            }
            Statement::ForKeyValue(for_kv) => {
                self.output.push_str("for(");
                if let Some(var_type) = &for_kv.var_type {
                    self.render_var_type(var_type);
                }
                write!(self.output, "{}, {} in ", for_kv.key.as_str(), for_kv.value.as_str()).unwrap();
                if let Some(in_list) = &for_kv.in_list {
                    self.render_expression(in_list);
                } else {
                    self.output.push_str("world");
                }
                self.output.push_str(")\n");
                self.render_indented_block(&for_kv.block, indent_level);
            }
            Statement::ForRange(for_range) => {
                self.output.push_str("for(");
                if let Some(var_type) = &for_range.var_type {
                    self.render_var_type(var_type);
                }
                self.output.push_str(for_range.name.as_str());
                self.output.push_str(" = ");
                self.render_expression(&for_range.start);
                self.output.push_str(" to ");
                self.render_expression(&for_range.end);
                if let Some(step) = &for_range.step {
                    self.output.push_str(" step ");
                    self.render_expression(step);
                }
                self.output.push_str(")\n");
                self.render_indented_block(&for_range.block, indent_level);
            }
            Statement::Var(var_stmt) => {
                self.render_var_statement(var_stmt);
                self.output.push('\n');
            }
            Statement::Vars(vars) => {
                for var in vars {
                    self.indent(indent_level);
                    self.render_var_statement(var);
                    self.output.push('\n');
                }
            }
            Statement::Setting { name, mode, value } => {
                write!(self.output, "set {} {} ", name.as_str(), mode).unwrap();
                self.render_expression(value);
                self.output.push('\n');
            }
            Statement::Spawn { delay, block } => {
                self.output.push_str("spawn");
                if let Some(delay) = delay {
                    self.output.push('(');
                    self.render_expression(delay);
                    self.output.push(')');
                }
                self.output.push('\n');
                self.render_indented_block(block, indent_level);
            }
            Statement::Switch { input, cases, default } => {
                self.output.push_str("switch(");
                self.render_expression(input);
                self.output.push_str(")\n");
                for (case_list, block) in cases.iter() {
                    for case in case_list.elem.iter() {
                        self.indent(indent_level + 1);
                        self.output.push_str("if(");
                        match case {
                            Case::Exact(expr) => self.render_expression(expr),
                            Case::Range(start, end) => {
                                self.render_expression(start);
                                self.output.push_str(" to ");
                                self.render_expression(end);
                            }
                        }
                        self.output.push_str(")\n");
                    }
                    self.render_indented_block(block, indent_level + 1);
                }
                if let Some(default_block) = default {
                    self.indent(indent_level + 1);
                    self.output.push_str("else\n");
                    self.render_indented_block(default_block, indent_level + 1);
                }
            }
            Statement::TryCatch { try_block, catch_params, catch_block } => {
                self.output.push_str("try\n");
                self.render_indented_block(try_block, indent_level);
                self.indent(indent_level);
                self.output.push_str("catch(");
                let mut first = true;
                for param in catch_params.iter() {
                    if !first {
                        self.output.push_str(" | ");
                    }
                    first = false;
                    self.render_tree_path(param);
                }
                self.output.push_str(")\n");
                self.render_indented_block(catch_block, indent_level);
            }
            Statement::Continue(label) => {
                self.output.push_str("continue");
                if let Some(label) = label {
                    write!(self.output, " {}", label).unwrap();
                }
                self.output.push('\n');
            }
            Statement::Break(label) => {
                self.output.push_str("break");
                if let Some(label) = label {
                    write!(self.output, " {}", label).unwrap();
                }
                self.output.push('\n');
            }
            Statement::Goto(label) => {
                write!(self.output, "goto {}\n", label).unwrap();
            }
            Statement::Label { name, block } => {
                write!(self.output, "{}:\n", name).unwrap();
                self.render_block(block, indent_level);
            }
            Statement::Del(expr) => {
                self.output.push_str("del(");
                self.render_expression(expr);
                self.output.push_str(")\n");
            }
            Statement::Crash(expr) => {
                self.output.push_str("CRASH(");
                if let Some(expr) = expr {
                    self.render_expression(expr);
                }
                self.output.push_str(")\n");
            }
        }
    }

    fn render_statement_inline(&mut self, statement: &Statement) {
        match statement {
            Statement::Expr(expr) => self.render_expression(expr),
            Statement::Var(var_stmt) => self.render_var_statement(var_stmt),
            _ => self.output.push_str("/* complex statement */"),
        }
    }

    fn render_var_statement(&mut self, var_stmt: &VarStatement) {
        self.render_var_type(&var_stmt.var_type);
        self.output.push_str(&var_stmt.name);
        if let Some(value) = &var_stmt.value {
            self.output.push_str(" = ");
            self.render_expression(value);
        }
    }

    fn render_var_type(&mut self, var_type: &VarType) {
        write!(self.output, "{}", var_type).unwrap();
    }

    fn render_tree_path(&mut self, path: &TreePath) {
        write!(self.output, "{}", FormatTreePath(path)).unwrap();
    }

    fn render_indented_block(&mut self, block: &Block, indent_level: usize) {
        self.indent(indent_level);
        self.output.push_str("{\n");
        self.render_block(block, indent_level + 1);
        self.indent(indent_level);
        self.output.push_str("}\n");
    }

    fn render_expression(&mut self, expr: &Expression) {
        match expr {
            Expression::Base { term, follow } => {
                self.render_term(&term.elem);
                for f in follow.iter() {
                    self.render_follow(&f.elem, &term.elem);
                }
            }
            Expression::BinaryOp { op, lhs, rhs } => {
                self.output.push('(');
                self.render_expression(lhs);
                write!(self.output, " {} ", op).unwrap();
                self.render_expression(rhs);
                self.output.push(')');
            }
            Expression::AssignOp { op, lhs, rhs } => {
                self.render_expression(lhs);
                write!(self.output, " {} ", op).unwrap();
                self.render_expression(rhs);
            }
            Expression::TernaryOp { cond, if_, else_ } => {
                self.output.push('(');
                self.render_expression(cond);
                self.output.push_str(" ? ");
                self.render_expression(if_);
                self.output.push_str(" : ");
                self.render_expression(else_);
                self.output.push(')');
            }
        }
    }

    fn render_term(&mut self, term: &Term) {
        match term {
            Term::Null => self.output.push_str("null"),
            Term::Int(i) => write!(self.output, "{}", i).unwrap(),
            Term::Float(f) => write!(self.output, "{}", FormatFloat(*f)).unwrap(),
            Term::Ident(s) => self.output.push_str(s),
            Term::String(s) => write!(self.output, "{}", Quote(s)).unwrap(),
            Term::Resource(s) => write!(self.output, "'{}'", s).unwrap(),
            Term::As(input_type) => write!(self.output, "as({})", input_type).unwrap(),
            Term::__PROC__ => self.output.push_str("__PROC__"),
            Term::__TYPE__ => self.output.push_str("__TYPE__"),
            Term::__IMPLIED_TYPE__ => self.output.push_str("__IMPLIED_TYPE__"),
            Term::Expr(expr) => {
                self.output.push('(');
                self.render_expression(expr);
                self.output.push(')');
            }
            Term::Prefab(prefab) => {
                write!(self.output, "{}", FormatTypePath(&prefab.path)).unwrap();
                if !prefab.vars.is_empty() {
                    self.output.push_str(" {");
                    let mut first = true;
                    for (k, v) in prefab.vars.iter() {
                        if !first {
                            self.output.push_str("; ");
                        }
                        first = false;
                        write!(self.output, "{} = ", k).unwrap();
                        self.render_expression(v);
                    }
                    self.output.push('}');
                }
            }
            Term::InterpString(first, parts) => {
                self.output.push('"');
                self.output.push_str(first.as_str());
                for (expr, text) in parts.iter() {
                    self.output.push('[');
                    if let Some(expr) = expr {
                        self.render_expression(expr);
                    }
                    self.output.push(']');
                    self.output.push_str(text);
                }
                self.output.push('"');
            }
            Term::Call(name, args) => {
                self.output.push_str(name.as_str());
                self.output.push('(');
                self.render_args(args);
                self.output.push(')');
            }
            Term::SelfCall(args) => {
                self.output.push_str(".(");
                self.render_args(args);
                self.output.push(')');
            }
            Term::ParentCall(args) => {
                self.output.push_str("..(");
                self.render_args(args);
                self.output.push(')');
            }
            Term::NewImplicit { args } => {
                self.output.push_str("new");
                if let Some(args) = args {
                    self.output.push('(');
                    self.render_args(args);
                    self.output.push(')');
                }
            }
            Term::NewPrefab { prefab, args } => {
                self.output.push_str("new ");
                write!(self.output, "{}", FormatTypePath(&prefab.path)).unwrap();
                if !prefab.vars.is_empty() {
                    self.output.push_str(" {");
                    let mut first = true;
                    for (k, v) in prefab.vars.iter() {
                        if !first {
                            self.output.push_str("; ");
                        }
                        first = false;
                        write!(self.output, "{} = ", k).unwrap();
                        self.render_expression(v);
                    }
                    self.output.push('}');
                }
                if let Some(args) = args {
                    self.output.push('(');
                    self.render_args(args);
                    self.output.push(')');
                }
            }
            Term::NewMiniExpr { expr, args } => {
                self.output.push_str("new ");
                self.render_mini_expr(expr);
                if let Some(args) = args {
                    self.output.push('(');
                    self.render_args(args);
                    self.output.push(')');
                }
            }
            Term::List(exprs) => {
                self.output.push_str("list(");
                self.render_args(exprs);
                self.output.push(')');
            }
            Term::Input { args, input_type, in_list } => {
                self.output.push_str("input(");
                self.render_args(args);
                self.output.push(')');
                if let Some(input_type) = input_type {
                    write!(self.output, " as {}", input_type).unwrap();
                }
                if let Some(in_list) = in_list {
                    self.output.push_str(" in ");
                    self.render_expression(in_list);
                }
            }
            Term::Locate { args, in_list } => {
                self.output.push_str("locate(");
                self.render_args(args);
                self.output.push(')');
                if let Some(in_list) = in_list {
                    self.output.push_str(" in ");
                    self.render_expression(in_list);
                }
            }
            Term::Pick(pick_args) => {
                self.output.push_str("pick(");
                let mut first = true;
                for (weight, expr) in pick_args.iter() {
                    if !first {
                        self.output.push_str("; ");
                    }
                    first = false;
                    if let Some(weight) = weight {
                        self.render_expression(weight);
                        self.output.push(';');
                    }
                    self.render_expression(expr);
                }
                self.output.push(')');
            }
            Term::DynamicCall(func, args) => {
                self.output.push_str("call(");
                self.render_args(func);
                self.output.push_str(")(");
                self.render_args(args);
                self.output.push(')');
            }
            Term::ExternalCall { library, function, args } => {
                self.output.push_str("call_ext(");
                if let Some(lib) = library {
                    self.render_expression(lib);
                    self.output.push_str(", ");
                }
                self.render_expression(function);
                self.output.push_str(")(");
                self.render_args(args);
                self.output.push(')');
            }
            Term::GlobalIdent(name) => {
                write!(self.output, "::{}", name.as_str()).unwrap();
            }
            Term::GlobalCall(name, args) => {
                write!(self.output, "::{}(", name.as_str()).unwrap();
                self.render_args(args);
                self.output.push(')');
            }
        }
    }

    fn render_mini_expr(&mut self, expr: &MiniExpr) {
        self.output.push_str(expr.ident.as_str());
        for field in expr.fields.iter() {
            write!(self.output, "{}{}", field.kind, field.ident.as_str()).unwrap();
        }
    }

    fn render_follow(&mut self, follow: &Follow, _base_term: &Term) {
        match follow {
            Follow::Index(kind, expr) => {
                match kind {
                    ListAccessKind::Normal => self.output.push('['),
                    ListAccessKind::Safe => self.output.push_str("?["),
                }
                self.render_expression(expr);
                self.output.push(']');
            }
            Follow::Field(kind, name) => {
                write!(self.output, "{}{}", kind, name.as_str()).unwrap();
            }
            Follow::Call(kind, name, args) => {
                write!(self.output, "{}{}(", kind, name.as_str()).unwrap();
                self.render_args(args);
                self.output.push(')');
            }
            Follow::Unary(op) => {
                // Handle postfix operators specially
                match op {
                    UnaryOp::PostIncr => self.output.push_str("++"),
                    UnaryOp::PostDecr => self.output.push_str("--"),
                    _ => write!(self.output, "{}", op.name()).unwrap(),
                }
            }
            Follow::StaticField(name) => {
                write!(self.output, "::{}", name.as_str()).unwrap();
            }
            Follow::ProcReference(name) => {
                write!(self.output, "::{}", name.as_str()).unwrap();
            }
        }
    }

    fn render_args(&mut self, args: &[Expression]) {
        let mut first = true;
        for arg in args {
            if !first {
                self.output.push_str(", ");
            }
            first = false;
            self.render_expression(arg);
        }
    }
}
