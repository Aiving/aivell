use crate::{
    Ast, AstBinaryOp, AstFor, AstIf, AstIndex, AstString, AstUnaryOp, ChainNode, ConstantIndex,
    ConstantPoolBuilder, Error, ErrorKind, Function, InternalError, MatchArm, Node, Result,
    StringContents, SyntaxError, Type, UseItem,
};
use derive_name::VariantName;
use lexer::{
    token::{Delimiter, Literal, Operator, Punctuation, ReservedWord, Token},
    Lexer,
};
use shared::{Positioned, Span};
use std::{collections::HashSet, iter::Peekable, vec::IntoIter};

// Contains info about the current frame, representing either the module's top level or a function
#[derive(Debug, Default)]
struct Frame {
    // If a frame contains yield then it represents a generator function
    // contains_yield: bool,
    // IDs that have been assigned within the current frame
    ids_assigned_in_frame: HashSet<ConstantIndex>,
    // IDs and chain roots which were accessed when not locally assigned at the time of access
    accessed_non_locals: HashSet<ConstantIndex>,
    // While expressions are being parsed we keep track of lhs assignments and rhs accesses.
    // At the end of a multi-assignment expression (see `finalize_id_accesses`),
    // accessed IDs that weren't locally assigned at the time of access are then counted as
    // non-local accesses.
    pending_accesses: HashSet<ConstantIndex>,
    pending_assignments: HashSet<ConstantIndex>,
}

impl Frame {
    // The number of local values declared within the frame
    fn local_count(&self) -> usize {
        self.ids_assigned_in_frame.len()
    }

    // Non-locals accessed in a nested frame need to be declared as also accessed in this
    // frame. This ensures that captures from the outer frame will be available when
    // creating the nested inner frame.
    fn add_nested_accessed_non_locals(&mut self, nested_frame: &Frame) {
        for non_local in nested_frame.accessed_non_locals.iter() {
            if !self.pending_assignments.contains(non_local) {
                self.add_id_access(*non_local);
            }
        }
    }

    // Declare that an id has been accessed within the frame
    fn add_id_access(&mut self, id: ConstantIndex) {
        self.pending_accesses.insert(id);
    }

    // Declare that an id is being assigned to within the frame
    fn add_local_id_assignment(&mut self, id: ConstantIndex) {
        self.pending_assignments.insert(id);
        // While an assignment expression is being parsed, the LHS id is counted as an access
        // until the assignment operator is encountered.
        self.pending_accesses.remove(&id);
    }

    // At the end of an expression, determine which RHS accesses are non-local
    fn finalize_id_accesses(&mut self) {
        for id in self.pending_accesses.drain() {
            if !self.ids_assigned_in_frame.contains(&id) {
                self.accessed_non_locals.insert(id);
            }
        }

        self.ids_assigned_in_frame
            .extend(self.pending_assignments.drain());
    }
}

pub struct Parser {
    ast: Ast,
    constants: ConstantPoolBuilder,
    tokens: Peekable<IntoIter<Positioned<Token>>>,
    frame_stack: Vec<Frame>,
}

fn operator_precedence(op: &Operator) -> Option<(u8, u8)> {
    let priority = match op {
        Operator::PlusEq
        | Operator::MinusEq
        | Operator::StarEq
        | Operator::SlashEq
        | Operator::PercentEq => (4, 3),
        Operator::OrOr => (7, 8),
        Operator::AndAnd => (9, 10),
        // Chained comparisons require right-associativity
        Operator::EqEq | Operator::Ne => (12, 11),
        Operator::Gt | Operator::Ge | Operator::Lt | Operator::Le => (14, 13),
        Operator::Plus | Operator::Minus => (15, 16),
        Operator::Star | Operator::Slash | Operator::Percent => (17, 18),
        _ => return None,
    };

    Some(priority)
}

impl Parser {
    fn consume_main_block(&mut self) -> Result<AstIndex> {
        self.frame_stack.push(Frame::default());

        let start_span = self.position();

        let mut body = Vec::new();

        while !self.check_if(Token::is_end) {
            body.push(self.parse_statement()?.value);
        }

        let eof = self.next().unwrap();

        if let Some(token) = self.peek() {
            return Err(Error::new(
                ErrorKind::SyntaxError(SyntaxError::UnexpectedToken),
                token.span,
            ));
        }

        let result = self.push_node(
            Node::MainBlock {
                body: body.into(),
                local_count: self.frame()?.local_count(),
            },
            start_span.between(eof.span),
        )?;

        self.frame_stack.pop();

        Ok(result.value)
    }

    fn parse_statement(&mut self) -> Result<Positioned<AstIndex>> {
        let span = self.position();
        let Positioned { value, span: _ } = self.peek().ok_or_else(|| {
            Error::new(
                ErrorKind::SyntaxError(SyntaxError::ExpectedExpression),
                span,
            )
        })?;

        match value {
            Token::ReservedWord(ReservedWord::Use) => self.parse_use_statement(),
            Token::ReservedWord(ReservedWord::Var) => self.parse_var_statement(),
            Token::ReservedWord(ReservedWord::Func) => self.parse_func_statement(),
            Token::ReservedWord(ReservedWord::Return) => self.parse_return_statement(),
            Token::ReservedWord(ReservedWord::Enum) => unimplemented!(),
            _ => {
                let result = self.parse_expression()?;

                self.try_consume(&Token::Punctuation(Punctuation::Semi));

                Ok(result)
            }
        }
    }

    fn parse_return_statement(&mut self) -> Result<Positioned<AstIndex>> {
        let start_span = self
            .consume(&Token::ReservedWord(ReservedWord::Return))?
            .span;

        let return_value = self.parse_expression().ok();

        let end_span = self.consume(&Token::Punctuation(Punctuation::Semi))?.span;

        self.push_node(
            Node::Return(return_value.map(|v| v.value)),
            start_span.between(end_span),
        )
    }

    fn parse_var_statement(&mut self) -> Result<Positioned<AstIndex>> {
        let start_span = self.consume(&Token::ReservedWord(ReservedWord::Var))?.span;

        let name = self.parse_id()?;

        let ty = if self.try_consume(&Token::Punctuation(Punctuation::Colon)) {
            Some(self.parse_type()?)
        } else {
            None
        };

        let target_span = if let Some(ty) = ty.as_ref() {
            name.span.between(ty.span)
        } else {
            name.span
        };

        let target = self.push_node(Node::Id(name.value, ty.map(|v| v.value)), target_span)?;

        let result = self.parse_assign_expression(target)?;

        let end_span = self.consume(&Token::Punctuation(Punctuation::Semi))?.span;

        Ok(start_span.between(end_span).wrap(result.value))
    }

    fn parse_assign_expression(
        &mut self,
        target: Positioned<AstIndex>,
    ) -> Result<Positioned<AstIndex>> {
        let assign_span = self.consume(&Token::Operator(Operator::Eq))?.span;

        // Note which identifiers are being assigned to
        match self.ast.node(target.value).node.clone() {
            Node::Id(id_index, ..) => {
                self.frame_mut()?.add_local_id_assignment(id_index);
            }
            Node::Meta { .. } | Node::Chain(_) | Node::Wildcard(..) => {}
            _ => {
                return Err(Error::new(
                    ErrorKind::SyntaxError(SyntaxError::ExpectedAssignmentTarget),
                    assign_span,
                ))
            }
        }

        let expression = self.parse_expression()?;

        self.push_node(
            Node::Assign {
                target: target.value,
                expression: expression.value,
            },
            target.span.between(expression.span),
        )
    }

    fn parse_func_statement(&mut self) -> Result<Positioned<AstIndex>> {
        let span_start = self.consume(&Token::ReservedWord(ReservedWord::Func))?.span;

        let name = self.parse_id()?;

        self.frame_mut()?.add_local_id_assignment(name.value);

        let name = self.push_node(Node::Id(name.value, None), name.span)?;

        self.consume(&Token::Delimiter(Delimiter::ParenOpen))?;

        let mut arg_nodes = Vec::new();
        let mut arg_ids = Vec::new();
        let mut is_variadic = false;

        while !self.check(&Token::Delimiter(Delimiter::ParenClose)) {
            is_variadic = self.try_consume(&Token::Punctuation(Punctuation::Ellipsis));

            let id = self.parse_id()?;

            arg_ids.push(id.value);

            self.consume(&Token::Punctuation(Punctuation::Colon))?;

            let ty = self.parse_type()?;

            arg_nodes.push(
                self.push_node(Node::Id(id.value, Some(ty.value)), id.span.between(ty.span))?
                    .value,
            );

            if is_variadic {
                break;
            }
        }

        self.consume(&Token::Delimiter(Delimiter::ParenClose))?;
        self.consume(&Token::Punctuation(Punctuation::RArrow))?;

        let output_type = self.parse_type()?;

        let mut function_frame = Frame::default();

        function_frame.ids_assigned_in_frame.extend(arg_ids.iter());

        self.frame_stack.push(function_frame);

        let body = self.parse_block_expression()?;

        let function_frame = self.frame_stack.pop().ok_or_else(|| {
            Error::new(
                ErrorKind::InternalError(InternalError::MissingFrame),
                span_start,
            )
        })?;

        self.frame_mut()?
            .add_nested_accessed_non_locals(&function_frame);

        let local_count = function_frame.local_count();

        self.push_node(
            Node::Function(Function {
                name: Some(name.value),
                args: arg_nodes.into(),
                local_count,
                accessed_non_locals: Vec::from_iter(function_frame.accessed_non_locals).into(),
                body: body.value,
                is_variadic,
                output_type: Some(output_type.value),
            }),
            span_start.between(body.span),
        )
    }

    fn parse_term(&mut self) -> Result<Positioned<AstIndex>> {
        let span = self.position();

        let Positioned { value, span } = self.peek().ok_or_else(|| {
            Error::new(
                ErrorKind::SyntaxError(SyntaxError::ExpectedExpression),
                span,
            )
        })?;

        match value {
            Token::Literal(_) => self.parse_literal(false),
            Token::Ident(_) => self.parse_ident_expression(),
            Token::ReservedWord(word) => match word {
                ReservedWord::Null => {
                    let span = self.next().unwrap().span;

                    self.push_node(Node::Null, span)
                }
                ReservedWord::Match => self.parse_match_expression(),
                ReservedWord::If => self.parse_if_expression(),
                ReservedWord::For => self.parse_for_expression(),
                ReservedWord::While => self.parse_while_expression(),
                ReservedWord::Break => {
                    let span = self.next().unwrap().span;

                    self.frame_mut().unwrap().finalize_id_accesses();

                    self.push_node(Node::Break, span)
                }
                ReservedWord::Continue => {
                    let span = self.next().unwrap().span;

                    self.push_node(Node::Continue, span)
                }
                _ => Err(Error::new(
                    ErrorKind::SyntaxError(SyntaxError::ExpectedExpression),
                    *span,
                )),
            },
            Token::Operator(Operator::Minus) => {
                let span = *span;

                self.parse_negation(span)
            }
            Token::Punctuation(Punctuation::Pound) => self.parse_map_expression(),
            Token::Delimiter(delimiter) => match delimiter {
                Delimiter::BracketOpen => self.parse_array_expression(),
                Delimiter::BraceOpen => self.parse_block_expression(),
                Delimiter::ParenOpen => self.parse_arrow_fn_expression(),
                _ => Err(Error::new(
                    ErrorKind::SyntaxError(SyntaxError::ExpectedExpression),
                    *span,
                )),
            },
            _ => Err(Error::new(
                ErrorKind::SyntaxError(SyntaxError::ExpectedExpression),
                *span,
            )),
        }
    }

    fn parse_negation(&mut self, span: Span) -> Result<Positioned<AstIndex>> {
        match self.peek().map(|v| (v.span, &v.value)) {
            Some((_, Token::Literal(Literal::Number(_) | Literal::Float(_)))) => {
                self.next();

                self.parse_literal(true)
            }
            Some((span, _)) => {
                self.next();

                if let Ok(term) = self.parse_term() {
                    self.push_node(
                        Node::UnaryOp {
                            op: AstUnaryOp::Negate,
                            value: term.value,
                        },
                        term.span,
                    )
                } else {
                    Err(Error::new(
                        ErrorKind::SyntaxError(SyntaxError::ExpectedExpression),
                        span,
                    ))
                }
            }
            _ => Err(Error::new(
                ErrorKind::SyntaxError(SyntaxError::ExpectedExpression),
                span,
            )),
        }
    }

    fn _parse_type(&mut self) -> Result<Positioned<Type>> {
        let ty = if let Ok(start) = self.consume(&Token::Delimiter(Delimiter::ParenOpen)) {
            let result = self._parse_type()?;

            let end = self.consume(&Token::Delimiter(Delimiter::ParenClose))?;

            start.between(&end).wrap(result.value)
        } else {
            let result = self.parse_id()?;

            if self.check(&Token::Operator(Operator::Or)) {
                let (start_span, value) = result.unpack();
                let mut items = vec![Type::Named(value)];
                let mut end_span = start_span;

                while self.try_consume(&Token::Operator(Operator::Or)) {
                    let (span, value) = self._parse_type()?.unpack();

                    items.push(value);

                    end_span = span;
                }

                start_span.between(end_span).wrap(Type::Union(items))
            } else {
                result.span.wrap(Type::Named(result.value))
            }
        };

        Ok(
            if self.try_consume(&Token::Delimiter(Delimiter::BracketOpen)) {
                let size = self.consume_map(Token::as_usize, "number which >= 0").ok().map(|v| v.value);

                let end_span = self
                    .consume(&Token::Delimiter(Delimiter::BracketClose))?
                    .span;

                ty.span.between(end_span).wrap(Type::Array {
                    item: Box::new(ty.value),
                    size,
                })
            } else {
                ty
            },
        )
    }

    fn parse_type(&mut self) -> Result<Positioned<AstIndex>> {
        let ty = self._parse_type()?;

        self.push_node(Node::Type(ty.value), ty.span)
    }

    fn parse_arrow_fn_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let span_start = self.consume(&Token::Delimiter(Delimiter::ParenOpen))?.span;

        let mut arg_nodes = Vec::new();
        let mut arg_ids = Vec::new();
        let mut is_variadic = false;

        while !self.check(&Token::Delimiter(Delimiter::ParenClose)) {
            is_variadic = self.try_consume(&Token::Punctuation(Punctuation::Ellipsis));

            let id = self.parse_id()?;

            arg_ids.push(id.value);
            arg_nodes.push(self.push_node(Node::Id(id.value, None), id.span)?.value);

            if is_variadic {
                break;
            }
        }

        self.consume(&Token::Delimiter(Delimiter::ParenClose))?;
        self.consume(&Token::Punctuation(Punctuation::FatArrow))?;

        let mut function_frame = Frame::default();

        function_frame.ids_assigned_in_frame.extend(arg_ids.iter());

        self.frame_stack.push(function_frame);

        let body = self.parse_block_expression()?;

        let function_frame = self.frame_stack.pop().ok_or_else(|| {
            Error::new(
                ErrorKind::InternalError(InternalError::MissingFrame),
                span_start,
            )
        })?;

        self.frame_mut()?
            .add_nested_accessed_non_locals(&function_frame);

        let local_count = function_frame.local_count();

        self.push_node(
            Node::Function(Function {
                name: None,
                args: arg_nodes.into(),
                local_count,
                accessed_non_locals: Vec::from_iter(function_frame.accessed_non_locals).into(),
                body: body.value,
                is_variadic,
                output_type: None,
            }),
            span_start.between(body.span),
        )
    }

    fn parse_while_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let start_span = self
            .consume(&Token::ReservedWord(ReservedWord::While))?
            .span;

        let condition = self.parse_expression()?;
        let body = self.parse_block_expression()?;

        self.push_node(
            Node::While {
                condition: condition.value,
                body: body.value,
            },
            start_span.between(body.span),
        )
    }

    fn parse_for_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let start_span = self.consume(&Token::ReservedWord(ReservedWord::For))?.span;

        let id = self.parse_id()?;

        self.frame_mut()?.ids_assigned_in_frame.insert(id.value);

        let args = vec![self.push_node(Node::Id(id.value, None), id.span)?.value];

        self.consume(&Token::ReservedWord(ReservedWord::In))?;

        let iterable = self.parse_expression()?;

        let body = self.parse_block_expression()?;

        self.push_node(
            Node::For(AstFor {
                args: args.into(),
                iterable: iterable.value,
                body: body.value,
            }),
            start_span.between(body.span),
        )
    }

    fn parse_block_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let start_span = self.consume(&Token::Delimiter(Delimiter::BraceOpen))?.span;

        let mut block = vec![];

        while !self.check(&Token::Delimiter(Delimiter::BraceClose)) {
            block.push(self.parse_statement()?.value);
        }

        let end_span = self.consume(&Token::Delimiter(Delimiter::BraceClose))?.span;

        self.push_node(Node::Block(block.into()), start_span.between(end_span))
    }

    fn parse_if_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let if_span = self.consume(&Token::ReservedWord(ReservedWord::If))?.span;

        let condition = self.parse_expression()?;

        let then_node = self.parse_block_expression()?;

        let mut else_if_blocks = vec![];
        let mut else_node = None;

        while self.try_consume(&Token::ReservedWord(ReservedWord::Else)) {
            if self.try_consume(&Token::ReservedWord(ReservedWord::If)) {
                else_if_blocks.push((self.parse_expression()?, self.parse_block_expression()?));
            } else {
                else_node = Some(self.parse_block_expression()?);

                break;
            }
        }

        let end_span = else_node
            .as_ref()
            .map(|node| node.span)
            .or_else(|| else_if_blocks.last().map(|(_, block)| block.span))
            .unwrap_or(then_node.span);

        self.push_node(
            Node::If(AstIf {
                condition: condition.value,
                then_node: then_node.value,
                else_if_blocks: else_if_blocks
                    .into_iter()
                    .map(|(k, v)| (k.value, v.value))
                    .collect(),
                else_node: else_node.map(|node| node.value),
            }),
            if_span.between(end_span),
        )
    }

    fn parse_match_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let match_span = self
            .consume(&Token::ReservedWord(ReservedWord::Match))?
            .span;

        let expression = self.parse_expression()?.value;

        self.consume(&Token::Delimiter(Delimiter::BraceOpen))?;

        let mut arms = vec![];

        while !self.check(&Token::Delimiter(Delimiter::BraceClose)) {
            let pattern = self.parse_expression()?.value;

            self.consume(&Token::Punctuation(Punctuation::FatArrow))?;

            let expression = self.parse_expression()?.value;

            arms.push(MatchArm {
                patterns: vec![pattern].into(),
                condition: None,
                expression,
            });
        }

        let end_span = self.consume(&Token::Delimiter(Delimiter::BraceClose))?.span;

        self.push_node(
            Node::Match { expression, arms },
            match_span.between(end_span),
        )
    }

    fn parse_array_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let start_span = self
            .consume(&Token::Delimiter(Delimiter::BracketOpen))?
            .span;

        let mut entries = vec![];

        while !self.check(&Token::Delimiter(Delimiter::BracketClose)) {
            if !entries.is_empty() {
                self.consume(&Token::Punctuation(Punctuation::Comma))?;
            }

            entries.push(self.parse_expression()?.value);
        }

        let end_span = self
            .consume(&Token::Delimiter(Delimiter::BracketClose))?
            .span;

        let node = self.push_node(Node::List(entries.into()), start_span.between(end_span))?;

        self.check_for_chain_after_node(node)
    }

    fn parse_map_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let start_span = self.consume(&Token::Punctuation(Punctuation::Pound))?.span;

        self.consume(&Token::Delimiter(Delimiter::BraceOpen))?;

        let mut entries = vec![];

        while !self.check(&Token::Delimiter(Delimiter::BraceClose)) {
            if !entries.is_empty() {
                self.consume(&Token::Punctuation(Punctuation::Comma))?;
            }

            let key = self.parse_ident()?;

            let value = if self.try_consume(&Token::Punctuation(Punctuation::Colon)) {
                Some(self.parse_expression()?.value)
            } else {
                match self.ast.node(key.value).node {
                    Node::Id(id, ..) => self.frame_mut()?.add_id_access(id),
                    _ => {
                        return Err(Error::new(
                            ErrorKind::SyntaxError(SyntaxError::ExpectedMapValue),
                            key.span,
                        ))
                    }
                }

                None
            };

            entries.push((key.value, value));
        }

        let end_span = self.consume(&Token::Delimiter(Delimiter::BraceClose))?.span;

        let node = self.push_node(Node::Map(entries), start_span.between(end_span))?;

        self.check_for_chain_after_node(node)
    }

    fn parse_ident_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let id = self.parse_id()?;

        let id_node = self.push_node(Node::Id(id.value, None), id.span)?;

        self.frame_mut()?.add_id_access(id.value);

        if self.check_if(Token::is_indexer) {
            self.parse_chain(id_node)
        } else if let Ok(args) = self.parse_args(None) {
            // Args were found, so add them to a chained call
            let call_node = self.push_node(
                Node::Chain((ChainNode::Call(args.value.into()), None)),
                args.span,
            )?;

            self.push_node(
                Node::Chain((ChainNode::Root(id_node.value), Some(call_node.value))),
                id_node.span.between(call_node.span),
            )
        } else {
            Ok(id_node)
        }
    }

    fn parse_args(&mut self, consumed: Option<Span>) -> Result<Positioned<Vec<AstIndex>>> {
        let start_span = if let Some(span) = consumed {
            span
        } else {
            self.consume(&Token::Delimiter(Delimiter::ParenOpen))?.span
        };

        let mut args = Vec::new();

        while !self.check(&Token::Delimiter(Delimiter::ParenClose)) {
            if !args.is_empty() {
                self.consume(&Token::Punctuation(Punctuation::Comma))?;
            }

            args.push(self.parse_expression_with_min_precedence(3)?.value);
        }

        let end_span = self.consume(&Token::Delimiter(Delimiter::ParenClose))?.span;

        Ok(start_span.between(end_span).wrap(args))
    }

    fn parse_chain(&mut self, root: Positioned<AstIndex>) -> Result<Positioned<AstIndex>> {
        let (root_span, root) = root.unpack();

        let mut chain = Vec::new();

        chain.push((ChainNode::Root(root), root_span));

        while let Ok(token) = self.consume_one_of(&[
            Token::Punctuation(Punctuation::Dot),
            Token::Delimiter(Delimiter::BracketOpen),
            Token::Delimiter(Delimiter::ParenOpen),
        ]) {
            match token.value {
                Token::Delimiter(Delimiter::ParenOpen) => {
                    let args = self.parse_args(Some(token.span))?;

                    chain.push((ChainNode::Call(args.value.into()), args.span));
                }
                Token::Delimiter(Delimiter::BracketOpen) => {
                    let index_expression = self.parse_index_expression()?;

                    let end_span = self
                        .consume(&Token::Delimiter(Delimiter::BracketClose))?
                        .span;

                    chain.push((
                        ChainNode::Index(index_expression.value),
                        token.span.between(end_span),
                    ));
                }
                Token::Punctuation(Punctuation::Dot) => {
                    let id = self.parse_id()?;

                    chain.push((ChainNode::Id(id.value), id.span));
                }
                _ => unreachable!(),
            }
        }

        let mut next_index = None;

        for (node, span) in chain.into_iter().rev() {
            next_index = Some(self.push_node(
                Node::Chain((
                    node.clone(),
                    next_index.map(|x: Positioned<AstIndex>| x.value),
                )),
                span,
            )?);
        }

        next_index.ok_or_else(|| {
            Error::new(
                ErrorKind::InternalError(InternalError::ChainParseFailure),
                root_span,
            )
        })
    }

    fn parse_index_expression(&mut self) -> Result<Positioned<AstIndex>> {
        if let Ok(index_expression) = self.parse_expression() {
            match self.peek().map(|v| (v.span, &v.value)) {
                Some((span, Token::Operator(Operator::Range))) => {
                    self.next();

                    if let Ok(end_expression) = self.parse_expression() {
                        self.push_node(
                            Node::Range {
                                start: index_expression.value,
                                end: end_expression.value,
                                inclusive: false,
                            },
                            index_expression.span.between(end_expression.span),
                        )
                    } else {
                        self.push_node(
                            Node::RangeFrom {
                                start: index_expression.value,
                            },
                            index_expression.span.between(span),
                        )
                    }
                }
                Some((span, Token::Operator(Operator::RangeInclusive))) => {
                    self.next();

                    if let Ok(end_expression) = self.parse_expression() {
                        self.push_node(
                            Node::Range {
                                start: index_expression.value,
                                end: end_expression.value,
                                inclusive: true,
                            },
                            index_expression.span.between(end_expression.span),
                        )
                    } else {
                        self.push_node(
                            Node::RangeFrom {
                                start: index_expression.value,
                            },
                            index_expression.span.between(span),
                        )
                    }
                }
                _ => Ok(index_expression),
            }
        } else {
            match self
                .consume_one_of(&[
                    Token::Operator(Operator::Range),
                    Token::Operator(Operator::RangeInclusive),
                ])?
                .unpack()
            {
                (span, Token::Operator(Operator::Range)) => {
                    if let Ok(end_expression) = self.parse_expression() {
                        self.push_node(
                            Node::RangeTo {
                                end: end_expression.value,
                                inclusive: false,
                            },
                            span.between(end_expression.span),
                        )
                    } else {
                        self.push_node(Node::RangeFull, span)
                    }
                }
                (span, Token::Operator(Operator::RangeInclusive)) => {
                    if let Ok(end_expression) = self.parse_expression() {
                        self.push_node(
                            Node::RangeTo {
                                end: end_expression.value,
                                inclusive: true,
                            },
                            span.between(end_expression.span),
                        )
                    } else {
                        self.push_node(Node::RangeFull, span)
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    fn parse_expression(&mut self) -> Result<Positioned<AstIndex>> {
        let result = self.parse_expression_with_min_precedence(0)?;

        self.frame_mut().unwrap().finalize_id_accesses();

        Ok(result)
    }

    fn parse_expression_with_min_precedence(
        &mut self,
        min_precedence: u8,
    ) -> Result<Positioned<AstIndex>> {
        let result = self.parse_expression_start(min_precedence);

        match self.peek().map(|v| &v.value) {
            Some(Token::Operator(Operator::Range) | Token::Operator(Operator::RangeInclusive)) => {
                self.parse_range(result.ok())
            }
            _ => result,
        }
    }

    fn parse_range(&mut self, lhs: Option<Positioned<AstIndex>>) -> Result<Positioned<AstIndex>> {
        let (range_span, value) = self
            .consume_one_of(&[
                Token::Operator(Operator::Range),
                Token::Operator(Operator::RangeInclusive),
            ])?
            .unpack();

        let inclusive = value == Token::Operator(Operator::RangeInclusive);

        let rhs = self.parse_expression().ok();

        let (range_node, span) = match (lhs, rhs) {
            (Some(start), Some(end)) => (
                Node::Range {
                    start: start.value,
                    end: end.value,
                    inclusive,
                },
                start.span.between(end.span),
            ),
            (Some(start), None) => (
                Node::RangeFrom { start: start.value },
                start.span.between(range_span),
            ),
            (None, Some(end)) => (
                Node::RangeTo {
                    end: end.value,
                    inclusive,
                },
                range_span.between(end.span),
            ),
            (None, None) => (Node::RangeFull, range_span),
        };

        let range_node = self.push_node(range_node, span)?;

        self.check_for_chain_after_node(range_node)
    }

    fn check_for_chain_after_node(
        &mut self,
        node: Positioned<AstIndex>,
    ) -> Result<Positioned<AstIndex>> {
        if self.check_if(Token::is_indexer) {
            self.parse_chain(node)
        } else {
            Ok(node)
        }
    }

    fn parse_expression_start(&mut self, min_precedence: u8) -> Result<Positioned<AstIndex>> {
        let expression_start = self.parse_term()?;

        self.parse_expression_continued(expression_start, min_precedence)
    }

    fn parse_expression_continued(
        &mut self,
        expression_start: Positioned<AstIndex>,
        min_precedence: u8,
    ) -> Result<Positioned<AstIndex>> {
        if let Ok(assign_expression) = self.parse_assign_expression(expression_start.clone()) {
            return Ok(assign_expression);
        } else if let Some(Token::Operator(operator)) = self.peek().map(|v| &v.value) {
            if let Some((left_priority, right_priority)) = operator_precedence(operator) {
                if left_priority >= min_precedence {
                    let Token::Operator(op) = self.next().unwrap().value else {
                        unreachable!()
                    };

                    let rhs = self.parse_expression_start(right_priority)?;

                    let ast_op = match op {
                        Operator::Plus => AstBinaryOp::Add,
                        Operator::Minus => AstBinaryOp::Subtract,
                        Operator::Star => AstBinaryOp::Multiply,
                        Operator::Slash => AstBinaryOp::Divide,
                        Operator::Percent => AstBinaryOp::Remainder,

                        Operator::PlusEq => AstBinaryOp::AddAssign,
                        Operator::MinusEq => AstBinaryOp::SubtractAssign,
                        Operator::StarEq => AstBinaryOp::MultiplyAssign,
                        Operator::SlashEq => AstBinaryOp::DivideAssign,
                        Operator::PercentEq => AstBinaryOp::RemainderAssign,

                        Operator::EqEq => AstBinaryOp::Equal,
                        Operator::Ne => AstBinaryOp::NotEqual,

                        Operator::Gt => AstBinaryOp::Greater,
                        Operator::Ge => AstBinaryOp::GreaterOrEqual,
                        Operator::Lt => AstBinaryOp::Less,
                        Operator::Le => AstBinaryOp::LessOrEqual,

                        Operator::AndAnd => AstBinaryOp::And,
                        Operator::OrOr => AstBinaryOp::Or,

                        _ => unreachable!(), // The list of tokens here matches the operators in
                                             // operator_precedence()
                    };

                    let op_node = self.push_node(
                        Node::BinaryOp {
                            op: ast_op,
                            lhs: expression_start.value,
                            rhs: rhs.value,
                        },
                        expression_start.span.between(rhs.span),
                    )?;

                    return self.parse_expression_continued(op_node, min_precedence);
                }
            }
        }

        Ok(expression_start)
    }

    fn parse_literal(&mut self, negate: bool) -> Result<Positioned<AstIndex>> {
        let literal = self.consume_map_if(Token::is_literal, Token::into_literal, "literal")?;

        let node = match literal.value {
            Literal::String(value) => Node::Str(AstString {
                contents: StringContents::Literal(
                    self.add_string_constant(literal.span.wrap(value))?.value,
                ),
            }),
            Literal::Number(value) => Node::Int(
                self.constants
                    .add_i64(if negate { -value } else { value })
                    .map_err(|_| {
                        Error::new(
                            ErrorKind::InternalError(InternalError::ConstantPoolCapacityOverflow),
                            literal.span,
                        )
                    })?,
            ),
            Literal::Float(value) => Node::Float(
                self.constants
                    .add_f64(if negate { -value } else { value })
                    .map_err(|_| {
                        Error::new(
                            ErrorKind::InternalError(InternalError::ConstantPoolCapacityOverflow),
                            literal.span,
                        )
                    })?,
            ),
            Literal::Boolean(value) => {
                if value {
                    Node::BoolTrue
                } else {
                    Node::BoolFalse
                }
            }
        };

        let name = node.variant_name();
        let node = self.push_node(node, literal.span)?;

        if ["Int", "Float", "Str"].contains(&name) {
            self.check_for_chain_after_node(node)
        } else {
            Ok(node)
        }
    }

    fn parse_use_statement(&mut self) -> Result<Positioned<AstIndex>> {
        let start_span = self.consume(&Token::ReservedWord(ReservedWord::Use))?.span;

        let mut items = vec![];

        if self.try_consume(&Token::Delimiter(Delimiter::BraceOpen)) {
            while !self.check(&Token::Delimiter(Delimiter::BraceClose)) {
                if !items.is_empty() {
                    self.consume(&Token::Punctuation(Punctuation::Comma))?;
                }

                let item = self.parse_ident()?.value;
                let name = self.parse_ident().map(|v| v.value).ok();

                items.push(UseItem { item, name });
            }

            self.consume(&Token::Delimiter(Delimiter::BraceClose))?;
            self.consume(&Token::ReservedWord(ReservedWord::From))?;
        }

        let mut path = self.parse_raw_ident()?;

        while self.try_consume(&Token::Operator(Operator::Slash)) {
            let Positioned { value, span } = self.parse_raw_ident()?;

            path.value.push('.');
            path.value.push_str(&value);
            path.span = path.span.between(span);
        }

        let path = self.parse_ident_from(path)?;
        let alias = if items.is_empty() {
            self.parse_ident().map(|v| v.value).ok()
        } else {
            None
        };

        let end_span = self.consume(&Token::Punctuation(Punctuation::Semi))?.span;

        let (from, items) = if items.is_empty() {
            (
                vec![],
                vec![UseItem {
                    item: path.value,
                    name: alias,
                }],
            )
        } else {
            (vec![path.value], items)
        };

        for item in items.iter() {
            let maybe_id = if let Node::Id(id, ..) = &self.ast.node(item.item).node {
                Some(*id)
            } else {
                None
            };

            let maybe_as =
                if let Some(Node::Id(id, ..)) = item.name.map(|node| &self.ast.node(node).node) {
                    Some(*id)
                } else {
                    None
                };

            if let (Some(id), None) | (_, Some(id)) = (maybe_id, maybe_as) {
                self.frame_mut()?.ids_assigned_in_frame.insert(id);
            }
        }

        self.push_node(
            Node::Use {
                from: from.into(),
                items,
            },
            start_span.between(end_span),
        )
    }

    fn parse_id(&mut self) -> Result<Positioned<ConstantIndex>> {
        let item = self.parse_raw_ident()?;

        self.add_string_constant(item)
    }

    fn parse_ident(&mut self) -> Result<Positioned<AstIndex>> {
        let id = self.parse_id()?;

        self.push_node(Node::Id(id.value, None), id.span)
    }

    fn parse_ident_from(&mut self, id: Positioned<String>) -> Result<Positioned<AstIndex>> {
        let id = self.add_string_constant(id)?;

        self.push_node(Node::Id(id.value, None), id.span)
    }

    fn parse_raw_ident(&mut self) -> Result<Positioned<String>> {
        self.consume_map_if(Token::is_ident, Token::into_ident, "identifier")
    }
}

impl Parser {
    pub fn parse(source: &str) -> Result<Ast> {
        let capacity_guess = source.len() / 4;

        let mut parser = Parser {
            ast: Ast::with_capacity(capacity_guess),
            constants: ConstantPoolBuilder::default(),
            tokens: Lexer::parse(&source).unwrap().1.into_iter().peekable(),
            frame_stack: Vec::new(),
        };

        parser.consume_main_block()?;
        parser.ast.set_constants(parser.constants.build());

        Ok(parser.ast)
    }

    fn add_string_constant(&mut self, s: Positioned<String>) -> Result<Positioned<ConstantIndex>> {
        match self.constants.add_string(&s.value) {
            Ok(result) => Ok(s.span.wrap(result)),
            Err(_) => Err(Error::new(
                ErrorKind::InternalError(InternalError::ConstantPoolCapacityOverflow),
                s.span,
            )),
        }
    }

    fn push_node(&mut self, node: Node, span: Span) -> Result<Positioned<AstIndex>> {
        self.ast.push(node, span).map(|v| span.wrap(v))
    }

    fn frame(&self) -> Result<&Frame> {
        match self.frame_stack.last() {
            Some(frame) => Ok(frame),
            None => Err(Error::new(
                InternalError::MissingFrame.into(),
                Span::default(),
            )),
        }
    }

    fn frame_mut(&mut self) -> Result<&mut Frame> {
        match self.frame_stack.last_mut() {
            Some(frame) => Ok(frame),
            None => Err(Error::new(
                InternalError::MissingFrame.into(),
                Span::default(),
            )),
        }
    }

    fn position(&mut self) -> Span {
        self.peek().map(|x| x.span).unwrap_or_default()
    }

    /// Consumes the current token only if it exists and is equal to `value`.
    pub fn try_consume(&mut self, value: &Token) -> bool {
        if self.peek().is_some_and(|v| &v.value == value) {
            self.next();

            true
        } else {
            false
        }
    }

    /// Checks if the next token exists and it is equal to `value`.
    pub fn check(&mut self, value: &Token) -> bool {
        self.peek().is_some_and(|v| &v.value == value)
    }

    /// Returns the `bool` result of `func` if the next token exists.
    pub fn check_if<F: Fn(&Token) -> bool>(&mut self, func: F) -> bool {
        self.peek().is_some_and(|v| func(&v.value))
    }

    /// Consumes the current token if it exists and is equal to `value`, otherwise returning `ParseError`.
    pub fn consume(&mut self, value: &Token) -> Result<Positioned<Token>> {
        if let Some(value) = self.next_if(|v| v == value) {
            Ok(value)
        } else {
            Err(Error::new(
                ErrorKind::SyntaxError(SyntaxError::ExpectedToken(
                    self.peek().unwrap().value.to_string(),
                    value.to_string(),
                )),
                self.position(),
            ))
        }
    }

    /// Consumes the current token if it exists and is equal to `value`, otherwise returning `ParseError`.
    pub fn consume_map<T, F: Fn(&Token) -> Option<T>, E: Into<String>>(&mut self, func: F, expected: E) -> Result<Positioned<T>> {
        if let Some(value) = self
            .peek()
            .and_then(|v| func(&v.value).map(|value| v.span.wrap(value)))
        {
            Ok(value)
        } else {
            Err(Error::new(
                ErrorKind::SyntaxError(SyntaxError::ExpectedToken(
                    self.peek().unwrap().value.to_string(),
                    expected.into(),
                )),
                self.position(),
            ))
        }
    }

    /// Consumes the current token if it exists and is equal to one of the values inside `values`, otherwise returning `ParseError`.
    pub fn consume_one_of(&mut self, values: &[Token]) -> Result<Positioned<Token>> {
        if let Some(value) = self.next_if(|value| values.contains(value)) {
            Ok(value)
        } else {
            let expected = match values.len() {
                0 => unreachable!(),
                1 => values.first().unwrap().to_string(),
                _ => {
                    let mut iterator = values.iter();
                    let last = iterator.next_back().unwrap().to_string();
                    let tokens = iterator
                        .map(|token| token.to_string())
                        .collect::<Vec<_>>()
                        .join(", ");

                    format!("{tokens} or {last}")
                }
            };

            Err(Error::new(
                ErrorKind::SyntaxError(SyntaxError::ExpectedToken(
                    self.peek().unwrap().value.to_string(),
                    expected,
                )),
                self.position(),
            ))
        }
    }

    /// Consumes the current token if it exists and the result of the `func` is `Some(T)`, otherwise returning `ParseError`.
    pub fn consume_map_if<T, C: Fn(&Token) -> bool, F: Fn(Token) -> T, E: Into<String>>(
        &mut self,
        check: C,
        func: F,
        expected: E,
    ) -> Result<Positioned<T>> {
        if let Some(Positioned { value, span }) = self.next_if(check) {
            Ok(span.wrap(func(value)))
        } else {
            Err(Error::new(
                ErrorKind::SyntaxError(SyntaxError::ExpectedToken(
                    self.peek().unwrap().value.to_string(),
                    expected.into(),
                )),
                self.position(),
            ))
        }
    }

    /// Consumes the current token and returns it wrapped in `Some` if it exists, otherwise returning `None`.
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<Positioned<Token>> {
        self.tokens.next()
    }

    /// Peeks the current token and returns a reference to it wrapped in `Some` if it exists, otherwise returning `None`.
    pub fn peek(&mut self) -> Option<&Positioned<Token>> {
        self.tokens.peek()
    }

    /// Consumes the current token and returns it wrapped in `Some` if the result of the `func` function is `true`, otherwise returning `None`.
    pub fn next_if<F: Fn(&Token) -> bool>(&mut self, func: F) -> Option<Positioned<Token>> {
        if self.check_if(func) {
            self.next()
        } else {
            None
        }
    }
}
