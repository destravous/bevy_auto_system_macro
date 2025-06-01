use proc_macro2::{Group, Ident, Span, TokenStream, TokenTree};
use quote::TokenStreamExt;
use std::collections::{HashSet};

#[derive(Debug, Clone)]
pub enum SysOp {
    Query {
        name: Ident,
    },
    Resource {
        type_param: TokenStream,
        mutable: bool,
    },
    Commands(Ident),
}

impl SysOp {
    pub fn query_name(&self) -> Option<String> {
        match self {
            SysOp::Query { name, .. } => Some(name.to_string()),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum BracketContent {
    None,
    Empty,  // For iteration []
    Expression(TokenStream),  // For lookup [expr]
}

#[derive(Debug)]
pub struct Chain {
    pub start_span: Span,
    pub sys_op: SysOp,
    pub operations: Vec<ChainOperation>,
    pub bracket_content: BracketContent,
    pub chain_id: usize,
    // Does this chain in some way associate with another?
    pub parent: Option<(usize, String)>,
    pub child: Option<(usize, String)>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum MethodKind {
    // Accessing methods
    Get,
    GetMut,
    TryGet,
    TryGetMut,
    Has,

    // Filtering methods
    Is,
    IsNot,
    Added,
    Changed,
    Filter,

    // Relationship traversal
    Via,  // Changed from Relation

    // Entity commands
    Command,  // New variant
}

#[derive(Debug)]
pub struct ChainOperation {
    pub method_kind: MethodKind,
    pub type_param: Option<Ident>,
    pub complex_type: Option<TokenStream>,
}

pub struct ParsedChains {
    pub chains: Vec<Chain>,
    pub entity_names: HashSet<String>,
    next_id: usize,
}

impl ParsedChains {
    pub fn new() -> Self {
        Self {
            chains: vec![],
            entity_names: HashSet::new(),
            next_id: 0,
        }
    }

    fn add_chain(&mut self, mut chain: Chain) -> usize {
        chain.chain_id = self.next_id;
        self.next_id += 1;
        let id = chain.chain_id;

        // Track entity names
        if let SysOp::Query { ref name, .. } = chain.sys_op {
            self.entity_names.insert(name.to_string());
        }

        self.chains.push(chain);
        id
    }
}

struct TokenParser {
    tokens: Vec<TokenTree>,
    pos: usize,
}

impl TokenParser {
    fn new(stream: TokenStream) -> Self {
        TokenParser {
            tokens: stream.into_iter().collect(),
            pos: 0,
        }
    }

    fn peek(&self) -> Option<&TokenTree> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) -> Option<TokenTree> {
        if self.pos < self.tokens.len() {
            let token = self.tokens[self.pos].clone();
            self.pos += 1;
            Some(token)
        } else {
            None
        }
    }

    fn current_pos(&self) -> usize {
        self.pos
    }

    fn reset_to(&mut self, pos: usize) {
        self.pos = pos;
    }

    fn try_parse_entity_chain(&mut self) -> Option<Chain> {
        // Must start with #
        let start_span = match self.peek() {
            Some(TokenTree::Punct(p)) if p.as_char() == '#' => {
                let span = p.span();
                self.advance();
                span
            }
            _ => return None,
        };

        // Parse the system operation
        let sys_op = self.parse_sys_op()?;

        // Parse operations
        let mut operations = vec![];

        loop {
            let before_dot_pos = self.current_pos();

            // Check for dot
            match self.peek() {
                Some(TokenTree::Punct(p)) if p.as_char() == '.' => {
                    self.advance();
                }
                _ => break,
            }

            // Parse operation
            if let Some(op) = self.parse_chain_operation() {
                operations.push(op);
            } else {
                self.reset_to(before_dot_pos);
                break;
            }
        }

        // Check for brackets
        let bracket_content = self.check_brackets();

        Some(Chain {
            start_span,
            sys_op,
            operations,
            bracket_content,
            parent: None,
            child: None,
            chain_id: 0,
        })
    }

    fn parse_sys_op(&mut self) -> Option<SysOp> {
        match self.peek() {
            Some(TokenTree::Ident(id)) => {
                let name = id.to_string();
                let ident = id.clone();
                self.advance();

                match name.as_str() {
                    "commands" => Some(SysOp::Commands(ident)),
                    "get_resource" => {
                        if let Some(type_param) = self.parse_complex_type_param() {
                            Some(SysOp::Resource {
                                type_param,
                                mutable: false,
                            })
                        } else {
                            None
                        }
                    }
                    "get_resource_mut" => {
                        if let Some(type_param) = self.parse_complex_type_param() {
                            Some(SysOp::Resource {
                                type_param,
                                mutable: true,
                            })
                        } else {
                            None
                        }
                    }
                    _ => {
                        // All other identifiers are query names
                        Some(SysOp::Query {
                            name: ident,
                        })
                    }
                }
            }
            _ => None,
        }
    }

    fn parse_chain_operation(&mut self) -> Option<ChainOperation> {
        let op_name = match self.advance() {
            Some(TokenTree::Ident(id)) => id,
            _ => return None,
        };

        let method_kind = match op_name.to_string().as_str() {
            "get" => MethodKind::Get,
            "get_mut" => MethodKind::GetMut,
            "try_get" => MethodKind::TryGet,
            "try_get_mut" => MethodKind::TryGetMut,
            "has" => MethodKind::Has,
            "is" => MethodKind::Is,
            "is_not" => MethodKind::IsNot,
            "added" => MethodKind::Added,
            "changed" => MethodKind::Changed,
            "filter" => MethodKind::Filter,
            "via" => MethodKind::Via,  // Changed from "r"
            "command" => MethodKind::Command,  // New
            _ => return None,
        };

        let (type_param, complex_type) = if matches!(method_kind, MethodKind::Filter) {
            (None, self.parse_complex_type_param())
        } else if matches!(method_kind, MethodKind::Command) {
            // command() method - expect parentheses but no type param
            self.parse_parenthesized_expr(); // Consume the ()
            (None, None)
        } else {
            (self.parse_type_param(), None)
        };

        Some(ChainOperation {
            method_kind,
            type_param,
            complex_type,
        })
    }

    fn parse_parenthesized_expr(&mut self) -> Option<TokenStream> {
        match self.peek() {
            Some(TokenTree::Group(g))
            if matches!(g.delimiter(), proc_macro2::Delimiter::Parenthesis) =>
                {
                    let result = Some(g.stream());
                    self.advance();
                    result
                }
            _ => None,
        }
    }

    fn parse_type_param(&mut self) -> Option<Ident> {
        match self.peek() {
            Some(TokenTree::Punct(p)) if p.as_char() == '<' => {
                self.advance();

                let type_name = match self.advance() {
                    Some(TokenTree::Ident(id)) => id,
                    _ => return None,
                };

                match self.peek() {
                    Some(TokenTree::Punct(p)) if p.as_char() == '>' => {
                        self.advance();
                        Some(type_name)
                    }
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn parse_complex_type_param(&mut self) -> Option<TokenStream> {
        match self.peek() {
            Some(TokenTree::Punct(p)) if p.as_char() == '<' => {
                self.advance();
                let (tokens, success) = self.collect_until_closing_angle();
                if success {
                    Some(tokens)
                } else {
                    None
                }
            }
            _ => None,
        }
    }

    fn collect_until_closing_angle(&mut self) -> (TokenStream, bool) {
        let mut angle_depth = 1;
        let mut collected = TokenStream::new();

        while angle_depth > 0 {
            match self.advance() {
                Some(TokenTree::Punct(p)) if p.as_char() == '<' => {
                    angle_depth += 1;
                    collected.append(TokenTree::Punct(p));
                }
                Some(TokenTree::Punct(p)) if p.as_char() == '>' => {
                    angle_depth -= 1;
                    if angle_depth > 0 {
                        collected.append(TokenTree::Punct(p));
                    }
                }
                Some(token) => collected.append(token),
                None => return (collected, false),
            }
        }

        (collected, true)
    }

    fn check_brackets(&mut self) -> BracketContent {
        match self.peek() {
            Some(TokenTree::Group(g))
            if matches!(g.delimiter(), proc_macro2::Delimiter::Bracket) => {
                let content = g.stream();
                self.advance();

                if content.is_empty() {
                    BracketContent::Empty
                } else {
                    BracketContent::Expression(content)
                }
            }
            _ => BracketContent::None,
        }
    }
}

/// Replaces all # chains with placeholders; extracts chain information for later processing.
pub fn transform_custom_syntax(input: TokenStream, chains: &mut ParsedChains) -> TokenStream {
    let mut output = TokenStream::new();
    let mut parser = TokenParser::new(input);

    // First pass: parse chains and insert placeholders
    while parser.pos < parser.tokens.len() {
        if let Some(chain) = parser.try_parse_entity_chain() {
            let start_span = chain.start_span;
            let chain_id = chains.add_chain(chain);

            let placeholder = Ident::new(&format!("__bevy_chain_{}", chain_id), start_span);
            output.append(placeholder);
        } else {
            if let Some(token) = parser.advance() {
                match token {
                    TokenTree::Group(group) => {
                        let inner_stream = transform_custom_syntax(group.stream(), chains);
                        let mut new_group = Group::new(group.delimiter(), inner_stream);
                        new_group.set_span(group.span());
                        output.append(TokenTree::Group(new_group));
                    }
                    other => {
                        output.append(other);
                    }
                }
            }
        }
    }

    // Second pass: detect for patterns in the transformed output
    associate_for_chains(&output, chains);

    output
}

fn associate_for_chains(tokens: &TokenStream, chains: &mut ParsedChains) {
    let token_vec: Vec<_> = tokens.clone().into_iter().collect();
    let mut i = 0;

    while i < token_vec.len() {
        if let TokenTree::Ident(id) = &token_vec[i] {
            if id.to_string() == "for" && i + 3 < token_vec.len() {
                // Check for pattern: for __bevy_chain_X in __bevy_chain_Y
                try_extract_for_pattern(&token_vec[i+1..], chains);
            }
        }

        // Recurse into groups
        if let TokenTree::Group(g) = &token_vec[i] {
            associate_for_chains(&g.stream(), chains);
        }

        i += 1;
    }
}

fn try_extract_for_pattern(tokens: &[TokenTree], chains: &mut ParsedChains){
    if tokens.len() < 3 {
        return;
    }

    // Extract entity chain id and name
    let (first_chain_id, first_name) = if let TokenTree::Ident(id) = &tokens[0] {
        let ident_str = id.to_string();
        if ident_str.starts_with("__bevy_chain_") {
            let Some(chain_id) = ident_str.trim_start_matches("__bevy_chain_").parse::<usize>().ok() else{return};
            let Some(chain) = chains.chains.get(chain_id) else {return};
            let Some(name) = chain.sys_op.query_name() else {return};
            (chain_id, name)
        } else {
            return;
        }
    } else {
        return;
    };

    // Check for "in"
    if let TokenTree::Ident(id) = &tokens[1] {
        if id.to_string() != "in" {
            return;
        }
    } else {
        return;
    }

    // Extract iterator chain id and name
    if let TokenTree::Ident(id) = &tokens[2] {
        let ident_str = id.to_string();
        if ident_str.starts_with("__bevy_chain_") {
            let Some(target_id) = ident_str.trim_start_matches("__bevy_chain_").parse::<usize>().ok() else {return};
            let Some(chain) = chains.chains.get(target_id) else {return};
            let Some(name) = chain.sys_op.query_name() else {return};
            chains.chains.get_mut(first_chain_id).unwrap().parent = Some((target_id, name));
            chains.chains.get_mut(target_id).unwrap().child = Some((first_chain_id, first_name));
        }
    }
}