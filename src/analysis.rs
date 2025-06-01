use std::collections::{HashMap};
use proc_macro2::{Span};
use syn::spanned::Spanned;
use crate::parser::{BracketContent, Chain, MethodKind, ParsedChains, SysOp};

// Core data structures for amortized information
#[derive(Debug)]
pub struct AmortizedData {
    pub queries: HashMap<String, QueryData>,
    pub canonical_name_cache: HashMap<usize, String>,
    pub needs_commands: bool,
}

#[derive(Debug)]
pub struct QueryData {
    pub canonical_name: String,
    pub components: HashMap<String, ComponentAccess>,
    pub filters: HashMap<Filter, Span>,
    pub needs_entity_id: bool,
    pub has_lookups: bool,
}

#[derive(Debug)]
pub struct ComponentAccess {
    pub needs_read: bool,
    pub needs_write: bool,
    pub is_optional: bool,  // true if accessed via try_get/try_get_mut
    pub span: Span,
}

#[derive(Debug, Hash, Eq, PartialEq)]
pub enum Filter {
    With(String),
    Without(String),
    Added(String),
    Changed(String),
    Custom(String), // for .filter<T>
}

fn is_loop_variable(chain_idx: usize, chains: &ParsedChains) -> bool {
    let chain = &chains.chains[chain_idx];
    if let Some((parent_idx, _)) = &chain.parent {
        if let Some((child_idx, _)) = &chains.chains[*parent_idx].child {
            return *child_idx == chain_idx;
        }
    }
    false
}

// Helper to check if a chain contains a via operation
fn chain_has_via(chain: &Chain) -> bool {
    chain.operations.iter().find(|op| matches!(op.method_kind, MethodKind::Via)).is_some()
}

// Recursively determine the canonical query name for a chain
fn get_canonical_query_name(chain_idx: usize, chains: &ParsedChains, cache: &mut HashMap<usize, String>) -> Option<String> {
    if let Some(cached) = cache.get(&chain_idx) {
        return Some(cached.clone());
    }

    let chain = &chains.chains[chain_idx];

    let canonical = match &chain.sys_op {
        SysOp::Query { name } => {
            // First check if this chain has a parent
            if let Some((parent_idx, _)) = &chain.parent {
                let parent_chain = &chains.chains[*parent_idx];
                if chain_has_via(parent_chain) {
                    // Via child starts a new query group
                    Some(name.to_string())
                } else {
                    // Non-via child uses parent's canonical name
                    get_canonical_query_name(*parent_idx, chains, cache)
                }
            } else {
                // No parent - might be a root chain or a reference to a loop variable
                // Check if there's a loop variable with the same name
                for (idx, other_chain) in chains.chains.iter().enumerate() {
                    if idx != chain_idx && is_loop_variable(idx, chains) {
                        if let SysOp::Query { name: other_name } = &other_chain.sys_op {
                            if other_name.to_string() == name.to_string() {
                                // Found a loop variable with the same name
                                // Use its canonical name
                                if let Some(canonical_name) = get_canonical_query_name(idx, chains, cache) {
                                    cache.insert(chain_idx, canonical_name.clone());
                                    return Some(canonical_name);
                                }
                            }
                        }
                    }
                }
                // No loop variable found, use own name
                Some(name.to_string())
            }
        }
        _ => None,
    };

    if let Some(ref name) = canonical {
        cache.insert(chain_idx, name.clone());
    }

    canonical
}

// Process a single chain's operations and add to query data
fn merge_chain_ops(chain: &Chain, query_data: &mut QueryData) {
    for op in &chain.operations {
        match op.method_kind {
            MethodKind::Get => {
                if let Some(ref type_name) = op.type_param {
                    let access = query_data.components.entry(type_name.to_string()).or_insert(ComponentAccess {
                        needs_read: false,
                        needs_write: false,
                        is_optional: true,
                        span: type_name.span()
                    });
                    access.needs_read = true;
                    access.is_optional = false;
                }
            }
            MethodKind::GetMut => {
                if let Some(ref type_name) = op.type_param {
                    let access = query_data.components.entry(type_name.to_string()).or_insert(ComponentAccess {
                        needs_read: false,
                        needs_write: false,
                        is_optional: true,
                        span: type_name.span()
                    });
                    access.needs_write = true;
                    access.is_optional = false;
                }
            }
            MethodKind::TryGet => {
                if let Some(ref type_name) = op.type_param {
                    let access = query_data.components.entry(type_name.to_string()).or_insert(ComponentAccess {
                        needs_read: false,
                        needs_write: false,
                        is_optional: true,
                        span: type_name.span()
                    });
                    access.needs_read = true;
                    // Keep is_optional as true
                }
            }
            MethodKind::TryGetMut => {
                if let Some(ref type_name) = op.type_param {
                    let access = query_data.components.entry(type_name.to_string()).or_insert(ComponentAccess {
                        needs_read: false,
                        needs_write: false,
                        is_optional: true,
                        span: type_name.span()
                    });
                    access.needs_write = true;
                    // Keep is_optional as true
                }
            }
            MethodKind::Is => {
                if let Some(ref type_name) = op.type_param {
                    query_data.filters.insert(Filter::With(type_name.to_string()), type_name.span());
                }
            }
            MethodKind::IsNot => {
                if let Some(ref type_name) = op.type_param {
                    query_data.filters.insert(Filter::Without(type_name.to_string()), type_name.span());
                }
            }
            MethodKind::Added => {
                if let Some(ref type_name) = op.type_param {
                    query_data.filters.insert(Filter::Added(type_name.to_string()), type_name.span());
                }
            }
            MethodKind::Changed => {
                if let Some(ref type_name) = op.type_param {
                    query_data.filters.insert(Filter::Changed(type_name.to_string()), type_name.span());
                }
            }
            MethodKind::Filter => {
                if let Some(ref complex_type) = op.complex_type {
                    query_data.filters.insert(Filter::Custom(complex_type.to_string()), complex_type.span());
                }
            }
            MethodKind::Via => {
                if let Some(ref type_name) = op.type_param {
                    let access = query_data.components.entry(type_name.to_string()).or_insert(ComponentAccess {
                        needs_read: false,
                        needs_write: false,
                        is_optional: true,
                        span: type_name.span()
                    });
                    access.needs_read = true;
                    access.is_optional = false;
                }
            }
            MethodKind::Command => {
                // Commands are handled at the system level
            }
            MethodKind::Has => {
                // Has doesn't add to query requirements
            }
        }
    }

    // Entity ID is always needed for queries
    query_data.needs_entity_id = true;
}

// Main function to amortize all chain information
pub fn amortize_chains(chains: &ParsedChains) -> AmortizedData {
    let mut queries: HashMap<String, QueryData> = HashMap::new();
    let mut needs_commands = false;
    let mut canonical_name_cache = HashMap::new();

    // First pass: identify all queries and their canonical names
    for (idx, chain) in chains.chains.iter().enumerate() {
        match &chain.sys_op {
            SysOp::Query { .. } => {
                if let Some(canonical_name) = get_canonical_query_name(idx, chains, &mut canonical_name_cache) {
                    queries.entry(canonical_name.clone()).or_insert_with(|| QueryData {
                        canonical_name: canonical_name.clone(),
                        components: HashMap::new(),
                        filters: HashMap::new(),
                        needs_entity_id: true,
                        has_lookups: false,
                    });
                }
            }
            SysOp::Resource { type_param: _, mutable: _ } => {
                // currently collected by codegen
            }
            SysOp::Commands(_) => {
                needs_commands = true;
            }
        }
    }

    // Second pass: process operations for each chain
    for (idx, chain) in chains.chains.iter().enumerate() {
        if let Some(canonical_name) = canonical_name_cache.get(&idx) {
            let query_data = queries.get_mut(canonical_name).unwrap();

            // Check if this chain is used for lookup
            if matches!(chain.bracket_content, BracketContent::Expression(_)) {
                query_data.has_lookups = true;
            }

            merge_chain_ops(chain, query_data);
        }
    }

    // Third pass: check for command usage on entity chains
    for chain in &chains.chains {
        if chain.operations.iter().any(|op| matches!(op.method_kind, MethodKind::Command)) {
            needs_commands = true;
        }
    }

    AmortizedData {
        queries,
        canonical_name_cache,
        needs_commands,
    }
}