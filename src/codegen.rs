use proc_macro2::{Ident, Span, TokenStream, TokenTree, Group, Delimiter};
use quote::{quote, quote_spanned, TokenStreamExt};
use std::collections::{HashMap, HashSet};
use crate::parser::{ParsedChains, Chain, BracketContent, MethodKind, SysOp};
use crate::analysis::{AmortizedData, QueryData, ComponentAccess, Filter};
use crate::util::{to_pascal_case, to_snake_case};

// Main entry point for generating the token streams
pub fn generate_final_tokens(
    body_tokens: TokenStream,
    chains: &ParsedChains,
    amortized: &AmortizedData,
) -> (TokenStream, TokenStream) {
    let ordered = build_ordered_info(chains, &amortized.canonical_name_cache);

    // Generate function parameters
    let params = generate_params(&ordered, amortized);

    // Generate body with view structs
    let body = generate_body(body_tokens, chains, amortized);

    (params, body)
}

// First, let's build the ordered information we need
#[derive(Debug)]
pub struct OrderedInfo {
    pub(crate) query_components: HashMap<String, Vec<String>>, // canonical_name -> ordered components
    pub(crate) parameter_order: Vec<ParameterInfo>,
}

#[derive(Debug)]
pub enum ParameterInfo {
    Query(String), // canonical name
    Resource { type_name: TokenStream, mutable: bool },
}

// Could probably rework analysis to do this instead of using hashmaps
// at some point in the future.
pub fn build_ordered_info(chains: &ParsedChains, canonical_cache: &HashMap<usize, String>) -> OrderedInfo {
    let mut query_components: HashMap<String, Vec<String>> = HashMap::new();
    let mut parameter_order = Vec::new();
    let mut seen_queries = HashSet::new();
    let mut seen_resources = HashSet::new();

    // Iterate through chains in order to preserve first appearance
    for chain in &chains.chains {
        match &chain.sys_op {
            SysOp::Query { .. } => {
                if let Some(canonical_name) = canonical_cache.get(&chain.chain_id) {
                    // Track parameter order
                    if !seen_queries.contains(canonical_name) {
                        seen_queries.insert(canonical_name.clone());
                        parameter_order.push(ParameterInfo::Query(canonical_name.clone()));
                    }

                    // Track component order
                    let components = query_components.entry(canonical_name.clone())
                        .or_insert_with(Vec::new);

                    // Add components from this chain's operations
                    for op in &chain.operations {
                        if matches!(op.method_kind, MethodKind::Get | MethodKind::GetMut | 
                                   MethodKind::TryGet | MethodKind::TryGetMut | MethodKind::Via) {
                            if let Some(type_param) = &op.type_param {
                                let type_name = type_param.to_string();
                                if !components.contains(&type_name) {
                                    components.push(type_name);
                                }
                            }
                        }
                    }
                }
            }
            SysOp::Resource { type_param, mutable } => {
                let type_name = type_param.to_string();
                if !seen_resources.contains(&type_name) {
                    seen_resources.insert(type_name.clone());
                    parameter_order.push(ParameterInfo::Resource {
                        type_name: type_param.clone(),
                        mutable: *mutable,
                    });
                }
            }
            SysOp::Commands(_) => {
                // Commands are added at the end
            }
        }
    }

    OrderedInfo { query_components, parameter_order }
}

// Generate function parameters
pub fn generate_params(
    ordered: &OrderedInfo,
    amortized: &AmortizedData,
) -> TokenStream {
    let mut params = Vec::new();

    // Add queries and resources in order
    for param_info in &ordered.parameter_order {
        match param_info {
            ParameterInfo::Query(canonical_name) => {
                if let Some(query_data) = amortized.queries.get(canonical_name) {
                    let param_name = Ident::new(
                        &format!("{}_query", canonical_name),
                        Span::call_site()
                    );

                    // Determine if we need mut
                    let needs_mut = query_data.components.values()
                        .any(|c| c.needs_write);

                    let query_type = generate_query_type(query_data, &ordered.query_components);

                    if needs_mut {
                        params.push(quote! { mut #param_name: #query_type });
                    } else {
                        params.push(quote! { #param_name: #query_type });
                    }
                }
            }
            ParameterInfo::Resource { type_name, mutable } => {
                let type_ident = type_name;
                let param_name = Ident::new(
                    &to_snake_case(&*type_name.to_string()),
                    Span::call_site()
                );

                if *mutable {
                    params.push(quote! { mut #param_name: bevy::ecs::system::ResMut<#type_ident> });
                } else {
                    params.push(quote! { #param_name: bevy::ecs::system::Res<#type_ident> });
                }
            }
        }
    }

    // Add commands if needed
    if amortized.needs_commands {
        params.push(quote! { mut commands: bevy::ecs::system::Commands });
    }

    quote! { #(#params),* }
}

// Generate Query type
fn generate_query_type(
    query_data: &QueryData,
    ordered_components: &HashMap<String, Vec<String>>,
) -> TokenStream {
    // Build component tuple type
    let mut component_types = vec![quote! { bevy::ecs::entity::Entity }];

    // Get components in order
    if let Some(ordered) = ordered_components.get(&query_data.canonical_name) {
        for comp_name in ordered {
            if let Some(comp_access) = query_data.components.get(comp_name) {
                let type_ident = Ident::new(comp_name, comp_access.span);
                let comp_type = generate_component_type(&type_ident, comp_access);
                component_types.push(comp_type);
            }
        }
    }

    let component_tuple = quote! { (#(#component_types),*) };

    // Build filter type
    let filter_type = generate_filter_type(&query_data.filters);

    // Build Query type
    if query_data.filters.is_empty() {
        quote! { bevy::ecs::system::Query<#component_tuple> }
    } else {
        quote! { bevy::ecs::system::Query<#component_tuple, #filter_type> }
    }
}

fn generate_component_type(type_ident: &Ident, access: &ComponentAccess) -> TokenStream {
    match (access.is_optional, access.needs_write) {
        (true, true) => quote! { Option<&mut #type_ident> },
        (true, false) => quote! { Option<&#type_ident> },
        (false, true) => quote! { &mut #type_ident },
        (false, false) => quote! { &#type_ident },
    }
}

fn generate_filter_type(filters: &HashMap<Filter, Span>) -> TokenStream {
    let filter_types: Vec<TokenStream> = filters.iter().map(|(filter, span)| {
        match filter {
            Filter::With(ty) => {
                let ty_ident = Ident::new(ty, *span);
                quote! { bevy::ecs::query::With<#ty_ident> }
            }
            Filter::Without(ty) => {
                let ty_ident = Ident::new(ty, *span);
                quote! { bevy::ecs::query::Without<#ty_ident> }
            }
            Filter::Added(ty) => {
                let ty_ident = Ident::new(ty, *span);
                quote! { bevy::ecs::query::Added<#ty_ident> }
            }
            Filter::Changed(ty) => {
                let ty_ident = Ident::new(ty, *span);
                quote! { bevy::ecs::query::Changed<#ty_ident> }
            }
            Filter::Custom(tokens) => {
                // Parse the custom filter tokens
                TokenStream::from(quote_spanned! {*span=> #tokens })
            }
        }
    }).collect();

    match filter_types.len() {
        0 => quote! { () },
        1 => filter_types[0].clone(),
        _ => quote! { (#(#filter_types),*) },
    }
}

// Generate view structs
fn generate_view_structs(
    ordered: &OrderedInfo,
    amortized: &AmortizedData,
) -> TokenStream {
    let mut structs = Vec::new();

    for (canonical_name, query_data) in &amortized.queries {
        let struct_name = Ident::new(
            &to_pascal_case(canonical_name),
            Span::call_site()
        );
        let view_struct_name = Ident::new(
            &format!("{}View", struct_name),
            Span::call_site()
        );

        let mut fields = vec![quote! { id: Entity }];

        // Get components in order
        if let Some(ordered) = ordered.query_components.get(canonical_name) {
            for comp_name in ordered {
                if let Some(comp_access) = query_data.components.get(comp_name) {
                    let field_name = Ident::new(
                        &to_snake_case(comp_name),
                        Span::call_site()
                    );
                    let type_ident = Ident::new(comp_name, comp_access.span);
                    let field_type = generate_view_field_type(&type_ident, comp_access);
                    fields.push(quote! { #field_name: #field_type });
                }
            }
        }

        structs.push(quote! {
            struct #view_struct_name<'a> {
                #(#fields),*
            }
        });
    }

    quote! { #(#structs)* }
}

fn generate_view_field_type(type_ident: &Ident, access: &ComponentAccess) -> TokenStream {
    match (access.is_optional, access.needs_write) {
        (true, true) => quote! { Option<bevy::ecs::change_detection::Mut<'a, #type_ident>> },
        (true, false) => quote! { Option<&'a #type_ident> },
        (false, true) => quote! { bevy::ecs::change_detection::Mut<'a, #type_ident> },
        (false, false) => quote! { &'a #type_ident },
    }
}

// Process the body tokens
pub fn generate_body(
    body_tokens: TokenStream,
    chains: &ParsedChains,
    amortized: &AmortizedData,
) -> TokenStream {
    let ordered = build_ordered_info(chains, &amortized.canonical_name_cache);
    let view_structs = generate_view_structs(&ordered, amortized);

    // Process the body
    let mut state = ProcessingState {
        magic_loops: Vec::new(),
        brace_depth: 0,
    };

    let processed_body = process_tokens(
        body_tokens,
        chains,
        amortized,
        &ordered,
        &mut state,
    );

    // Combine view structs and processed body
    quote! {
        #view_structs
        #processed_body
    }
}

#[derive(Debug)]
struct ProcessingState {
    magic_loops: Vec<MagicLoopInfo>,
    brace_depth: usize,
}

#[derive(Debug)]
struct MagicLoopInfo {
    loop_var_name: String,
    start_depth: usize,
}

fn process_tokens(
    tokens: TokenStream,
    chains: &ParsedChains,
    amortized: &AmortizedData,
    ordered: &OrderedInfo,
    state: &mut ProcessingState,
) -> TokenStream {
    let token_vec: Vec<TokenTree> = tokens.into_iter().collect();
    let mut output = TokenStream::new();
    let mut idx = 0;

    while idx < token_vec.len() {
        match &token_vec[idx] {
            TokenTree::Ident(id) if id.to_string() == "for" => {
                // Check for magic entity syntax
                if let Some((magic_loop, end_idx)) = detect_magic_entity(
                    &token_vec,
                    idx,
                    chains,
                    &amortized.canonical_name_cache,
                ) {
                    let generated = generate_magic_entity_loop(
                        magic_loop,
                        chains,
                        amortized,
                        ordered,
                        state,
                    );
                    output.extend(generated);
                    idx = end_idx;
                    continue;
                }

                // Regular for loop
                output.append(token_vec[idx].clone());
                idx += 1;
            }
            TokenTree::Ident(id) if is_placeholder(id) => {
                // Replace placeholder
                let replacement = generate_replacement(
                    id,
                    chains,
                    amortized,
                    ordered,
                    state,
                );
                output.extend(replacement);
                idx += 1;
            }
            TokenTree::Group(group) => {
                // Track brace depth
                if matches!(group.delimiter(), Delimiter::Brace) {
                    state.brace_depth += 1;
                }

                // Process group contents
                let inner = process_tokens(
                    group.stream(),
                    chains,
                    amortized,
                    ordered,
                    state,
                );
                let new_group = Group::new(group.delimiter(), inner);
                output.append(TokenTree::Group(new_group));

                // Update brace depth and clean up magic loops
                if matches!(group.delimiter(), Delimiter::Brace) {
                    state.brace_depth -= 1;
                    let brace_depth = state.brace_depth;
                    state.magic_loops.retain(|loop_info| {
                        loop_info.start_depth <= brace_depth
                    });
                }

                idx += 1;
            }
            _ => {
                output.append(token_vec[idx].clone());
                idx += 1;
            }
        }
    }

    output
}

fn is_placeholder(id: &Ident) -> bool {
    id.to_string().starts_with("__bevy_chain_")
}

fn extract_chain_id(id: &Ident) -> Option<usize> {
    id.to_string()
        .strip_prefix("__bevy_chain_")
        .and_then(|s| s.parse().ok())
}

fn detect_magic_entity(
    tokens: &[TokenTree],
    start_idx: usize,
    chains: &ParsedChains,
    canonical_cache: &HashMap<usize, String>,
) -> Option<(MagicEntityPattern, usize)> {
    // Expected pattern: for __bevy_chain_X in __bevy_chain_Y { body }
    if start_idx + 4 >= tokens.len() {
        return None;
    }

    // Check for placeholder after 'for'
    let loop_var_id = match &tokens[start_idx + 1] {
        TokenTree::Ident(id) if is_placeholder(id) => id,
        _ => return None,
    };

    // Check for 'in'
    match &tokens[start_idx + 2] {
        TokenTree::Ident(id) if id.to_string() == "in" => {},
        _ => return None,
    }

    // Check for iterator placeholder
    let iterator_id = match &tokens[start_idx + 3] {
        TokenTree::Ident(id) if is_placeholder(id) => id,
        _ => return None,
    };

    // Get chain IDs
    let loop_var_chain_id = extract_chain_id(loop_var_id)?;
    let iterator_chain_id = extract_chain_id(iterator_id)?;

    // Get chains
    let loop_var_chain = chains.chains.get(loop_var_chain_id)?;
    let iterator_chain = chains.chains.get(iterator_chain_id)?;

    // Check if loop var is child of iterator
    if let Some((parent_id, _)) = &loop_var_chain.parent {
        if *parent_id != iterator_chain_id {
            return None;
        }
    } else {
        return None;
    }

    // Get loop body
    let body = match &tokens[start_idx + 4] {
        TokenTree::Group(g) if matches!(g.delimiter(), Delimiter::Brace) => g.stream(),
        _ => return None,
    };

    // Get canonical name and loop var name
    let canonical_name = canonical_cache.get(&iterator_chain_id)?.clone();
    let loop_var_name = match &loop_var_chain.sys_op {
        SysOp::Query { name } => name.to_string(),
        _ => return None,
    };

    // Check if this is a via relationship
    let is_via = iterator_chain.operations.iter()
        .any(|op| matches!(op.method_kind, MethodKind::Via));

    Some((
        MagicEntityPattern {
            loop_var_chain_id,
            iterator_chain_id,
            canonical_name,
            loop_var_name,
            body,
            is_via,
        },
        start_idx + 5, // Skip past the entire for loop
    ))
}

#[derive(Debug)]
struct MagicEntityPattern {
    loop_var_chain_id: usize,
    iterator_chain_id: usize,
    canonical_name: String,
    loop_var_name: String,
    body: TokenStream,
    is_via: bool,
}

fn generate_magic_entity_loop(
    pattern: MagicEntityPattern,
    chains: &ParsedChains,
    amortized: &AmortizedData,
    ordered: &OrderedInfo,
    state: &mut ProcessingState,
) -> TokenStream {
    let query_data = match amortized.queries.get(&pattern.canonical_name) {
        Some(data) => data,
        None => return quote! {},
    };

    // Add to magic loops
    state.magic_loops.push(MagicLoopInfo {
        loop_var_name: pattern.loop_var_name.clone(),
        start_depth: state.brace_depth,
    });

    if pattern.is_via {
        generate_via_loop(pattern, chains, amortized, ordered, state)
    } else {
        generate_regular_magic_loop(pattern, query_data, chains, amortized, ordered, state)
    }
}

fn generate_regular_magic_loop(
    pattern: MagicEntityPattern,
    query_data: &QueryData,
    chains: &ParsedChains,
    amortized: &AmortizedData,
    ordered: &OrderedInfo,
    state: &mut ProcessingState,
) -> TokenStream {
    let query_name = Ident::new(
        &format!("{}_query", pattern.canonical_name),
        Span::call_site()
    );

    // Determine if we need iter_mut
    let needs_mut = query_data.components.values().any(|c| c.needs_write);
    let iter_method = if needs_mut { quote! { iter_mut } } else { quote! { iter } };

    // Build tuple destructuring pattern
    let mut tuple_parts = vec![quote! { entity }];
    let mut field_names = Vec::new();

    if let Some(comp_names) = ordered.query_components.get(&pattern.canonical_name) {
        for comp_name in comp_names {
            let field_name = Ident::new(&to_snake_case(comp_name), Span::call_site());
            tuple_parts.push(quote! { #field_name });
            field_names.push(field_name);
        }
    }

    // Build view struct instantiation
    let view_struct_name = Ident::new(
        &format!("{}View", to_pascal_case(&pattern.canonical_name)),
        Span::call_site()
    );

    let mut struct_fields = vec![quote! { id: entity }];
    for field_name in &field_names {
        struct_fields.push(quote! { #field_name });
    }

    // Process body
    let processed_body = process_tokens(
        pattern.body,
        chains,
        amortized,
        ordered,
        state,
    );

    // Generate loop
    let loop_var = Ident::new(&pattern.loop_var_name, Span::call_site());
    let let_binding = if needs_mut {
        quote! { let mut #loop_var }
    } else {
        quote! { let #loop_var }
    };

    quote! {
        for (#(#tuple_parts),*) in #query_name.#iter_method() {
            #let_binding = #view_struct_name {
                #(#struct_fields),*
            };
            #processed_body
        }
    }
}

fn generate_via_loop(
    pattern: MagicEntityPattern,
    chains: &ParsedChains,
    amortized: &AmortizedData,
    ordered: &OrderedInfo,
    state: &mut ProcessingState,
) -> TokenStream {
    // Get the via component from the iterator chain
    let iterator_chain = &chains.chains[pattern.iterator_chain_id];
    let via_component = iterator_chain.operations.iter()
        .find(|op| matches!(op.method_kind, MethodKind::Via))
        .and_then(|op| op.type_param.as_ref())
        .map(|t| t.to_string())
        .unwrap_or_default();

    // Get the target query (from the loop var)
    let target_canonical = amortized.canonical_name_cache.get(&pattern.loop_var_chain_id)
        .cloned()
        .unwrap_or(pattern.canonical_name.clone());

    let target_query_data = match amortized.queries.get(&target_canonical) {
        Some(data) => data,
        None => return quote! {},
    };

    let source_entity_name = match &iterator_chain.sys_op {
        SysOp::Query { name } => name.to_string(),
        _ => return quote! {},
    };
    
    // Generate names
    let source_entity = Ident::new(
        &source_entity_name,
        Span::call_site()
    );
    let via_field = Ident::new(&to_snake_case(&via_component), Span::call_site());
    let target_query = Ident::new(
        &format!("{}_query", target_canonical),
        Span::call_site()
    );

    // Build tuple destructuring for target
    let mut tuple_parts = vec![quote! { entity }];
    let mut field_names = Vec::new();

    if let Some(comp_names) = ordered.query_components.get(&target_canonical) {
        for comp_name in comp_names {
            let field_name = Ident::new(&to_snake_case(comp_name), Span::call_site());
            tuple_parts.push(quote! { #field_name });
            field_names.push(field_name);
        }
    }

    // Build view struct for target
    let view_struct_name = Ident::new(
        &format!("{}View", to_pascal_case(&target_canonical)),
        Span::call_site()
    );

    let mut struct_fields = vec![quote! { id: entity }];
    for field_name in &field_names {
        struct_fields.push(quote! { #field_name });
    }

    // Process body
    let processed_body = process_tokens(
        pattern.body,
        chains,
        amortized,
        ordered,
        state,
    );

    // Generate loop
    let loop_var = Ident::new(&pattern.loop_var_name, Span::call_site());
    let needs_mut = target_query_data.components.values().any(|c| c.needs_write);
    let get_method = if needs_mut { quote! { get_mut } } else { quote! { get } };

    quote! {
        for member_id in #source_entity.#via_field.iter() {
            let Ok((#(#tuple_parts),*)) = #target_query.#get_method(member_id) else { continue; };
            let mut #loop_var = #view_struct_name {
                #(#struct_fields),*
            };
            #processed_body
        }
    }
}

fn generate_replacement(
    placeholder: &Ident,
    chains: &ParsedChains,
    amortized: &AmortizedData,
    ordered: &OrderedInfo,
    state: &ProcessingState,
) -> TokenStream {
    let chain_id = match extract_chain_id(placeholder) {
        Some(id) => id,
        None => return quote! { #placeholder },
    };

    let chain = match chains.chains.get(chain_id) {
        Some(c) => c,
        None => return quote! { #placeholder },
    };
    
    // Check if this is a reference to a loop variable
    let chain_name = match &chain.sys_op {
        SysOp::Query { name } => name.to_string(),
        _ => {
            // For resources and commands, never treat as loop variable
            return match &chain.sys_op {
                SysOp::Resource { type_param, .. } => {
                    let resource_name = Ident::new(
                        &to_snake_case(&type_param.to_string()),
                        chain.start_span
                    );
                    quote! { #resource_name }
                }
                SysOp::Commands(_) => {
                    quote! { commands }
                }
                _ => quote! { #placeholder },
            };
        }
    };

    // Check if this chain name matches any active loop variable name
    // AND the chain doesn't have brackets (which would make it an iteration/lookup)
    if matches!(chain.bracket_content, BracketContent::None) {
        for loop_info in &state.magic_loops {
            if chain_name == loop_info.loop_var_name {
                return generate_loop_var_replacement(
                    chain,
                    &loop_info.loop_var_name,
                );
            }
        }
    }
    
    // Generate based on chain type
    match &chain.sys_op {
        SysOp::Query { .. } => {
            generate_query_replacement(chain, chain_id, amortized, ordered)
        }
        SysOp::Resource { type_param, .. } => {
            let resource_name = Ident::new(
                &to_snake_case(&type_param.to_string()),
                Span::call_site()
            );
            quote! { #resource_name }
        }
        SysOp::Commands(ident) => {
            let span = ident.span();
            quote_spanned! {span=> commands }
        }
    }
}

fn generate_loop_var_replacement(
    chain: &Chain,
    loop_var_name: &str,
) -> TokenStream {
    let var_name = Ident::new(loop_var_name, Span::call_site());

    // Check if this chain uses entity commands
    let has_command = chain.operations.iter()
        .any(|op| matches!(op.method_kind, MethodKind::Command));

    if has_command {
        // Generate commands.entity(var.id)
        return quote! { commands.entity(#var_name.id) };
    }
    
    // Check if this chain accesses a component
    let component_access = chain.operations.iter().rev()
        .find(|op| matches!(op.method_kind, MethodKind::Get | MethodKind::GetMut | 
                           MethodKind::TryGet | MethodKind::TryGetMut))
        .and_then(|op| op.type_param.as_ref());

    if let Some(component) = component_access {
        let field_name = Ident::new(&to_snake_case(&component.to_string()), Span::call_site());
        quote! { #var_name.#field_name }
    } else {
        quote! { #var_name }
    }
}

fn generate_query_replacement(
    chain: &Chain,
    chain_id: usize,
    amortized: &AmortizedData,
    ordered: &OrderedInfo,
) -> TokenStream {
    let canonical_name = match amortized.canonical_name_cache.get(&chain_id) {
        Some(name) => name,
        None => return quote! {},
    };

    let query_name = Ident::new(
        &format!("{}_query", canonical_name),
        chain.start_span
    );

    let query_data = match amortized.queries.get(canonical_name) {
        Some(data) => data,
        None => return quote! {},
    };

    match &chain.bracket_content {
        BracketContent::Empty => {
            // Iteration
            generate_query_iteration(&query_name, query_data, canonical_name, ordered)
        }
        BracketContent::Expression(expr) => {
            // Lookup
            generate_query_lookup(&query_name, query_data, canonical_name, ordered, expr)
        }
        BracketContent::None => {
            // Just the query reference
            quote! { #query_name }
        }
    }
}

fn generate_query_iteration(
    query_name: &Ident,
    query_data: &QueryData,
    canonical_name: &str,
    ordered: &OrderedInfo,
) -> TokenStream {
    let needs_mut = query_data.components.values().any(|c| c.needs_write);
    let iter_method = if needs_mut { quote! { iter_mut } } else { quote! { iter } };

    let map_closure = generate_view_struct_map(canonical_name, ordered);

    quote! {
        #query_name.#iter_method()#map_closure
    }
}

fn generate_query_lookup(
    query_name: &Ident,
    query_data: &QueryData,
    canonical_name: &str,
    ordered: &OrderedInfo,
    expr: &TokenStream,
) -> TokenStream {
    let needs_mut = query_data.components.values().any(|c| c.needs_write);
    let get_method = if needs_mut { quote! { get_mut } } else { quote! { get } };

    let map_closure = generate_view_struct_map(canonical_name, ordered);

    quote! {
        #query_name.#get_method(#expr)#map_closure
    }
}

// Shared function to generate the map closure for view struct creation
fn generate_view_struct_map(
    canonical_name: &str,
    ordered: &OrderedInfo,
) -> TokenStream {
    // Build tuple destructuring
    let mut tuple_parts = vec![quote! { entity }];
    let mut field_names = Vec::new();

    if let Some(comp_names) = ordered.query_components.get(canonical_name) {
        for comp_name in comp_names {
            let field_name = Ident::new(&to_snake_case(comp_name), Span::call_site());
            tuple_parts.push(quote! { #field_name });
            field_names.push(field_name);
        }
    }

    // Build view struct instantiation
    let view_struct_name = Ident::new(
        &format!("{}View", to_pascal_case(canonical_name)),
        Span::call_site()
    );

    let mut struct_fields = vec![quote! { id: entity }];
    for field_name in &field_names {
        struct_fields.push(quote! { #field_name });
    }

    quote! {
        .map(|(#(#tuple_parts),*)| {
            #view_struct_name {
                #(#struct_fields),*
            }
        })
    }
}