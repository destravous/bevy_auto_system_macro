use proc_macro::TokenStream;
use proc_macro2::{Delimiter, TokenStream as TokenStream2, TokenTree};
use quote::quote;

mod parser;
mod analysis;
mod codegen;
mod util;

use parser::ParsedChains;

use crate::analysis::amortize_chains;
use crate::codegen::{build_ordered_info, generate_final_tokens};
use crate::util::generate_param_docs;

/// The `system!` macro transforms entity-focused code into efficient Bevy ECS systems.
///
/// # Core Operations
///
/// **Entity Queries:**
/// - `#entities[]` - Iterate over all entities
/// - `#entities[id]` - Look up specific entity by ID
/// - `#entities` - Get query reference (no iteration)
///
/// **Component Access:**
/// - `.get<T>` - Immutable component access
/// - `.get_mut<T>` - Mutable component access
/// - `.try_get<T>` - Optional immutable access (returns Option)
/// - `.try_get_mut<T>` - Optional mutable access (returns Option)
///
/// **Query Filters:**
/// - `.is<T>` - Entity must have component T (With<T>)
/// - `.is_not<T>` - Entity must not have component T (Without<T>)
/// - `.changed<T>` - Component T was changed (Changed<T>)
/// - `.added<T>` - Component T was added (Added<T>)
///
/// **Resources:**
/// - `#get_resource<T>` - Access resource immutably
/// - `#get_resource_mut<T>` - Access resource mutably
///
/// **Commands:**
/// - `#commands.spawn(...)` - Spawn new entity
/// - `#entity.command().despawn()` - Despawn entity
/// - `#entity.command().insert(Component)` - Insert component
///
/// **Relationships:**
/// - `.via<RelationComponent>[]` - Iterate over related entities
///
/// # Usage Patterns
///
/// **Simple iteration:**
/// ```rust
/// system!(fn damage_enemies() {
///     for enemy in #enemies.is<Enemy>.get_mut<Health>[] {
///         enemy.health.0 -= 5;
///     }
/// });
/// ```
///
/// **Magic entity syntax:**
/// ```rust
/// system!(fn heal_players() {
///     for #player in #players.is<Player>[] {
///         if #player.get<Health>.0 < 50 {
///             #player.get_mut<Health>.0 += 10;
///         }
///     }
/// });
/// ```
///
/// **Entity lookups:**
/// ```rust
/// system!(fn damage_target() {
///     let target_id = #get_resource<TargetEntity>.0;
///     if let Ok(mut target) = #enemies.get_mut<Health>[target_id] {
///         target.health.0 -= 25;
///     }
/// });
/// ```
///
/// **Optional components:**
/// ```rust
/// system!(fn apply_buffs() {
///     for #unit in #units.is<Alive>[] {
///         if let Some(buff) = #unit.try_get<SpeedBuff> {
///             #unit.get_mut<Velocity>.0 *= buff.multiplier;
///         }
///     }
/// });
/// ```
///
/// **Relationships:**
/// ```rust
/// system!(fn squad_commands() {
///     for #leader in #leaders.is<SquadLeader>[] {
///         for #member in #leader.via<SquadMembers>[] {
///             #member.get_mut<Morale>.0 += 5;
///         }
///     }
/// });
/// ```
///
/// **Resources and spawning:**
/// ```rust
/// system!(fn spawn_enemies() {
///     let time = #get_resource<Time>;
///     if time.elapsed_seconds() > 5.0 {
///         #commands.spawn((Enemy, Health(100)));
///     }
/// });
/// ```
#[proc_macro]
pub fn system(input: TokenStream) -> TokenStream {
    let input2 = TokenStream2::from(input);

    // Parse the function
    let parsed_fn = match parse_fn(input2) {
        Ok(f) => f,
        Err(e) => return TokenStream::from(e),
    };

    // Pass 1: Parse chains and replace with placeholders
    let mut chains = ParsedChains::new();
    let transformed_body = parser::transform_custom_syntax(parsed_fn.body, &mut chains);

    // Pass 2: Analyze queries
    let analyzer = amortize_chains(&chains);

    // Pass 3: Generate code
    let (params, body) = generate_final_tokens(transformed_body, &chains, &analyzer);

    let all_params = combine_params(parsed_fn.existing_params, params);

    let output = match parsed_fn.kind {
        CallableKind::Function => {
            // Generate documentation for functions only
            let ordered = build_ordered_info(&chains, &analyzer.canonical_name_cache);
            let doc_attrs = generate_param_docs(&analyzer, &ordered);
            let prefix = parsed_fn.prefix_tokens;
            quote! {
                #(#doc_attrs)*
                #prefix(#all_params) {
                    #body
                }
            }
        }
        CallableKind::Closure => {
            quote! {
                |#all_params| {
                    #body
                }
            }
        }
    };

    TokenStream::from(output)
}

#[derive(Debug, Clone)]
struct FnParser {
    pub kind: CallableKind,
    pub existing_params: TokenStream2,
    pub body: TokenStream2,
    pub prefix_tokens: TokenStream2, // Everything before params (e.g., "pub fn name")
}

#[derive(Debug, Clone)]
enum CallableKind {
    Function,
    Closure,
}

#[derive(Debug, Clone, PartialEq)]
enum ParseState {
    Start,
    AfterFn,
    AfterFunctionParams,
    InClosureParams,
    AfterClosureParams,
    Done,
}

fn parse_fn(input: TokenStream2) -> Result<FnParser, TokenStream2> {
    let tokens: Vec<TokenTree> = input.into_iter().collect();
    let mut state = ParseState::Start;
    let mut pos = 0;
    let mut prefix_tokens = TokenStream2::new();
    let mut existing_params = TokenStream2::new();
    let mut body = TokenStream2::new();
    let mut kind = None;

    while pos < tokens.len() && state != ParseState::Done {
        let token = &tokens[pos];

        match (&state, token) {
            // Start state - look for `fn` or `|`
            (ParseState::Start, TokenTree::Ident(id)) if id.to_string() == "fn" => {
                prefix_tokens.extend(std::iter::once(token.clone()));
                state = ParseState::AfterFn;
                kind = Some(CallableKind::Function);
            }
            (ParseState::Start, TokenTree::Punct(p)) if p.as_char() == '|' => {
                state = ParseState::InClosureParams;
                kind = Some(CallableKind::Closure);
            }
            (ParseState::Start, _) => {
                prefix_tokens.extend(std::iter::once(token.clone()));
            }

            // After `fn` - collect function name and look for params
            (ParseState::AfterFn, TokenTree::Group(g)) if g.delimiter() == Delimiter::Parenthesis => {
                existing_params = g.stream();
                state = ParseState::AfterFunctionParams;
            }
            (ParseState::AfterFn, _) => {
                prefix_tokens.extend(std::iter::once(token.clone()));
            }

            // After function params - look for body
            (ParseState::AfterFunctionParams, TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => {
                body = g.stream();
                state = ParseState::Done;
            }
            (ParseState::AfterFunctionParams, _) => {
                // Skip tokens between params and body (like return type, where clauses)
            }

            // In closure params - collect until closing `|`
            (ParseState::InClosureParams, TokenTree::Punct(p)) if p.as_char() == '|' => {
                state = ParseState::AfterClosureParams;
            }
            (ParseState::InClosureParams, _) => {
                existing_params.extend(std::iter::once(token.clone()));
            }

            // After closure params - look for body
            (ParseState::AfterClosureParams, TokenTree::Group(g)) if g.delimiter() == Delimiter::Brace => {
                body = g.stream();
                state = ParseState::Done;
            }
            (ParseState::AfterClosureParams, _) => {
                // Could be `->` return type or other tokens before body
            }

            _ => {
                // Unexpected token for current state
            }
        }

        pos += 1;
    }

    let kind = kind.ok_or_else(|| quote! {
        compile_error!("Expected function or closure");
    })?;

    if body.is_empty() {
        return Err(quote! {
            compile_error!("Could not find function/closure body");
        });
    }

    Ok(FnParser {
        kind,
        existing_params,
        body,
        prefix_tokens,
    })
}

// Helper function to combine existing params with new ones
fn combine_params(existing: TokenStream2, new: TokenStream2) -> TokenStream2 {
    if existing.is_empty() {
        new
    } else if new.is_empty() {
        existing
    } else {
        quote! { #existing, #new }
    }
}
