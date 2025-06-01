use std::collections::HashMap;
use proc_macro2::{Span, TokenStream};
use quote::quote;
use crate::analysis::{AmortizedData, ComponentAccess, Filter, QueryData};
use crate::codegen::{OrderedInfo, ParameterInfo};

pub fn to_snake_case(s: &str) -> String {
    let mut result = String::new();
    let mut chars = s.chars().peekable();

    while let Some(ch) = chars.next() {
        if ch.is_uppercase() {
            if !result.is_empty()
                && (result
                .chars()
                .last()
                .map_or(false, |last| last.is_lowercase())
                || chars.peek().map_or(false, |next| next.is_lowercase()))
            {
                result.push('_');
            }
            result.push(ch.to_lowercase().next().unwrap());
        } else {
            result.push(ch);
        }
    }

    result
}

pub fn to_pascal_case(s: &str) -> String {
    s.split('_')
        .filter(|word| !word.is_empty())
        .map(|word| {
            let mut chars = word.chars();
            match chars.next() {
                None => String::new(),
                Some(first) => first.to_uppercase().chain(chars.as_str().to_lowercase().chars()).collect(),
            }
        })
        .collect()
}

/// Generate documentation comments describing the system parameters
pub fn generate_param_docs(
    amortized: &AmortizedData,
    ordered: &OrderedInfo,
) -> Vec<TokenStream> {
    let mut doc_attrs = Vec::new();

    // Use triple-slash comments for better IDE support
    doc_attrs.push(quote! { 
        #[doc = "# Generated System Parameters\n\nThis system uses the following automatically generated parameters:\n"] 
    });

    // Document queries in order
    for param_info in &ordered.parameter_order {
        match param_info {
            ParameterInfo::Query(canonical_name) => {
                if let Some(query_data) = amortized.queries.get(canonical_name) {
                    // Create a properly formatted query description
                    let mut query_doc = format!(
                        "## `{}_query: Query<{}, {}>`\n\n",
                        canonical_name,
                        format_component_tuple(canonical_name, query_data, ordered),
                        format_filter_tuple(&query_data.filters)
                    );

                    query_doc.push_str("**Components:**\n");

                    // List components in order with proper formatting
                    if let Some(components) = ordered.query_components.get(canonical_name) {
                        for comp_name in components {
                            if let Some(access) = query_data.components.get(comp_name) {
                                let access_desc = format_component_access(comp_name, access);
                                query_doc.push_str(&format!("- `{}`\n", access_desc));
                            }
                        }
                    } else {
                        query_doc.push_str("- `Entity` (ID only)\n");
                    }

                    // List filters if any with proper formatting
                    if !query_data.filters.is_empty() {
                        query_doc.push_str("\n**Filters:**\n");
                        for filter in query_data.filters.keys() {
                            let filter_desc = format_filter_description(filter);
                            query_doc.push_str(&format!("- `{}`\n", filter_desc));
                        }
                    }

                    query_doc.push('\n');
                    doc_attrs.push(quote! { #[doc = #query_doc] });
                }
            }
            ParameterInfo::Resource { type_name, mutable } => {
                let access_type = if *mutable { "ResMut" } else { "Res" };
                let param_name = to_snake_case(&type_name.to_string());
                let type_str = format_type_name(type_name);

                let resource_doc = format!(
                    "## `{}: {}<{}>`\n\nResource access for `{}`\n\n",
                    param_name, access_type, type_str, type_str
                );

                doc_attrs.push(quote! { #[doc = #resource_doc] });
            }
        }
    }

    if amortized.needs_commands {
        let commands_doc = "## `commands: Commands`\n\nEntity manipulation commands for spawning, despawning, and modifying entities\n\n";
        doc_attrs.push(quote! { #[doc = #commands_doc] });
    }

    doc_attrs
}

/// Format a component access pattern for documentation
fn format_component_access(comp_name: &str, access: &ComponentAccess) -> String {
    match (access.is_optional, access.needs_write) {
        (true, true) => format!("Option<&mut {}>", comp_name),
        (true, false) => format!("Option<&{}>", comp_name),
        (false, true) => format!("&mut {}", comp_name),
        (false, false) => format!("&{}", comp_name),
    }
}

/// Format the component tuple for the query type signature
fn format_component_tuple(
    canonical_name: &str,
    query_data: &QueryData,
    ordered: &OrderedInfo
) -> String {
    let mut components = vec!["Entity".to_string()];

    if let Some(comp_names) = ordered.query_components.get(canonical_name) {
        for comp_name in comp_names {
            if let Some(access) = query_data.components.get(comp_name) {
                components.push(format_component_access(comp_name, access));
            }
        }
    }

    if components.len() == 1 {
        components[0].clone()
    } else {
        format!("({})", components.join(", "))
    }
}

/// Format the filter tuple for the query type signature  
fn format_filter_tuple(filters: &HashMap<Filter, Span>) -> String {
    if filters.is_empty() {
        return "()".to_string();
    }

    let filter_strs: Vec<String> = filters.keys()
        .map(|f| format_filter_description(f))
        .collect();

    if filter_strs.len() == 1 {
        filter_strs[0].clone()
    } else {
        format!("({})", filter_strs.join(", "))
    }
}

/// Format a filter for documentation with full type information
fn format_filter_description(filter: &Filter) -> String {
    match filter {
        Filter::With(ty) => {
            if ty.is_empty() {
                "With<Unknown>".to_string() // Fallback for missing type info
            } else {
                format!("With<{}>", ty)
            }
        }
        Filter::Without(ty) => {
            if ty.is_empty() {
                "Without<Unknown>".to_string()
            } else {
                format!("Without<{}>", ty)
            }
        }
        Filter::Added(ty) => {
            if ty.is_empty() {
                "Added<Unknown>".to_string()
            } else {
                format!("Added<{}>", ty)
            }
        }
        Filter::Changed(ty) => {
            if ty.is_empty() {
                "Changed<Unknown>".to_string()
            } else {
                format!("Changed<{}>", ty)
            }
        }
        Filter::Custom(tokens) => {
            if tokens.is_empty() {
                "UnknownFilter".to_string()
            } else {
                // Clean up the token stream for better display
                clean_token_string(tokens)
            }
        }
    }
}

/// Clean up token stream strings for better documentation display
fn clean_token_string(tokens: &str) -> String {
    tokens
        .replace(" :: ", "::")
        .replace(" < ", "<")
        .replace(" > ", ">")
        .replace(" , ", ", ")
        .trim()
        .to_string()
}

/// Format type names, handling complex generic types better
fn format_type_name(type_stream: &TokenStream) -> String {
    let type_str = type_stream.to_string();

    // Handle common formatting issues
    clean_token_string(&type_str)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_snake_case() {
        assert_eq!(to_snake_case("Health"), "health");
        assert_eq!(to_snake_case("PlayerStats"), "player_stats");
        assert_eq!(to_snake_case("MyCustomComponent"), "my_custom_component");
        assert_eq!(to_snake_case("XYZCoordinate"), "x_y_z_coordinate");
    }

    #[test]
    fn test_to_pascal_case() {
        assert_eq!(to_pascal_case("health"), "Health");
        assert_eq!(to_pascal_case("player_stats"), "PlayerStats");
        assert_eq!(to_pascal_case("my_custom_component"), "MyCustomComponent");
        assert_eq!(to_pascal_case("x_y_z_coordinate"), "XYZCoordinate");
    }
}