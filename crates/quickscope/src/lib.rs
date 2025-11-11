//! This crate contains two data structures for representing variable scopes:
//! [`ScopeMap`] and [`ScopeSet`]. They are analogous to `HashMap` and `HashSet`,
//! but the main difference is that they store their data in multiple "layers",
//! or scopes. 
//!
//! When created, the [`ScopeMap`] and [`ScopeSet`] start with one base layer, which cannot be removed.
//! Additional layers can be added with `push_layer()`, and removed with `pop_layer()`.
//! Removing a layer removes the entries defined on it.
//! 
//! `ScopeMap` is ideal for representing variable scopes and associated values,
//! while `ScopeSet` is more suitable for simply representing the presence of variables and their scopes.
//! 
//! ### Example
//!
//! ```rust
//! use quickscope::*;
//!
//! let mut vars = ScopeMap::new();
//! 
//! // Define two variables in main scope
//! vars.define("a", 1);
//! vars.define("b", 2);
//! 
//! // Add a child scope
//! vars.push_layer();
//! 
//! // Override value of `a`
//! vars.define("a", 3);
//! 
//! assert_eq!(Some(&3), vars.get("a"));
//! assert_eq!(Some(&2), vars.get("b"));
//! 
//! // Remove child scope
//! vars.pop_layer();
//! 
//! // Value of `a` is no longer overridden
//! assert_eq!(Some(&1), vars.get("a"));
//! ```
//! 
//! [`ScopeMap`]: map/struct.ScopeMap.html
//! [`ScopeSet`]: set/struct.ScopeSet.html

mod map;
mod set;

pub use map::*;
pub use set::*;

