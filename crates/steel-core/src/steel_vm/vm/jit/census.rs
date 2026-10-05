//! `STEEL_DEOPT_CENSUS`: counts calls that leave compiled code.

use super::*;

#[inline(always)]
/// `STEEL_DEOPT_CENSUS=1` tallies what the global tail-call deopt handler
/// dispatches on, so time spent in it can be traced back to a callee kind.
pub fn deopt_census_enabled() -> bool {
    crate::config::config().deopt_census
}

pub static DEOPT_CENSUS: std::sync::Mutex<Option<std::collections::HashMap<String, u64>>> =
    std::sync::Mutex::new(None);

pub(super) fn record_deopt(func: &SteelVal, index: usize, ctx: &VmCore) {
    let kind = match func {
        SteelVal::Closure(c) => {
            if c.super_instructions().is_some() {
                "Closure (compiled - caller was built before it was)"
            } else {
                "Closure (never compiled)"
            }
        }
        SteelVal::FuncV(_) => "FuncV (plain primitive)",
        SteelVal::BuiltIn(_) => "BuiltIn",
        SteelVal::MutFunc(_) => "MutFunc",
        SteelVal::BoxedFunction(_) => "BoxedFunction",
        SteelVal::CustomStruct(_) => "CustomStruct",
        SteelVal::MutableStruct(_) => "MutableStruct",
        SteelVal::ContinuationFunction(_) => "ContinuationFunction",
        _ => "other",
    };
    // The function making the call, which is the frame on top.
    let caller = ctx
        .thread
        .stack_frames
        .last()
        .map(|f| f.function.id)
        .unwrap_or(0);
    let mut guard = DEOPT_CENSUS.lock().unwrap();
    let map = guard.get_or_insert_with(Default::default);
    let key = match map.get(&format!("@name{index}")) {
        Some(_) => format!("{kind}|{index}|caller {caller}"),
        None => {
            // Resolve the global's source name once, so the census shows the
            // primitive's name instead of its index.
            let name = ctx
                .thread
                .compiler
                .read()
                .symbol_map
                .values()
                .get(index)
                .map(|x| x.resolve().to_string())
                .unwrap_or_else(|| "(applied by a builtin)".into());
            map.insert(format!("@name{index}"), 0);
            map.insert(format!("#name{index}={name}"), 0);
            format!("{kind}|{index}|caller {caller}")
        }
    };
    *map.entry(key).or_insert(0) += 1;
}

pub fn dump_deopt_census() {
    let guard = DEOPT_CENSUS.lock().unwrap();
    let Some(map) = guard.as_ref() else { return };
    let mut by_kind: std::collections::HashMap<&str, u64> = Default::default();
    let mut total = 0u64;
    let names: std::collections::HashMap<&str, &str> = map
        .keys()
        .filter_map(|k| k.strip_prefix("#name"))
        .filter_map(|k| k.split_once('='))
        .collect();
    for (k, v) in map.iter() {
        if k.starts_with('@') || k.starts_with('#') {
            continue;
        }
        let kind = k.split('|').next().unwrap();
        *by_kind.entry(kind).or_insert(0) += v;
        total += v;
    }
    let mut kinds: Vec<_> = by_kind.into_iter().collect();
    kinds.sort_by_key(|x| std::cmp::Reverse(x.1));
    eprintln!("deopt-census: {total} tail-call deopts");
    for (k, v) in kinds {
        eprintln!(
            "  {:6.2}%  {:12}  {}",
            100.0 * v as f64 / total as f64,
            v,
            k
        );
    }
    let mut sites: Vec<_> = map
        .iter()
        .filter(|(k, _)| !k.starts_with('@') && !k.starts_with('#'))
        .collect();
    sites.sort_by_key(|x| std::cmp::Reverse(*x.1));
    eprintln!("  hottest call sites:");
    for (k, v) in sites.into_iter().take(20) {
        let idx = k.split('|').nth(1).unwrap_or("");
        let name = names.get(idx).copied().unwrap_or("?");
        eprintln!("    {v:12}  {:<18}  {k}", name);
    }
}
