use crate::value_type::TypeSet;

#[derive(Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Ty {
    pub kinds: TypeSet,
    /// What the container kinds in `kinds` hold, or `TOP` when unknown.
    pub elements: TypeSet,
    /// The keys of a hash map in `kinds`, or `TOP` when unknown.
    pub keys: TypeSet,
}

/// The kinds `elements` describe.
const CONTAINERS: TypeSet = TypeSet::LIST
    .with(TypeSet::PAIR)
    .with(TypeSet::VECTOR)
    .with(TypeSet::HASHSET)
    .with(TypeSet::HASHMAP);

impl Ty {
    pub const BOTTOM: Ty = Ty::kinds(TypeSet::BOTTOM);
    pub const TOP: Ty = Ty::kinds(TypeSet::TOP);
    pub const BOOL: Ty = Ty::kinds(TypeSet::BOOL);
    pub const INT: Ty = Ty::kinds(TypeSet::INT);
    pub const FLOAT: Ty = Ty::kinds(TypeSet::FLOAT);
    pub const OTHER_NUMBER: Ty = Ty::kinds(TypeSet::OTHER_NUMBER);
    pub const NUMBER: Ty = Ty::kinds(TypeSet::NUMBER);
    pub const CHAR: Ty = Ty::kinds(TypeSet::CHAR);
    pub const STRING: Ty = Ty::kinds(TypeSet::STRING);
    pub const SYMBOL: Ty = Ty::kinds(TypeSet::SYMBOL);
    pub const LIST: Ty = Ty::kinds(TypeSet::LIST);
    pub const PAIR: Ty = Ty::kinds(TypeSet::PAIR);
    pub const VECTOR: Ty = Ty::kinds(TypeSet::VECTOR);
    pub const MUTABLE_VECTOR: Ty = Ty::kinds(TypeSet::MUTABLE_VECTOR);
    pub const HASHMAP: Ty = Ty::kinds(TypeSet::HASHMAP);
    pub const HASHSET: Ty = Ty::kinds(TypeSet::HASHSET);
    pub const BYTEVECTOR: Ty = Ty::kinds(TypeSet::BYTEVECTOR);
    pub const VOID: Ty = Ty::kinds(TypeSet::VOID);
    pub const PROCEDURE: Ty = Ty::kinds(TypeSet::PROCEDURE);
    pub const OTHER: Ty = Ty::kinds(TypeSet::OTHER);
    pub const STRUCTS: Ty = Ty::kinds(TypeSet::STRUCTS);

    /// Values of `kinds`, holding anything.
    pub const fn kinds(kinds: TypeSet) -> Ty {
        Ty {
            kinds,
            elements: TypeSet::TOP,
            keys: TypeSet::TOP,
        }
    }

    /// A container of `kinds` holding `elements` (and, for hash maps, `keys`).
    pub fn holding(kinds: TypeSet, elements: TypeSet, keys: TypeSet) -> Ty {
        Ty {
            kinds,
            elements,
            keys,
        }
    }

    /// A proper list of `elements`.
    pub fn list_of(elements: TypeSet) -> Ty {
        Ty::holding(TypeSet::LIST, elements, TypeSet::BOTTOM)
    }

    pub fn structure(index: usize) -> Ty {
        Ty::kinds(TypeSet::structure(index))
    }

    pub fn from_value_type(ty: crate::value_type::ValueType) -> Ty {
        Ty::kinds(TypeSet::from_value_type(ty))
    }

    pub fn from_arg_type(ty: crate::value_type::ArgType) -> Ty {
        Ty::kinds(TypeSet::from_arg_type(ty))
    }

    /// `elements` and `keys` only describe container kinds that are there.
    /// Two types that differ only in what an absent container would hold are
    /// the same.
    fn normal(self) -> Ty {
        Ty {
            kinds: self.kinds,
            elements: if self.kinds.intersect(CONTAINERS).is_bottom() {
                TypeSet::BOTTOM
            } else {
                self.elements
            },
            keys: if self.kinds.intersect(TypeSet::HASHMAP).is_bottom() {
                TypeSet::BOTTOM
            } else {
                self.keys
            },
        }
    }

    pub fn union(self, other: Ty) -> Ty {
        let (a, b) = (self.normal(), other.normal());
        Ty {
            kinds: a.kinds.union(b.kinds),
            elements: a.elements.union(b.elements),
            keys: a.keys.union(b.keys),
        }
        .normal()
    }

    /// The values in both. A container in both holds what both say it holds.
    pub fn intersect(self, other: Ty) -> Ty {
        Ty {
            kinds: self.kinds.intersect(other.kinds),
            elements: self.elements.intersect(other.elements),
            keys: self.keys.intersect(other.keys),
        }
        .normal()
    }

    /// Without the kinds of `other`; what the rest hold is unchanged.
    pub fn without(self, other: Ty) -> Ty {
        Ty {
            kinds: self.kinds.without(other.kinds),
            ..self
        }
        .normal()
    }

    pub fn is_bottom(self) -> bool {
        self.kinds.is_bottom()
    }

    pub fn is_top(self) -> bool {
        self.normal() == Ty::TOP.normal()
    }

    /// Whether every value of `other` is a value of this type.
    pub fn contains(self, other: Ty) -> bool {
        let (a, b) = (self.normal(), other.normal());
        a.kinds.contains(b.kinds) && a.elements.contains(b.elements) && a.keys.contains(b.keys)
    }

    /// Like [`TypeSet::show`], with what a container holds when that is known
    /// and it is the only container kind: `(listof int)`,
    /// `(hash string int)`.
    pub fn show(self, structs: &[String]) -> String {
        let ty = self.normal();
        let containers = ty.kinds.intersect(CONTAINERS);
        let precise = !ty.elements.is_top() && !ty.elements.is_bottom();
        let single = [
            (TypeSet::LIST, "listof"),
            (TypeSet::VECTOR, "vectorof"),
            (TypeSet::HASHSET, "setof"),
            (TypeSet::HASHMAP, "hash"),
        ]
        .into_iter()
        .find(|(kind, _)| containers == *kind);
        let Some((kind, name)) = single.filter(|_| precise) else {
            return ty.kinds.show(structs);
        };
        let elements = wrap(ty.elements.show(structs));
        let container = if kind == TypeSet::HASHMAP {
            if ty.keys.is_top() || ty.keys.is_bottom() {
                format!("(hash any {elements})")
            } else {
                format!("(hash {} {elements})", wrap(ty.keys.show(structs)))
            }
        } else {
            format!("({name} {elements})")
        };
        let rest = ty.kinds.without(kind);
        if rest.is_bottom() {
            container
        } else {
            format!("{container} | {}", rest.show(structs))
        }
    }
}

/// A union shown inside a container, parenthesized.
fn wrap(shown: String) -> String {
    if shown.contains(' ') && !shown.starts_with('(') {
        format!("({shown})")
    } else {
        shown
    }
}

impl From<TypeSet> for Ty {
    fn from(kinds: TypeSet) -> Ty {
        Ty::kinds(kinds)
    }
}

impl core::fmt::Display for Ty {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.write_str(&self.show(&[]))
    }
}

impl core::fmt::Debug for Ty {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        core::fmt::Display::fmt(self, f)
    }
}
