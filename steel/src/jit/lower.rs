use crate::parser::ast;
use crate::parser::visitors::VisitorMut;

use im_rc::HashSet;

// ((lambda (x y z) (+ x y z)) 10 20 30)
// =>
// (x = 10)
// (y = 20)
// (z = 30)
// (+ x (+ y z))
struct RenameScopedVars {}
