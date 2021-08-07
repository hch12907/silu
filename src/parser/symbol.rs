use crate::parser::*;
use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Scope {
    File {
        filename: PathBuf,
    },
    Function {
        filename: PathBuf,
        func_name: Identifier,
        scope_depth: u32,
        shadowed: Option<Box<Scope>>,
    },
}

pub struct SymbolTable {
    inner: HashMap<Identifier, Scope>,
}

impl SymbolTable {
    pub(super) fn new() -> Self {
        Self {
            inner: HashMap::new()
        }
    }

    pub(super) fn add_symbol(&mut self, typedef: Identifier, scope: Scope) {
        match scope {
            Scope::File { .. } => self.inner.insert(typedef, scope),
            Scope::Function { filename, func_name, scope_depth, shadowed } => {
                assert_eq!(shadowed, None);
                match self.inner.remove(&typedef) {
                    Some(original_scope) => self.inner.insert(typedef, Scope::Function {
                        filename,
                        func_name,
                        scope_depth,
                        shadowed: Some(Box::new(original_scope)),
                    }),
                    None => self.inner.insert(typedef, Scope::Function {
                        filename,
                        func_name,
                        scope_depth,
                        shadowed: None,
                    }),
                }
            }
        };
    }

    pub(super) fn leave_scope(&mut self, scope: &Scope) {
        self.inner.retain(|_, sc| {
            let matched = match scope {
                Scope::File { filename: file_scope } => match sc {
                    Scope::File { filename } => filename == file_scope,
                    Scope::Function { filename, .. } => filename == file_scope,
                },

                Scope::Function {
                    filename: file_scope,
                    func_name: func_scope,
                    scope_depth: depth_scope,
                    ..
                } => match sc {
                    Scope::File { .. } => false,
                    Scope::Function { filename, func_name, scope_depth, .. } =>
                        filename == file_scope && func_name == func_scope && *scope_depth > *depth_scope,
                }
            };

            if matched {
                match scope {
                    Scope::File { .. } => true,
                    Scope::Function { .. } => {
                        let shadowed = force_unwrap!(sc, Scope::Function { shadowed, .. } => shadowed);
                        if let Some(shadowed) = shadowed.take() {
                            *sc = *shadowed;
                            true
                        } else {
                            false
                        }
                    }
                }
            } else {
                true
            }
        });
    }

    pub(super) fn is_ident_a_type(&self, typedef: &Identifier, scope: &Scope) -> bool {
        let typedef_scope = match self.inner.get(typedef) {
            Some(scope) => scope,
            None => return false,
        };

        match typedef_scope {
            Scope::File { filename: file_typedef } => match scope {
                Scope::File { filename } => file_typedef == filename,
                Scope::Function { filename, .. } => file_typedef == filename,
            },

            Scope::Function {
                filename: file_typedef,
                func_name: func_typedef,
                scope_depth: scope_typedef,
                ..
            } => match scope {
                Scope::File { .. } => false,
                Scope::Function { filename, func_name, scope_depth, .. } =>
                    filename == filename && func_name == func_name && scope_typedef <= scope_depth,
            }
        }
    }
}
