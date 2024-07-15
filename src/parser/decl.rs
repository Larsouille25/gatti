//! Module responsible for parsing Gatti's declarations, like functions, type
//! defs, imports

use crate::{derive_loc, Span};

use super::AstNode;

#[derive(Debug, Clone)]
pub struct Declaration {
    pub decl: DeclarationInner,
    pub loc: Span,
}

derive_loc!(Declaration);

impl AstNode for Declaration {
    type Output = Declaration;

    fn parse(parser: &mut super::Parser<'_>) -> crate::errors::PartialResult<Self::Output> {
        todo!()
    }
}

#[derive(Debug, Clone)]
pub enum DeclarationInner {
    Function {
        // name: String,
        // args: Vec<(String, Type)>,
        // ret: Option<Type>,
        // block: Block<Statement>,
    },
}
