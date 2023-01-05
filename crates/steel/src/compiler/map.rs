use crate::rvals::Result;
use crate::throw;
use std::collections::HashMap;

// TODO -> use hashmap speed up access
#[derive(Debug, PartialEq)]
pub struct SymbolMap {
    values: Vec<String>,
    // TODO don't do this - don't expose this API
    pub(crate) map: HashMap<String, usize>,
}

impl Default for SymbolMap {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolMap {
    pub fn new() -> Self {
        SymbolMap {
            values: Vec::new(),
            map: HashMap::new(),
        }
    }

    // pub fn add(&mut self, ident: &str) -> usize {
    //     let idx = self.values.len();
    //     self.values.push(ident.to_string());
    //     // println!("`add`: {} @ {}", ident, idx);
    //     idx
    // }

    pub fn get_or_add(&mut self, ident: &str) -> usize {
        // let rev_iter = self.values.iter().enumerate().rev();

        // for (idx, val) in rev_iter {
        //     if val == ident {
        //         return idx;
        //     }
        // }

        // let idx = self.values.len();
        // self.values.push(ident.to_string());

        // idx

        if let Some(idx) = self.map.get(ident) {
            // if ident == "b" {
            //     println!("Fetching from map: {}", idx);
            // }

            *idx
        } else {
            let idx = self.map.len();

            // if ident == "b" {
            //     println!("Adding to map: {}", idx);
            // }

            self.map.insert(ident.to_string(), idx);

            idx
        }
    }

    // fallible
    pub fn get(&self, ident: &str) -> Result<usize> {
        // let rev_iter = self.values.iter().enumerate().rev();

        // for (idx, val) in rev_iter {
        //     // println!("{}", idx);
        //     if val == ident {
        //         return Ok(idx);
        //     }
        // }
        // let e = ident.to_string();
        // // TODO come back to this - this is causing things to be renamed to 0 regardless
        // stop!(FreeIdentifier => e)

        self.map
            .get(ident)
            .copied()
            .ok_or_else(throw!(FreeIdentifier => ident.to_string()))
    }

    // // TODO -> want to package the metadata up for declaring structs
    // // into a program so that someone can take a binary and load it correctly
    // // probably front load the program with a vector of strings declaring what struct functions will inevitably
    // // be declared
    // pub fn insert_struct_function_names_from_concrete<'a>(
    //     &mut self,
    //     struct_builder: &'a StructFuncBuilderConcrete,
    // ) -> Vec<usize> {
    //     let mut indices = Vec::new();

    //     // Constructor
    //     indices.push(self.get_or_add(&struct_builder.name));
    //     // Predicate
    //     indices.push(self.get_or_add(format!("{}?", &struct_builder.name).as_str()));
    //     for field in &struct_builder.fields {
    //         // Getter
    //         indices.push(self.get_or_add(format!("{}-{}", &struct_builder.name, field).as_str()));
    //         // Setter
    //         indices
    //             .push(self.get_or_add(format!("set-{}-{}!", &struct_builder.name, field).as_str()));
    //     }

    //     indices
    // }

    // // TODO -> want to package the metadata up for declaring structs
    // // into a program so that someone can take a binary and load it correctly
    // // probably front load the program with a vector of strings declaring what struct functions will inevitably
    // // be declared
    // pub fn insert_struct_function_names<'a>(
    //     &mut self,
    //     struct_builder: &'a StructFuncBuilder,
    // ) -> Vec<usize> {
    //     let mut indices = Vec::new();

    //     // Constructor
    //     indices.push(self.get_or_add(&struct_builder.name));
    //     // Predicate
    //     indices.push(self.get_or_add(format!("{}?", &struct_builder.name).as_str()));
    //     for field in &struct_builder.fields {
    //         // Getter
    //         indices.push(self.get_or_add(format!("{}-{}", &struct_builder.name, field).as_str()));
    //         // Setter
    //         indices
    //             .push(self.get_or_add(format!("set-{}-{}!", &struct_builder.name, field).as_str()));
    //     }

    //     indices
    // }
}
