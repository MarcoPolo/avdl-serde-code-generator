extern crate pest;
#[macro_use]
extern crate pest_derive;

pub mod to_rust;

#[derive(Parser)]
#[grammar = "avdl.pest"]
pub struct AVDLParser;
