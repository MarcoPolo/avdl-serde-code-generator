#![allow(dead_code)]
use crate::Rule;
use pest::iterators::{Pair, Pairs};
use regex::Regex;
use std::{
  cell::RefCell,
  error::Error,
  fmt::Write as FmtWrite,
  io::{self, Write},
  str::FromStr,
};

const DERIVES: &'static str = "Serialize, Deserialize, Debug";
const DERIVES_WITH_HASH: &'static str = "Serialize, Deserialize, Debug, Hash, PartialEq, Eq";
const DERIVES_WITH_HASH_ENUM: &'static str =
  "Serialize_repr, Deserialize_repr, Debug, Hash, PartialEq, Eq";

thread_local! {
    pub static ADD_DERIVE: RefCell<bool> = RefCell::new(false);
    pub static DERIVE_HASH_FOR_IDENT: RefCell<Vec<String>> = RefCell::new(vec![]);

}

pub fn set_derive_hash_for_ident(v: Vec<String>) {
  DERIVE_HASH_FOR_IDENT.with(|t| {
    *t.borrow_mut() = v;
  });
}

fn should_derive_hash(v: &str) -> bool {
  DERIVE_HASH_FOR_IDENT.with(|t| t.borrow().iter().any(|s| s.as_str() == v))
}

pub fn set_add_derive(v: bool) {
  ADD_DERIVE.with(|add_derive| {
    *add_derive.borrow_mut() = v;
  })
}

fn convert_path_str_to_rust_mod(path: &str, as_name: &str) -> String {
  let path = String::from(path);
  let has_parts = path.contains("/");
  let parts: Vec<&str> = path.split("/").collect();
  let is_as_name_same = as_name == "" || parts.last().unwrap() == &as_name;

  let mut module = if has_parts {
    if parts.get(0) == Some(&"..") {
      let mut s = String::from("super::");
      s.push_str(
        &parts
          .into_iter()
          .map(|part| if part == ".." { "super" } else { part })
          .collect::<Vec<&str>>()
          .join("::"),
      );
      s
    } else {
      let important_parts = parts
        .into_iter()
        .skip_while(|s| s != &"protocol")
        .collect::<Vec<&str>>();
      format!("crate::{}", important_parts.join("::"))
    }
  } else {
    format!("super::{}::*", path.replace(".avdl", ""))
  };

  if !is_as_name_same {
    module.push_str(" as ");
    module.push_str(as_name);
  }
  module.push_str(";");

  module
}

fn convert_to_import<W>(w: &mut W, p: Pair<Rule>) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  assert_eq!(p.as_rule(), Rule::import);
  let parts = p.into_inner().take(3).collect::<Vec<Pair<Rule>>>();
  let (kind, path, as_name) = (&parts[0], &parts[1], parts.get(2));
  match kind.as_str() {
    "idl" => {
      write!(
        w,
        "use {}",
        convert_path_str_to_rust_mod(path.as_str(), as_name.map(|p| p.as_str()).unwrap_or(""))
      )?;
    }
    _ => panic!("Unhandled import kind (not idl)"),
  }

  Ok(())
}

// Hack to handle cases where the struct is actually infinite size.
// We make the type a Box<T> when it's used.
fn should_box(idl_type: &str) -> bool {
  match idl_type {
    "MessageUnboxed" | "UIMessage" => true,
    _ => false,
  }
}

fn convert_idl_type_to_rust_type(idl_type: &str) -> String {
  if should_box(idl_type) {
    return format!("Box<{}>", idl_type);
  }
  match idl_type {
    // "bytes" => String::from("Vec<u8>"),
    "bytes" => String::from("String"),
    "boolean" => String::from("bool"),
    "uint" => String::from("u32"),
    "int" => String::from("i32"),
    "int64" => String::from("i64"),
    "long" => String::from("i64"),
    "double" => String::from("f64"),
    "uint64" => String::from("u64"),
    "uint32" => String::from("u32"),
    "array" => String::from("Vec"),
    "string" => String::from("String"),
    "Hash" => String::from("String"),
    "void" => String::from("()"),
    "map" => String::from("std::collections::HashMap"),
    _ => idl_type.into(),
  }
}

fn convert_dot_to_sep(a: &str) -> String {
  a.split(".").collect::<Vec<&str>>().join("::")
}

enum AVDLType {
  Simple(String),
  Maybe(String),
  Union(),
}

impl ToString for AVDLType {
  fn to_string(&self) -> String {
    match self {
      AVDLType::Simple(s) => s.clone(),
      AVDLType::Maybe(s) => format!("Option<{}>", s),
      AVDLType::Union() => panic!("Not implemented"),
    }
  }
}

struct AVDLSimpleType {
  ty: Option<String>,
  generics: Vec<AVDLType>,
}

impl ToString for AVDLSimpleType {
  fn to_string(&self) -> String {
    let mut s = self.ty.as_ref().unwrap().clone();
    if !self.generics.is_empty() {
      write!(
        &mut s,
        "<{}>",
        self
          .generics
          .iter()
          .map(|g| g.to_string())
          .collect::<Vec<String>>()
          .join(", ")
      )
      .unwrap();
    }
    s
  }
}

impl<'a> From<Pair<'a, Rule>> for AVDLSimpleType {
  fn from(pair: Pair<'a, Rule>) -> Self {
    assert_eq!(pair.as_rule(), Rule::simple_ty);
    let parts = pair.into_inner();
    let mut ty = AVDLSimpleType {
      ty: None,
      generics: vec![],
    };

    let transform = |p: &Pair<Rule>| convert_dot_to_sep(&convert_idl_type_to_rust_type(p.as_str()));

    for pair in parts {
      match pair.as_rule() {
        Rule::ns_ident => ty.ty = Some(transform(&pair)),
        Rule::ty => ty.generics.push(pair.into()),
        _ => unreachable!(),
      }
    }

    if ty.ty.as_ref().map(|s| s.as_str()) == Some("std::collections::HashMap")
      && ty.generics.len() == 1
    {
      ty.generics
        .insert(0, AVDLType::Simple(String::from("String")));
    }

    ty
  }
}

impl<'a> From<Pair<'a, Rule>> for AVDLType {
  fn from(pair: Pair<'a, Rule>) -> Self {
    assert!(
      pair.as_rule() == Rule::ty || pair.as_rule() == Rule::maybe_ty,
      "Unexpected rule: {:?}",
      pair.as_rule()
    );

    let inner = pair.into_inner().next().unwrap();
    match inner.as_rule() {
      Rule::simple_ty => {
        let ty: AVDLSimpleType = inner.into();
        AVDLType::Simple(ty.to_string())
      }
      Rule::maybe_ty => {
        let inner_ty: AVDLType = inner.into();
        AVDLType::Maybe(inner_ty.to_string())
      }
      _ => panic!("Unhandled case: {:?}", inner.as_rule()),
    }
  }
}

struct AVDLIdent(String);
impl<'a> From<Pair<'a, Rule>> for AVDLIdent {
  fn from(pair: Pair<'a, Rule>) -> Self {
    assert_eq!(pair.as_rule(), Rule::ident);
    let s = match pair.as_str() {
      "box" => "box_",
      "match" => "match_",
      "ref" => "ref_",
      "type" => "ty",
      "self" => "self_",
      "where" => "where_",
      _ => pair.as_str(),
    };
    AVDLIdent(s.into())
  }
}

fn convert_typedef<W>(w: &mut W, p: Pair<Rule>) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  assert_eq!(p.as_rule(), Rule::typedef);
  let mut parts = p.into_inner();
  let mut type_name: Option<String> = None;
  let mut type_target: Option<String> = None;
  while let Some(pair) = parts.next() {
    match pair.as_rule() {
      Rule::ty => {
        let ty: AVDLType = pair.into();
        type_target = Some(ty.to_string());
      }
      Rule::lint => {
        write!(w, "// LINT: {}\n", pair.as_str())?;
      }
      Rule::record => type_name = Some(pair.into_inner().next().unwrap().as_str().into()),
      _ => unreachable!(),
    }
  }

  write!(
    w,
    "pub type {} = {};",
    type_name.unwrap(),
    type_target.unwrap()
  )?;

  Ok(())
}

struct EnumCaseTy {
  enum_name: String,
  comment: Option<String>,
  preline_comment: Option<String>,
}

impl<'a> From<Pair<'a, Rule>> for EnumCaseTy {
  fn from(pair: Pair<'a, Rule>) -> Self {
    let mut parts = pair.into_inner();
    let mut enum_name: Option<String> = None;
    let mut comment: Option<String> = None;
    while let Some(pair) = parts.next() {
      match pair.as_rule() {
        Rule::ident => enum_name = Some(quiet_voice(pair.into())),
        Rule::comment => comment = Some(format!(" {}", pair.as_str())),
        _ => unreachable!(),
      }
    }

    EnumCaseTy {
      enum_name: enum_name.unwrap(),
      comment,
      preline_comment: None,
    }
  }
}

impl WriteTo for EnumCaseTy {
  fn write_to<W: Write>(&self, w: &mut W) -> Result<(), io::Error> {
    if let Some(preline_comment) = self.preline_comment.as_ref() {
      write!(w, "  {}", preline_comment)?;
    }
    write!(
      w,
      "  {},{}",
      self.enum_name,
      self.comment.as_ref().map(|s| s.as_str()).unwrap_or("\n")
    )?;
    Ok(())
  }
}

struct EnumTy {
  ident: String,
  cases: Vec<EnumCaseTy>,
}

impl<'a> From<Pair<'a, Rule>> for EnumTy {
  fn from(pair: Pair<'a, Rule>) -> Self {
    let mut parts = pair.into_inner();
    let mut ident: Option<String> = None;
    let mut comment: Option<String> = None;
    let mut cases: Vec<EnumCaseTy> = vec![];
    while let Some(pair) = parts.next() {
      match pair.as_rule() {
        Rule::ident => ident = Some(AVDLIdent::from(pair).0),
        Rule::enum_case => {
          let mut case: EnumCaseTy = pair.into();
          case.preline_comment = comment.take();
          cases.push(case);
        }
        Rule::comment => comment = Some(pair.as_str().into()),
        _ => unreachable!(),
      }
    }

    EnumTy {
      ident: ident.expect("Couldn't find name for variant"),
      cases,
    }
  }
}

impl WriteTo for EnumTy {
  fn write_to<W: Write>(&self, w: &mut W) -> Result<(), io::Error> {
    write!(
      w,
      "#[derive({})]\n#[repr(u8)]\npub enum {} {{\n",
      DERIVES_WITH_HASH_ENUM, self.ident
    )?;
    for case in self.cases.iter() {
      case.write_to(w)?;
    }
    write!(w, "}}")?;
    Ok(())
  }
}

fn convert_enum<W>(w: &mut W, p: Pair<Rule>) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  assert_eq!(p.as_rule(), Rule::enum_ty);
  // This is actually already a rust enum!
  let ty: EnumTy = p.into();
  ty.write_to(w)?;

  // write!(w, "{}", p.as_str())?;
  Ok(())
}

// Turns FOO -> Foo
fn quiet_voice(s: AVDLIdent) -> String {
  s.0
    .chars()
    .enumerate()
    .map(|(i, c)| {
      if i == 0 {
        c.to_ascii_uppercase()
      } else {
        c.to_ascii_lowercase()
      }
    })
    .collect()
}

struct VariantCaseTy {
  enum_name: String,
  enum_inner_ty: String,
  comment: Option<String>,
}

impl<'a> From<Pair<'a, Rule>> for VariantCaseTy {
  fn from(pair: Pair<'a, Rule>) -> Self {
    let mut parts = pair.into_inner();
    let mut enum_name: Option<String> = None;
    let mut enum_inner_ty: Option<AVDLType> = None;
    let mut comment: Option<String> = None;
    while let Some(pair) = parts.next() {
      match pair.as_rule() {
        Rule::ident => enum_name = Some(quiet_voice(pair.into())),
        Rule::ty => enum_inner_ty = Some(pair.into()),
        Rule::comment => comment = Some(format!(" {}", pair.as_str())),
        _ => unreachable!(),
      }
    }

    VariantCaseTy {
      enum_name: enum_name.unwrap(),
      enum_inner_ty: enum_inner_ty.unwrap().to_string(),
      comment,
    }
  }
}

impl WriteTo for VariantCaseTy {
  fn write_to<W: Write>(&self, w: &mut W) -> Result<(), io::Error> {
    if self.enum_name == "Default" {
      writeln!(w, "  {} {{}},", self.enum_name)?;
    } else {
      let enum_lowercase = match self.enum_name.as_str() {
        "Move" => String::from("r#move"),
        s => s.to_ascii_lowercase(),
      };
      writeln!(
        w,
        "  {} {{{}: {}}},",
        self.enum_name, enum_lowercase, self.enum_inner_ty
      )?;
    }
    Ok(())
  }
}

struct VariantTy {
  ident: String,
  cases: Vec<VariantCaseTy>,
}

impl<'a> From<Pair<'a, Rule>> for VariantTy {
  fn from(pair: Pair<'a, Rule>) -> Self {
    let mut parts = pair.into_inner();
    let mut ident: Option<String> = None;
    let mut cases: Vec<VariantCaseTy> = vec![];
    while let Some(pair) = parts.next() {
      match pair.as_rule() {
        Rule::ident => ident = Some(pair.as_str().into()),
        Rule::variant_case => cases.push(pair.into()),
        Rule::variant_param => {}
        _ => unreachable!(),
      }
    }

    VariantTy {
      ident: ident.expect("Couldn't find name for variant"),
      cases,
    }
  }
}

impl WriteTo for VariantTy {
  fn write_to<W: Write>(&self, w: &mut W) -> Result<(), io::Error> {
    let added_derives = write_derives(w, false).unwrap();
    if added_derives {
      write!(w, "#[serde(untagged)]\n")?;
    }
    write!(w, "pub enum {} {{\n", self.ident)?;
    for case in self.cases.iter() {
      case.write_to(w)?;
    }
    write!(w, "}}")?;
    Ok(())
  }
}

fn convert_variant<W>(w: &mut W, p: Pair<Rule>) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  assert_eq!(p.as_rule(), Rule::variant_ty);
  let ty: VariantTy = p.into();
  ty.write_to(w)?;
  Ok(())
}

fn convert_fixed<W>(w: &mut W, p: Pair<Rule>) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  assert_eq!(p.as_rule(), Rule::fixed_ty);
  let mut parts = p.into_inner();
  let mut ty: Option<AVDLType> = None;
  let mut _byte_size: usize = 0;
  while let Some(pair) = parts.next() {
    match pair.as_rule() {
      Rule::ty => ty = Some(pair.into()),
      Rule::byte_size => {
        _byte_size = usize::from_str(pair.as_str()).expect("Couldn't parse byte_size")
      }
      _ => unreachable!(),
    }
  }

  write!(
    w,
    // This is the correct one, but doesn't serialize/deserialize easily
    // "pub type {} = [u8;{}];",
    "pub type {} = Vec<u8>;",
    ty.unwrap().to_string(),
    // byte_size
  )?;
  Ok(())
}

struct JSONKey {
  rename_to: String,
}
impl<'a> From<&str> for JSONKey {
  fn from(s: &str) -> Self {
    let re = Regex::new(r#"@jsonkey\("([^"]+)"\)"#).unwrap();
    let mut captures = re.captures_iter(s);
    JSONKey {
      rename_to: captures.next().expect("Regex didn't match")[1].into(),
    }
  }
}

struct AVDLRecordProp {
  ty: AVDLType,
  field: String,
  attributes: Vec<String>,
}

impl AVDLRecordProp {
  fn can_hash(&self) -> bool {
    match self.ty.to_string().as_str() {
      "f64" => false,
      _ => true,
    }
  }
}

pub trait WriteTo {
  fn write_to<W: Write>(&self, w: &mut W) -> Result<(), io::Error>;
}

impl WriteTo for AVDLRecordProp {
  fn write_to<W: Write>(&self, w: &mut W) -> Result<(), io::Error> {
    let mut json_key: Option<JSONKey> = None;
    let add_derive = ADD_DERIVE.with(|add_derive| *add_derive.borrow());
    let mut is_optional = add_derive;
    let mut add_default = false;
    let mut is_bytes = false;
    for attr in self.attributes.iter() {
      if attr.contains("@jsonkey") {
        json_key = Some(attr.as_str().into())
      } else if attr.contains("@optional(true)") {
        is_optional = true;
      } else {
        write!(w, "  // {}\n", attr)?;
      }
    }

    if json_key.is_none() && add_derive {
      match self.field.as_str() {
        "ty" => write!(w, "  #[serde(rename = \"type\")]\n").unwrap(),
        "box_" => write!(w, "  #[serde(rename = \"box\")]\n").unwrap(),
        "match_" => write!(w, "  #[serde(rename = \"match\")]\n").unwrap(),
        "ref_" => write!(w, "  #[serde(rename = \"ref\")]\n").unwrap(),
        "self_" => write!(w, "  #[serde(rename = \"self\")]\n").unwrap(),
        "where_" => write!(w, "  #[serde(rename = \"where\")]\n").unwrap(),
        _ => {}
      }
    }

    if let Some(json_key) = json_key {
      if add_derive {
        write!(w, "  #[serde(rename = \"{}\")]\n", json_key.rename_to).unwrap();
      }
    }

    let ty_str = self.ty.to_string();
    if ty_str == "String" || ty_str.contains("Vec") {
      add_default = true;
      is_optional = true;
    }

    if ty_str.contains("Vec<u8>") {
      is_bytes = true;
    }

    if ty_str == "bool" {
      add_default = true;
    }

    if ty_str.contains("Option<") {
      is_optional = false;
    }

    if add_default && add_derive {
      write!(w, "  #[serde(default)]\n").unwrap();
    }

    if is_bytes && add_derive {
      // write!(w, r#"  #[serde(with = "Base64Standard")]\n"#).unwrap();
    }

    if is_optional {
      write!(
        w,
        "  pub {}: Option<{}>,\n",
        self.field,
        self.ty.to_string()
      )?;
    } else {
      write!(w, "  pub {}: {},\n", self.field, self.ty.to_string())?;
    }
    Ok(())
  }
}

impl<'a> From<Pair<'a, Rule>> for AVDLRecordProp {
  fn from(pair: Pair<'a, Rule>) -> Self {
    assert_eq!(pair.as_rule(), Rule::record_prop);
    let mut ty: Option<AVDLType> = None;
    let mut field: Option<String> = None;
    let mut attributes = vec![];
    let mut parts = pair.into_inner();
    while let Some(pair) = parts.next() {
      match pair.as_rule() {
        Rule::lint | Rule::generic_annotation => attributes.push(pair.as_str().into()),
        Rule::ty => ty = Some(pair.into()),
        Rule::ident => {
          let ident: AVDLIdent = pair.into();
          field = Some(ident.0);
        }
        _ => panic!("Unhandled case: {:?}", pair),
      }
    }

    AVDLRecordProp {
      ty: ty.expect("Couldn't find types"),
      field: field.expect("Couldn't find field"),
      attributes,
    }
  }
}

fn write_derives<W>(w: &mut W, can_hash: bool) -> Result<bool, Box<dyn Error>>
where
  W: Write,
{
  let did_add = ADD_DERIVE.with(|add_derive| {
    if *add_derive.borrow() {
      write!(
        w,
        "#[derive({})]\n",
        if can_hash { DERIVES_WITH_HASH } else { DERIVES }
      )
      .unwrap();
      true
    } else {
      false
    }
  });

  Ok(did_add)
}

fn convert_record<W>(w: &mut W, p: Pair<Rule>) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  assert_eq!(p.as_rule(), Rule::record);
  let mut parts = p.into_inner();
  let mut type_name: Option<AVDLType> = None;
  let mut record_props: Vec<AVDLRecordProp> = vec![];

  while let Some(pair) = parts.next() {
    match pair.as_rule() {
      Rule::ty => type_name = Some(pair.into()),
      Rule::comment => write!(w, "{}", pair.as_str())?,
      Rule::record_prop => {
        record_props.push(pair.into());
      }
      _ => panic!("Unhandled case: {:?}", pair),
    }
  }

  // let can_hash = false;
  let ident = type_name.expect("No Record name").to_string();
  let can_hash = should_derive_hash(&ident);
  println!("Can Hash {} {}", ident, can_hash);

  write_derives(w, can_hash)?;
  write!(w, "pub struct {} {{\n", ident)?;
  for prop in record_props.into_iter() {
    prop.write_to(w)?;
  }
  write!(w, "}}")?;

  Ok(())
}

fn convert_interface_fn<W>(_w: &mut W, _p: Pair<Rule>) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  // Not implemented. Skipping for now
  Ok(())
}

pub fn build_rust_code_from_avdl<W>(mut input: Pairs<Rule>, w: &mut W) -> Result<(), Box<dyn Error>>
where
  W: Write,
{
  for node in input.next().expect("Nothing to parse").into_inner() {
    match node.as_rule() {
      Rule::namespace_annotation => {
        if let Some(n) = node.into_inner().next() {
          match n.as_rule() {
            Rule::namespace_name => write!(w, "// Namespace: {:?}\n", n.as_str())?,
            _ => unreachable!(),
          }
        }
      }
      Rule::protocol => {
        let mut inner = node.into_inner();
        while let Some(n) = inner.next() {
          match n.as_rule() {
            Rule::protocol_name => {
              let protocol_name = n.as_str();
              write!(w, "// Protocol: {:?}\n", protocol_name)?;
              write!(w, "#![allow(dead_code)]\n")?;
              write!(w, "#![allow(non_snake_case)]\n")?;
              write!(w, "#![allow(non_camel_case_types)]\n")?;
              write!(w, "#![allow(unused_imports)]\n")?;

              write!(w, "use serde::{{Serialize, Deserialize}};\n")?;
              write!(w, "use serde_repr::{{Deserialize_repr, Serialize_repr}};")?;
              write!(w, "use super::*;\n")?;
              if protocol_name.to_ascii_lowercase() != "common" {
                // write!(w, "use super::common::*;\n")?
                // write!(w, "use super::*;\n")?
              }
            }
            Rule::protocol_body => {
              let mut inner = n.into_inner();
              while let Some(protocol_body_node) = inner.next() {
                let separator = match protocol_body_node.as_rule() {
                  Rule::comment => "",
                  Rule::generic_annotation | Rule::import => "\n",
                  _ => "\n\n",
                };

                match protocol_body_node.as_rule() {
                  Rule::comment => write!(w, "{}", protocol_body_node.as_str())?,
                  Rule::import => convert_to_import(w, protocol_body_node)?,
                  Rule::typedef => convert_typedef(w, protocol_body_node)?,
                  Rule::generic_annotation => write!(w, "// {}", protocol_body_node.as_str())?,
                  Rule::record => convert_record(w, protocol_body_node)?,
                  Rule::enum_ty => convert_enum(w, protocol_body_node)?,
                  Rule::variant_ty => convert_variant(w, protocol_body_node)?,
                  Rule::fixed_ty => convert_fixed(w, protocol_body_node)?,
                  Rule::interface_fn => convert_interface_fn(w, protocol_body_node)?,
                  _ => {}
                }

                write!(w, "{}", separator)?;
              }
            }
            _ => unreachable!(),
          }
          write!(w, "\n")?;
        }
      }
      Rule::generic_annotation => {}
      _ => unreachable!(),
    }
  }
  Ok(())
}

#[cfg(test)]
mod tests {
  use super::*;
  use crate::AVDLParser;
  use pest::Parser;

  fn test_conversion<F>(
    r: Rule,
    conversion_fn: F,
    input: &str,
    expected: &str,
  ) -> Result<(), Box<dyn Error>>
  where
    F: Fn(&mut Vec<u8>, Pair<Rule>) -> Result<(), Box<dyn Error>>,
  {
    let mut output = vec![];
    conversion_fn(&mut output, AVDLParser::parse(r, input)?.next().unwrap())?;
    assert_eq!(String::from_utf8(output).unwrap(), expected);
    Ok(())
  }

  #[test]
  fn test_type_conversion() {
    {
      let input = AVDLParser::parse(Rule::ty, "map<TeamInviteID,AnnotatedTeamInvite>");
      let ty: AVDLType = input.unwrap().next().unwrap().into();
      assert_eq!(
        ty.to_string(),
        "std::collections::HashMap<TeamInviteID, AnnotatedTeamInvite>"
      );
    }
    {
      let input = AVDLParser::parse(Rule::ty, "map<int>");
      let ty: AVDLType = input.unwrap().next().unwrap().into();
      assert_eq!(ty.to_string(), "std::collections::HashMap<String, i32>");
    }
  }

  #[test]
  fn test_convert_import() -> Result<(), Box<dyn Error>> {
    test_conversion(
      Rule::import,
      convert_to_import,
      r#"import idl "chat_ui.avdl";"#,
      "use super::chat_ui::*;",
    )
    .unwrap();
    test_conversion(
      Rule::import,
      convert_to_import,
      r#"import idl "github.com/keybase/client/go/protocol/gregor1" as gregor1;"#,
      "use crate::protocol::gregor1;",
    )
    .unwrap();
    test_conversion(
      Rule::import,
      convert_to_import,
      r#"import idl "github.com/keybase/client/go/protocol/gregor1" as otherGregor;"#,
      "use crate::protocol::gregor1 as otherGregor;",
    )
    .unwrap();

    Ok(())
  }

  #[test]
  fn test_interface_fn() {
    // test_conversion(
    //   Rule::typedef,
    //   convert_typedef,
    //   r#"GetInboxAndUnboxLocalRes getInboxAndUnboxLocal(union { null, GetInboxLocalQuery} query, union { null, Pagination } pagination, keybase1.TLFIdentifyBehavior identifyBehavior);"#,
    //   r#"fn getInboxAndUnboxLocal(&self, query: Option<GetInboxLocalQuery> query, pagination: Option<Pagination>, identifyBehavior: keybase1.TLFIdentifyBehavior) -> GetInboxAndUnboxLocalRes;"#,
    // )
    // .unwrap();
    test_conversion(
      Rule::interface_fn,
      convert_interface_fn,
      r#"GetInboxAndUnboxLocalRes getInboxAndUnboxLocal(union { null, GetInboxLocalQuery} query, union { null, Pagination } pagination, keybase1.TLFIdentifyBehavior identifyBehavior);"#,
      r#""#,
    )
    .unwrap();
    test_conversion(
      Rule::interface_fn,
      convert_interface_fn,
      r#"  UnreadlineRes getUnreadline(int sessionID, ConversationID convID,
MessageID readMsgID, keybase1.TLFIdentifyBehavior identifyBehavior);"#,
      r#""#,
    )
    .unwrap();
    test_conversion(
      Rule::interface_fn,
      convert_interface_fn,
      r#"    OutboxID generateOutboxID();"#,
      r#""#,
    )
    .unwrap();

    test_conversion(
      Rule::interface_fn,
      convert_interface_fn,
      r#"  void start(int sessionID, string username, IdentifyReason reason, boolean forceDisplay=false);"#,
      r#""#,
    )
    .unwrap();

    test_conversion(
      Rule::interface_fn,
      convert_interface_fn,
      r#"  PostRemoteRes postRemote(
    ConversationID conversationID,
    MessageBoxed messageBoxed,
    array<gregor1.UID> atMentions,
    ChannelMention channelMention,
    union { null, TopicNameState } topicNameState,
    // Add any atMentions to the conversation automatically with the given
    // status
    union { null, ConversationMemberStatus } joinMentionsAs
  );"#,
      r#""#,
    )
    .unwrap();

    test_conversion(
      Rule::interface_fn,
      convert_interface_fn,
      r#"  void chatAttachmentDownloadProgress(int sessionID, long bytesComplete, long bytesTotal) oneway;"#,
      r#""#,
    )
    .unwrap();
  }

  #[test]
  fn test_typedef() {
    test_conversion(
      Rule::typedef,
      convert_typedef,
      r#"@typedef("bytes")  record ThreadID {}"#,
      // TODO fix this when we support bytes
      // "pub type ThreadID = Vec<u8>;",
      "pub type ThreadID = String;",
    )
    .unwrap();
  }

  #[test]
  fn test_enum() {
    test_conversion(
      Rule::enum_ty,
      convert_enum,
      r#"enum RetentionPolicyType {
  NONE_0,
  RETAIN_1, // Keep messages forever
  EXPIRE_2, // Delete after a while
  INHERIT_3, // Use the team's policy
  EPHEMERAL_4 // Force all messages to be exploding.
}"#,
      &format!(
        "#[derive({})]\n#[repr(u8)]\npub enum RetentionPolicyType {{
  None_0,
  Retain_1, // Keep messages forever
  Expire_2, // Delete after a while
  Inherit_3, // Use the team's policy
  Ephemeral_4, // Force all messages to be exploding.
}}",
        DERIVES_WITH_HASH_ENUM
      ),
    )
    .unwrap();
  }

  #[test]
  fn test_record() {
    set_add_derive(true);
    test_conversion(
      Rule::record,
      convert_record,
      r#"record InboxVersInfo {
  gregor1.UID uid;
  gregor1.UID type;
  @optional(true)
  InboxVers vers;
}"#,
      &format!(
        r#"#[derive({})]
pub struct InboxVersInfo {{
  pub uid: Option<gregor1::UID>,
  #[serde(rename = "type")]
  pub ty: Option<gregor1::UID>,
  pub vers: Option<InboxVers>,
}}"#,
        format!("{}", DERIVES)
      ),
    )
    .unwrap();

    test_conversion(
      Rule::record,
      convert_record,
      r#"record InboxVersInfo {
    @mpackkey("b") @jsonkey("b")
    union { null, gregor1.UID } botUID;
    @mpackkey("c") @jsonkey("c")
    InboxVers vers;
}"#,
      &format!(
        r#"#[derive({})]
pub struct InboxVersInfo {{
  // @mpackkey("b")
  #[serde(rename = "b")]
  pub botUID: Option<gregor1::UID>,
  // @mpackkey("c")
  #[serde(rename = "c")]
  pub vers: Option<InboxVers>,
}}"#,
        format!("{}", DERIVES)
      ),
    )
    .unwrap();

    set_derive_hash_for_ident(vec![String::from("ConvSummary")]);
    test_conversion(
      Rule::record,
      convert_record,
      r#"record ConvSummary {
  @optional(true)
  array<string> supersedes;
}"#,
      &format!(
        "#[derive({})]
pub struct ConvSummary {{
  #[serde(default)]
  pub supersedes: Option<Vec<String>>,
}}",
        format!("{}, Hash, PartialEq, Eq", DERIVES)
      ),
    )
    .unwrap();
    set_derive_hash_for_ident(vec![]);

    test_conversion(
      Rule::record,
      convert_record,
      r#"record ConvSummary {
  array<string> supersedes;
}"#,
      &format!(
        "#[derive({})]
pub struct ConvSummary {{
  #[serde(default)]
  pub supersedes: Option<Vec<String>>,
}}",
        format!("{}", DERIVES)
      ),
    )
    .unwrap();

    test_conversion(
      Rule::record,
      convert_record,
      r#"record ConvSummary {
  @optional(true)
  array<f32> supersedes;
}"#,
      &format!(
        "#[derive({})]
pub struct ConvSummary {{
  #[serde(default)]
  pub supersedes: Option<Vec<f32>>,
}}",
        format!("{}", DERIVES)
      ),
    )
    .unwrap();
  }

  #[test]
  fn test_fixed() {
    test_conversion(
      Rule::fixed_ty,
      convert_fixed,
      r#"fixed Bytes32(32);"#,
      // "pub type Bytes32 = [u8;32];",
      "pub type Bytes32 = Vec<u8>;",
    )
    .unwrap();
  }

  #[test]
  fn test_variant() {
    set_add_derive(true);
    test_conversion(
      Rule::variant_ty,
      convert_variant,
      r#"variant AssetMetadata switch (AssetMetadataType assetType) {
  case IMAGE: AssetMetadataImage;
  case VIDEO: AssetMetadataVideo;
  case AUDIO: AssetMetadataAudio;
}"#,
      &format!(
        "#[derive({})]\n#[serde(untagged)]\npub enum AssetMetadata {{
  Image {{image: AssetMetadataImage}},
  Video {{video: AssetMetadataVideo}},
  Audio {{audio: AssetMetadataAudio}},
}}",
        DERIVES
      ),
    )
    .unwrap();
    test_conversion(
      Rule::variant_ty,
      convert_variant,
      r#"variant AssetMetadata switch (AssetMetadataType assetType) {
    case IMAGE: AssetMetadataImage;
    case MOVE: Thing;
    default: void; // Note, if badged, we should urge an upgrade here.
}"#,
      &format!(
        "#[derive({})]\n#[serde(untagged)]\npub enum AssetMetadata {{
  Image {{image: AssetMetadataImage}},
  Move {{r#move: Thing}},
  Default {{}},
}}",
        DERIVES
      ),
    )
    .unwrap();
  }

  #[test]
  fn test_import_bug() {
    assert_eq!(
      "super::super::keybase1;",
      convert_path_str_to_rust_mod("../keybase1", "keybase1")
    );
  }
}
