# AVDL Rust Code Generator (With serde support)

A library to convert avdl declarations into Rust types with serde support.

Turns this:

```avdl
record Thread {
  @jsonkey("messages")
  array<Message> messages;
  @jsonkey("pagination")
  union { null, Pagination } pagination;
  @jsonkey("offline")
  boolean offline;
  @jsonkey("identify_failures")
  array<keybase1.TLFIdentifyFailure> identifyFailures;
  @jsonkey("ratelimits")
  array<RateLimitRes> rateLimits;
}
```

into this:

```rust
#[derive(Serialize, Deserialize, Debug)]
pub struct Thread {
  #[serde(rename = "messages")]
  #[serde(default)]
  pub messages: Option<Vec<Message>>,
  #[serde(rename = "pagination")]
  pub pagination: Option<Pagination>,
  #[serde(rename = "offline")]
  #[serde(default)]
  pub offline: Option<bool>,
  #[serde(rename = "identify_failures")]
  #[serde(default)]
  pub identifyFailures: Option<Vec<keybase1::TLFIdentifyFailure>>,
  #[serde(rename = "ratelimits")]
  #[serde(default)]
  pub rateLimits: Option<Vec<RateLimitRes>>,
}
```

## Why?

This is mostly to export Keybase's types defined in their [protocol](https://github.com/MarcoPolo/keybase-protocol) for use in Rust.

## Usage

look at `to_rust::build_rust_code_from_avdl`.
