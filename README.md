# zigparser
wip parser thing in zig

## interface goal

```zig
Parser(.{
  .expression = .{"a", ._, "=", ._, "b"},
  ._ = regex("\\s*"), // probably won't use regex
}, struct {

  /// would be a bit nicer if it could get the function return
  /// value, but you can't do that with generic functions (yet?)
  pub const expression_RV = AstNode;
  pub fn expression(items: var, pos: range) expression_RV {
    items[3]; // fully typed, items[3] is void, items[0] is
    // the default for things without handlers
    // (struct{value: []const u8, range: Range})
    return .defaultNode;
  }

  pub const __RV = void;
  pub fn _(match: var, pos: Range) __RV {
    return {};
  }
  
}).parse(
  .expression,
  \\a = b
  , arena,
);
```

## goals

- fully typed
- nice error messages (`lockin()` and `errmsg()`)
  - after lockin, errors must bubble up completely,
    above ors won't continue
- custom defaults? `.{"var", ._, default(.identifier)}`
- (not resonable in zig 0.6.0) named outputs?
  ```zig
  .{"var", ._, name(.varname, .identifier), ._, "=", ._, name(.expr, .expression), ._, ";"}
  ```
- fun to use

## notes

doesn't use a tokenizer. I'm not sure what the point of
tokenizers is, they are probably useful because everyone
uses one, but I'm not sure how they help and they only
seem to make parsing more complicated.

eg how do you tokenize a template string? parsing is easy
without a tokenizer:

```zig
.templateString = .{"`", star(.char), "`"},
.char = .{.{"\\", lockin, "(", ._, .expression, ._, ")"}, @"or", regex("[^`\\\\]")}
```
