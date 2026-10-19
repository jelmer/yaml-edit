# Rowan quirks

Known problems in rowan 0.16 that affect yaml-edit, and how the crate
avoids them. None of these have been reported upstream yet.

## `splice_children` removes only one child

`SyntaxNode::splice_children` detaches the range while iterating over it,
so it stops after the first element and leaves the rest of the range in
place.

Never call `splice_children` directly to replace or remove more than one
child. Use `nodes::replace_children`, `insert_children` or
`append_children`, which detach the range one child at a time. They also
rewrite inserted line breaks to the document's own line break.

## `splice_children` walks every child

Even for a pure insert, `splice_children` walks all of the parent's
children, creating a cursor for each, to find the ones to detach, and then
rebuilds the parent's green node. Each edit is therefore linear in the
number of children, and building a long sequence or mapping one entry at a
time is quadratic: nearly all of `Sequence::push`'s cost is spent there.
There is no cheaper insertion in rowan's public API, so the crate accepts
this. Build large collections with the builders, or parse the text in one
go, which is linear.

## Token navigation stops at a node without tokens

`prev_token`, `next_token`, `first_token` and `last_token` return `None`
when the adjacent sibling, or the first or last child, is a node holding no
tokens. Code that walks tokens, such as `column::token_column`, then sees
the start or end of the tree too early.

The parser never builds such nodes: an empty value or scalar holds a
zero-width token (`NULL ""`), and `debug::validate_tree` checks that every
SCALAR holds a token and every KEY, VALUE and SEQUENCE_ENTRY holds a node.
Builders must do the same.

## Offsets in mutable trees cost the node's depth

A mutable tree (`SyntaxNode::new_root_mut`) works out each node's offset by
walking up to the root. This is by design, but it makes a traversal of a
deep tree quadratic. Passes that only read a whole tree, such as the
validator, `YamlValue` conversion and `debug::validate_tree`, run on
`nodes::immutable_copy` of it instead.

## `NodeCache` rehashes whole subtrees

When the green node cache grows, it rehashes each entry with a hash that
walks the whole subtree, and not the hash it uses for lookups. Parsing
deeply nested input (`[` repeated) is therefore about quadratic in the
depth, and deduplication can miss after a resize, which only costs memory
sharing. The parser's nesting limit (`MAX_FLOW_DEPTH`, 256) bounds the
cost.

## Dropping a deep tree recurses

Dropping the last reference to a green tree recurses once per level, so a
deep tree can overflow a small thread stack (seen with 64 to 128 KiB stacks
in debug builds at depth 255). The nesting limit bounds the depth; tests
that run on small stacks keep a reference to the tree outside the thread.
