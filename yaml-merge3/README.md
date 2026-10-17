# yaml-merge3

A three-way merge tool for YAML files that understands their structure,
built on [yaml-edit](https://crates.io/crates/yaml-edit).

Entries in mappings, block or flow, are matched by key, so changes to
different keys merge cleanly even when they are on adjacent lines or in the
same `{...}`. Keys can be of any type, and repeated keys are matched up by
how often they occurred before. Tagged collections are merged the same way.

Items in sequences are matched against the common ancestor, so items
added, removed or changed on one side are applied to the other. When both
sides add items at the same place, all of them are kept, ours first, and an
item both added appears once. Items both sides changed are merged in turn,
so edits to different fields of the same list entry combine.

Scalars, such as multi-line strings, and comments fall back to a
line-by-line merge using [merge3](https://crates.io/crates/merge3).
Collections are never merged line by line: if their structure cannot be
merged, the result has a conflict.

Everything the merge does not need to touch is copied byte for byte from
the current version, including comments, quoting, indentation and regions
that fail to parse. Entries taken from the other version are reindented to
match the current one.

Conflicts are written with diff3-style markers around the conflicting
entry, as `git merge-file --diff3` would. A conflict within a flow
collection covers the whole lines it is on.

## Usage

The command line follows `git merge-file`:

```sh
yaml-merge3 [-p] [-q] [--marker-size N] [-L current -L base -L other] current.yaml base.yaml other.yaml
```

The result is written to `current.yaml` (or stdout with `-p`), and the
exit code is the number of conflicts.

## Using it as a git merge driver

```sh
git config merge.yaml.name "YAML merge"
git config merge.yaml.driver "yaml-merge3 --marker-size %L -L ours -L base -L theirs %A %O %B"
echo '*.yaml merge=yaml' >> .gitattributes
echo '*.yml merge=yaml' >> .gitattributes
```

## Limitations

- List items are matched by value, not by an identity key such as `name`,
  so moving an item while the other side changes it shows up as a
  conflict.
- Items both sides add at the same place are kept in the order ours, then
  theirs, which may not be right for lists where order matters.
- If both sides reorder the keys of a mapping differently, the whole
  mapping conflicts.
