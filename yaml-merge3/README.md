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

## Examples

Each example shows what ours and theirs changed relative to the common
ancestor, and what `yaml-merge3` makes of it. For all of them,
`git merge-file` reports at least one conflict.

### Changes on adjacent lines

Both sides edit the same service, on neighbouring lines, and both add to
the same list and mapping. A plain text merge reports three conflicts here.

Ours:

```diff
 services:
   web:
-    image: nginx:1.25
+    image: nginx:1.27
     ports:
       - "80:80"
+      - "443:443"
     environment:
       LOG_LEVEL: info
+      TLS: "on"
```

Theirs:

```diff
 services:
   web:
     image: nginx:1.25
+    restart: always
     ports:
       - "80:80"
+      - "8080:8080"
     environment:
-      LOG_LEVEL: info
+      LOG_LEVEL: debug
+      WORKERS: 4
```

Result, without conflicts:

```yaml
services:
  web:
    image: nginx:1.27
    restart: always
    ports:
      - "80:80"
      - "443:443"
      - "8080:8080"
    environment:
      LOG_LEVEL: debug
      TLS: "on"
      WORKERS: 4
```

Where both sides added something at the same place, ours comes first.

### Formatting and comments

Comments, blank lines, quoting style and flow collections are kept as they
are. Here theirs switches a value to double quotes and adds a comment above
an entry whose value ours changes, and both edit the same flow mapping.

Ours:

```diff
 # Deployment settings
 name: 'frontend'   # shown in the dashboard
-replicas: 2
+replicas: 3        # bumped for the launch

 # Resource limits, see RUNBOOK.md
-limits: {cpu: 500m, memory: 256Mi}
+limits: {cpu: 500m, memory: 512Mi}

 tags: [web, public]
```

Theirs:

```diff
 # Deployment settings
-name: 'frontend'   # shown in the dashboard
+name: "frontend-v2"   # shown in the dashboard
 replicas: 2

 # Resource limits, see RUNBOOK.md
-limits: {cpu: 500m, memory: 256Mi}
+# (raised after the outage on 3 March)
+limits: {cpu: 1000m, memory: 256Mi}

-tags: [web, public]
+tags: [web, public, beta]
```

Result, without conflicts:

```yaml
# Deployment settings
name: "frontend-v2"   # shown in the dashboard
replicas: 3        # bumped for the launch

# Resource limits, see RUNBOOK.md
# (raised after the outage on 3 March)
limits: {cpu: 1000m, memory: 512Mi}

tags: [web, public, beta]
```

### Lists of mappings

Edits to different fields of the same list entry combine.

Ours:

```diff
 containers:
   - name: app
-    image: example/app:1.0
+    image: example/app:1.1
     args: [--serve]
   - name: sidecar
     image: example/proxy:2.3
```

Theirs:

```diff
 containers:
   - name: app
     image: example/app:1.0
-    args: [--serve]
+    args: [--serve, --verbose]
   - name: sidecar
-    image: example/proxy:2.3
+    image: example/proxy:2.4
```

Result, without conflicts:

```yaml
containers:
  - name: app
    image: example/app:1.1
    args: [--serve, --verbose]
  - name: sidecar
    image: example/proxy:2.4
```

### JSON

JSON is YAML too, so changes to different keys on the same line merge.

Ours:

```diff
 {
   "name": "example",
-  "version": "1.0.0",
-  "dependencies": {"serde": "1.0", "rowan": "0.15"}
+  "version": "1.1.0",
+  "dependencies": {"serde": "1.0", "rowan": "0.16"}
 }
```

Theirs:

```diff
 {
   "name": "example",
   "version": "1.0.0",
-  "dependencies": {"serde": "1.0", "rowan": "0.15"}
+  "dependencies": {"serde": "1.0", "rowan": "0.15", "clap": "4"}
 }
```

Result, without conflicts:

```json
{
  "name": "example",
  "version": "1.1.0",
  "dependencies": {"serde": "1.0", "rowan": "0.16", "clap": "4"}
}
```

### Files that do not parse

Parts of the file that fail to parse only matter if both sides change
them. Here the unterminated list is left alone.

Ours:

```diff
-title: Notes
+title: Release notes
 author: someone
 draft: [unfinished, list
```

Theirs:

```diff
 title: Notes
-author: someone
+author: someone else
 draft: [unfinished, list
```

Result, without conflicts:

```yaml
title: Release notes
author: someone else
draft: [unfinished, list
```

### Genuine conflicts

When both sides change the same value differently, the conflict covers
just that entry, and everything else is still merged.

Ours:

```diff
 on: push
 jobs:
   test:
-    runs-on: ubuntu-22.04
+    runs-on: ubuntu-24.04
     timeout-minutes: 10
```

Theirs:

```diff
-on: push
+on: [push, pull_request]
 jobs:
   test:
-    runs-on: ubuntu-22.04
-    timeout-minutes: 10
+    runs-on: macos-14
+    timeout-minutes: 20
```

Result, with `-L ours -L base -L theirs`:

```yaml
on: [push, pull_request]
jobs:
  test:
<<<<<<< ours
    runs-on: ubuntu-24.04
||||||| base
    runs-on: ubuntu-22.04
=======
    runs-on: macos-14
>>>>>>> theirs
    timeout-minutes: 20
```

`yaml-merge3` exits with status 1 and reports where the conflict is:

```
conflict in jobs.test.runs-on
```

## Limitations

- List items are matched by value, not by an identity key such as `name`,
  so moving an item while the other side changes it shows up as a
  conflict.
- Items both sides add at the same place are kept in the order ours, then
  theirs, which may not be right for lists where order matters.
- If both sides reorder the keys of a mapping differently, the whole
  mapping conflicts.
