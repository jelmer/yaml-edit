//! Structure-aware three-way merge for YAML files.
//!
//! Mappings, block or flow, are merged entry by entry, matching keys across
//! the three versions, so that changes to different keys combine even when
//! they sit on adjacent lines. Sequences are merged item by item: items
//! added, removed or changed on one side are applied to the other, and
//! items both sides inserted at the same place are kept, ours first.
//! Scalars, comments and blank lines fall back to a line-by-line merge, as
//! a plain text merge would do. Collections are never merged line by line:
//! if their structure cannot be merged, the result has a conflict.
//!
//! The output is assembled from byte ranges of the inputs, so anything the
//! merge does not have to touch is copied verbatim from "ours". That
//! includes comments, quoting and regions that fail to parse.
//!
//! Conflicts are written with git-style diff3 markers around the smallest
//! set of whole lines that contains them.
//!
//! ```
//! let base = "name: app\nversion: 1\n";
//! let ours = "name: app\nversion: 2\n";
//! let theirs = "name: application\nversion: 1\n";
//! let result = yaml_merge3::merge(base, ours, theirs, &Default::default());
//! assert_eq!(result.text, "name: application\nversion: 2\n");
//! assert!(result.is_clean());
//! ```

use merge3::{Merge3, MergeRegion};
use std::hash::{Hash, Hasher};
use std::ops::Range;
use yaml_edit::{yaml_eq, yaml_hash, Document, TextPosition, YamlFile, YamlNode};

/// Options controlling how conflicts are written.
#[derive(Debug, Clone)]
pub struct MergeOptions {
    /// Label written after the `<<<<<<<` marker.
    pub ours_label: Option<String>,
    /// Label written after the `|||||||` marker.
    pub base_label: Option<String>,
    /// Label written after the `>>>>>>>` marker.
    pub theirs_label: Option<String>,
    /// Number of marker characters, as git's `conflict-marker-size`.
    pub marker_size: usize,
}

impl Default for MergeOptions {
    fn default() -> Self {
        Self {
            ours_label: None,
            base_label: None,
            theirs_label: None,
            marker_size: 7,
        }
    }
}

/// A conflict written into the merged text.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Conflict {
    /// Mapping keys and sequence indexes (as `[n]`, counted in ours)
    /// leading from the document root to the conflict. Empty when the
    /// conflict is outside any mergeable collection.
    pub path: Vec<String>,
}

/// The outcome of a merge.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MergeResult {
    /// The merged text, with conflict markers if there were conflicts.
    pub text: String,
    /// The conflicts that were written into `text`.
    pub conflicts: Vec<Conflict>,
}

impl MergeResult {
    /// Whether the merge completed without conflicts.
    pub fn is_clean(&self) -> bool {
        self.conflicts.is_empty()
    }
}

/// Merge the changes from `base` to `theirs` into `ours`.
pub fn merge(base: &str, ours: &str, theirs: &str, options: &MergeOptions) -> MergeResult {
    let mut merger = Merger::new(base, ours, theirs, options, Vec::new());
    let text = merger.merge_files();
    MergeResult {
        text,
        conflicts: merger.conflicts,
    }
}

struct Side<'a> {
    text: &'a str,
    file: YamlFile,
}

impl<'a> Side<'a> {
    fn new(text: &'a str) -> Self {
        // Parse errors are deliberately not fatal: the merge only needs the
        // structure of the regions both sides changed, and everything else is
        // copied as text.
        let file = YamlFile::parse(text).tree();
        Self { text, file }
    }

    fn slice(&self, range: Range<usize>) -> &'a str {
        &self.text[range]
    }

    fn line_start(&self, pos: usize) -> usize {
        self.text[..pos].rfind('\n').map_or(0, |i| i + 1)
    }

    /// The end of the line containing `pos`, after its newline. A position
    /// that already starts a line is returned unchanged.
    fn line_end(&self, pos: usize) -> usize {
        if pos == 0 || self.text.as_bytes()[pos - 1] == b'\n' {
            return pos;
        }
        self.text[pos..]
            .find('\n')
            .map_or(self.text.len(), |i| pos + i + 1)
    }
}

/// A value, compared with [`yaml_eq`] and hashed with [`yaml_hash`].
///
/// `yaml_eq` does not consider a value with aliases or unresolvable content
/// equal even to itself. Such values compare by their source text instead,
/// so that equality is reflexive as hash-based matching requires.
#[derive(Debug, Clone)]
struct Value {
    node: YamlNode,
    text: String,
    comparable: bool,
}

impl Value {
    fn new(side: &Side, node: YamlNode) -> Self {
        let text = side.slice(to_range(node.byte_range())).to_string();
        let comparable = yaml_eq(&node, &node);
        Self {
            node,
            text,
            comparable,
        }
    }

    fn is_collection(&self) -> bool {
        collection(&self.node).is_some()
    }
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self.comparable, other.comparable) {
            (true, true) => yaml_eq(&self.node, &other.node),
            (false, false) => self.text == other.text,
            _ => false,
        }
    }
}

impl Eq for Value {}

impl Hash for Value {
    fn hash<H: Hasher>(&self, state: &mut H) {
        if self.comparable {
            yaml_hash(&self.node, state);
        } else {
            self.text.hash(state);
        }
    }
}

/// A mapping entry or sequence item, with the value it is matched on.
trait HasValue {
    fn value(&self) -> &Option<Value>;

    /// The text of the entry or item itself, without trivia around it.
    fn span(&self) -> Range<usize>;
}

fn same_values<T: HasValue>(a: &[T], b: &[T]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.value() == b.value())
}

/// The mapping or sequence `node` is, looking through a tag on it.
fn collection(node: &YamlNode) -> Option<YamlNode> {
    match node {
        YamlNode::Mapping(_) | YamlNode::Sequence(_) => Some(node.clone()),
        YamlNode::TaggedNode(t) => t
            .as_mapping()
            .map(YamlNode::Mapping)
            .or_else(|| t.as_sequence().map(YamlNode::Sequence)),
        _ => None,
    }
}

fn is_flow(node: &YamlNode) -> bool {
    match node {
        YamlNode::Mapping(m) => m.is_flow_style(),
        YamlNode::Sequence(s) => s.is_flow_style(),
        _ => false,
    }
}

/// The root node of a document.
fn root_node(doc: &Document) -> Option<YamlNode> {
    doc.as_mapping()
        .map(YamlNode::Mapping)
        .or_else(|| doc.as_sequence().map(YamlNode::Sequence))
        .or_else(|| doc.as_tagged().map(YamlNode::TaggedNode))
}

/// One block mapping entry or sequence item, together with the comments
/// and blank lines leading up to it. All ranges cover whole lines.
struct Segment {
    value: Option<Value>,
    /// Trivia lines between the previous segment and this one.
    lead: Range<usize>,
    /// The entry or item itself.
    body: Range<usize>,
}

impl Segment {
    fn full(&self) -> Range<usize> {
        self.lead.start..self.body.end
    }

    fn has_collection(&self) -> bool {
        self.value.as_ref().is_some_and(Value::is_collection)
    }
}

impl HasValue for Segment {
    fn value(&self) -> &Option<Value> {
        &self.value
    }

    fn span(&self) -> Range<usize> {
        self.body.clone()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Kind {
    Mapping,
    Sequence,
}

/// A block collection split into line-aligned segments.
struct Region {
    kind: Kind,
    range: Range<usize>,
    /// Column at which the keys or dashes start.
    indent: usize,
    /// The key of each segment, for mappings.
    keys: Vec<Value>,
    segments: Vec<Segment>,
    /// Trivia after the last segment.
    tail: Range<usize>,
}

fn to_range(pos: TextPosition) -> Range<usize> {
    pos.start as usize..pos.end as usize
}

fn is_blank_or_comment(s: &str) -> bool {
    let s = s.trim();
    s.is_empty() || s.starts_with('#')
}

/// Split a block mapping or sequence into segments, or `None` if it cannot
/// be merged segment by segment.
fn region(side: &Side, node: &YamlNode) -> Option<Region> {
    let node = collection(node)?;
    if is_flow(&node) {
        return None;
    }
    let mut keys = Vec::new();
    let mut spans: Vec<(Range<usize>, Option<YamlNode>)> = Vec::new();
    let (kind, range) = match &node {
        YamlNode::Mapping(m) => {
            for entry in m.entries() {
                // Leave entries without a key in the trivia, so that a broken
                // entry only matters if someone changes it.
                let Some(key) = entry.key_node() else {
                    continue;
                };
                keys.push(Value::new(side, key));
                spans.push((to_range(entry.byte_range()), entry.value_node()));
            }
            (Kind::Mapping, to_range(m.byte_range()))
        }
        YamlNode::Sequence(s) => {
            spans.extend(s.values().map(|v| (to_range(v.byte_range()), Some(v))));
            (Kind::Sequence, to_range(s.byte_range()))
        }
        _ => return None,
    };
    let start = side.line_start(range.start);
    if !side.text[start..range.start].trim().is_empty() {
        return None;
    }
    let end = side.line_end(range.end);
    if !is_blank_or_comment(&side.text[range.end..end]) {
        return None;
    }

    let mut segments = Vec::new();
    let mut cursor = start;
    let mut indent = None;
    for (range, value) in spans {
        let body_start = side.line_start(range.start);
        if body_start < cursor {
            return None;
        }
        let line = side.slice(body_start..range.start);
        let content = line.trim_start_matches(' ');
        // An item whose dash is on an earlier line than its value would
        // leave the dash in the trivia.
        if kind == Kind::Sequence && !content.starts_with('-') {
            return None;
        }
        indent.get_or_insert(line.len() - content.len());
        let body_end = side.line_end(range.end);
        segments.push(Segment {
            value: value.map(|v| Value::new(side, v)),
            lead: cursor..body_start,
            body: body_start..body_end,
        });
        cursor = body_end;
    }
    if cursor > end {
        return None;
    }
    Some(Region {
        kind,
        range: start..end,
        indent: indent?,
        keys,
        segments,
        tail: cursor..end,
    })
}

/// The block collection held by a segment's value, if it fills the rest of
/// the segment so that it can be merged segment by segment.
fn value_region(side: &Side, segment: &Segment) -> Option<Region> {
    let region = region(side, &segment.value.as_ref()?.node)?;
    (region.range.start > segment.body.start && region.range.end == segment.body.end)
        .then_some(region)
}

/// One entry or item of a flow collection. For entries, the range runs
/// from the start of the key to the end of the value.
struct FlowItem {
    value: Option<Value>,
    range: Range<usize>,
}

impl HasValue for FlowItem {
    fn value(&self) -> &Option<Value> {
        &self.value
    }

    fn span(&self) -> Range<usize> {
        self.range.clone()
    }
}

/// A flow collection split into its brackets, items and separators.
struct FlowRegion {
    kind: Kind,
    range: Range<usize>,
    /// The text the collection is merged as part of: the whole lines that
    /// hold it, or the flow item it is the value of.
    within: Range<usize>,
    /// The keys of the items, for mappings.
    keys: Vec<Value>,
    items: Vec<FlowItem>,
    /// The opening bracket and any trivia up to the first item.
    open: Range<usize>,
    /// The text between each item and the next, including the comma.
    seps: Vec<Range<usize>>,
    /// Any trivia and trailing comma after the last item, and the closing
    /// bracket.
    close: Range<usize>,
}

/// The number of commas in `s`, if it holds nothing but those, whitespace
/// and comments.
fn flow_trivia_commas(s: &str) -> Option<usize> {
    let mut commas = 0;
    let mut in_comment = false;
    for c in s.chars() {
        match c {
            '\n' => in_comment = false,
            _ if in_comment => {}
            '#' => in_comment = true,
            ',' => commas += 1,
            c if c.is_whitespace() => {}
            _ => return None,
        }
    }
    Some(commas)
}

/// Split a flow mapping or sequence into items, or `None` if it cannot be
/// merged item by item. `within` is the text it is merged as part of,
/// which must contain it.
fn flow_region(side: &Side, node: &YamlNode, within: Range<usize>) -> Option<FlowRegion> {
    let node = collection(node)?;
    if !is_flow(&node) {
        return None;
    }
    let mut keys = Vec::new();
    let mut items = Vec::new();
    let (kind, range) = match &node {
        YamlNode::Mapping(m) => {
            for entry in m.entries() {
                let key = entry.key_node()?;
                let key_range = to_range(key.byte_range());
                let value = entry.value_node();
                let end = value
                    .as_ref()
                    .map_or(key_range.end, |v| to_range(v.byte_range()).end)
                    .max(key_range.end);
                keys.push(Value::new(side, key));
                items.push(FlowItem {
                    value: value.map(|v| Value::new(side, v)),
                    range: key_range.start..end,
                });
            }
            (Kind::Mapping, to_range(m.byte_range()))
        }
        YamlNode::Sequence(s) => {
            for value in s.values() {
                items.push(FlowItem {
                    range: to_range(value.byte_range()),
                    value: Some(Value::new(side, value)),
                });
            }
            (Kind::Sequence, to_range(s.byte_range()))
        }
        _ => return None,
    };
    if range.len() < 2 {
        return None;
    }

    // Anchors and other properties sit outside the item ranges; insist that
    // everything between items is plain separators so none of that text can
    // end up attached to the wrong item.
    let inner = range.start + 1..range.end - 1;
    let open = range.start..items.first().map_or(inner.start, |i| i.range.start);
    let close = items.last().map_or(inner.start, |i| i.range.end)..range.end;
    let seps: Vec<Range<usize>> = items
        .windows(2)
        .map(|w| w[0].range.end..w[1].range.start)
        .collect();
    let valid = within.start <= range.start
        && range.end <= within.end
        && open.end >= inner.start
        && close.start <= inner.end
        && flow_trivia_commas(side.slice(inner.start..open.end)) == Some(0)
        && flow_trivia_commas(side.slice(close.start..inner.end)).is_some_and(|c| c <= 1)
        && seps
            .iter()
            .all(|s| s.start <= s.end && flow_trivia_commas(side.slice(s.clone())) == Some(1));
    valid.then_some(FlowRegion {
        kind,
        range,
        within,
        keys,
        items,
        open,
        seps,
        close,
    })
}

/// The flow collection held by a segment's value.
fn value_flow_region(side: &Side, segment: &Segment) -> Option<FlowRegion> {
    flow_region(side, &segment.value.as_ref()?.node, segment.body.clone())
}

/// The flow collection held by a flow item's value.
fn item_flow_region(side: &Side, item: &FlowItem) -> Option<FlowRegion> {
    flow_region(side, &item.value.as_ref()?.node, item.range.clone())
}

/// Shift each non-blank line of `text` from column `from` to column `to`.
fn reindent(text: &str, from: usize, to: usize) -> String {
    if from == to {
        return text.to_string();
    }
    let mut out = String::with_capacity(text.len());
    for line in text.split_inclusive('\n') {
        if line.trim().is_empty() {
            out.push_str(line);
        } else if to > from {
            out.push_str(&" ".repeat(to - from));
            out.push_str(line);
        } else {
            let leading = line.len() - line.trim_start_matches(' ').len();
            out.push_str(&line[leading.min(from - to)..]);
        }
    }
    out
}

/// Concatenate whole-line pieces, terminating any that lack a newline
/// unless it is the last one and `terminate_last` is false.
fn join(pieces: Vec<String>, terminate_last: bool) -> String {
    let pieces: Vec<String> = pieces.into_iter().filter(|p| !p.is_empty()).collect();
    let n = pieces.len();
    let mut out = String::new();
    for (i, piece) in pieces.into_iter().enumerate() {
        out.push_str(&piece);
        if (i + 1 < n || terminate_last) && !out.ends_with('\n') {
            out.push('\n');
        }
    }
    out
}

/// Which side's version of a unit of text to keep, if that does not need a
/// conflict.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Pick {
    Ours,
    Theirs,
}

fn pick(base: Option<&str>, ours: &str, theirs: &str) -> Option<Pick> {
    if ours == theirs || base == Some(theirs) {
        Some(Pick::Ours)
    } else if base == Some(ours) {
        Some(Pick::Theirs)
    } else {
        None
    }
}

/// The range covering a run of adjacent segments.
fn span(segments: &[Segment]) -> Range<usize> {
    match (segments.first(), segments.last()) {
        (Some(first), Some(last)) => first.lead.start..last.body.end,
        _ => 0..0,
    }
}

/// A key, told apart from earlier entries with an equal key by its
/// occurrence, and its index in each version of a mapping.
struct Slot {
    key: Value,
    occurrence: usize,
    base: Option<usize>,
    ours: Option<usize>,
    theirs: Option<usize>,
}

type SlotIndex = fn(&Slot) -> Option<usize>;

fn match_keys(base: Option<&[Value]>, ours: &[Value], theirs: &[Value]) -> Vec<Slot> {
    let mut slots: Vec<Slot> = Vec::new();
    let mut add = |keys: &[Value], set: fn(&mut Slot, usize)| {
        for (i, key) in keys.iter().enumerate() {
            let occurrence = keys[..i].iter().filter(|k| *k == key).count();
            match slots
                .iter_mut()
                .find(|s| s.key == *key && s.occurrence == occurrence)
            {
                Some(slot) => set(slot, i),
                None => {
                    let mut slot = Slot {
                        key: key.clone(),
                        occurrence,
                        base: None,
                        ours: None,
                        theirs: None,
                    };
                    set(&mut slot, i);
                    slots.push(slot);
                }
            }
        }
    };
    if let Some(base) = base {
        add(base, |s, i| s.base = Some(i));
    }
    add(ours, |s, i| s.ours = Some(i));
    add(theirs, |s, i| s.theirs = Some(i));
    slots
}

/// The order in which `side` has the keys that all three versions share.
fn common_order(slots: &[Slot], side: SlotIndex) -> Vec<usize> {
    let mut common: Vec<(usize, usize)> = slots
        .iter()
        .enumerate()
        .filter(|(_, s)| s.base.is_some() && s.ours.is_some() && s.theirs.is_some())
        .filter_map(|(k, s)| Some((side(s)?, k)))
        .collect();
    common.sort_unstable();
    common.into_iter().map(|(_, k)| k).collect()
}

/// The order to write the keys of a mapping in, or `None` if both sides
/// reordered them differently and there is no single order to pick.
///
/// Theirs' order is followed if only they reordered the keys all three
/// versions share.
fn key_order(slots: &[Slot]) -> Option<Vec<usize>> {
    let order_b = common_order(slots, |s| s.base);
    let order_o = common_order(slots, |s| s.ours);
    let order_t = common_order(slots, |s| s.theirs);
    if order_o != order_b && order_t != order_b && order_o != order_t {
        return None;
    }
    Some(if order_t != order_b {
        output_order(slots, |s| s.theirs, |s| s.ours, false)
    } else {
        output_order(slots, |s| s.ours, |s| s.theirs, true)
    })
}

/// `primary`'s order, with keys only `secondary` has placed after the
/// closest key preceding them in `secondary` that `primary` also has, or
/// failing that before the closest following one. Keys that only one side
/// has and that end up between the same two shared keys come out with
/// ours' first, as items both sides add to a sequence do.
fn output_order(
    slots: &[Slot],
    primary: SlotIndex,
    secondary: SlotIndex,
    primary_is_ours: bool,
) -> Vec<usize> {
    let mut primary_keys: Vec<usize> = (0..slots.len())
        .filter(|&k| primary(&slots[k]).is_some())
        .collect();
    primary_keys.sort_by_key(|&k| primary(&slots[k]));
    let mut secondary_only: Vec<usize> = (0..slots.len())
        .filter(|&k| primary(&slots[k]).is_none() && secondary(&slots[k]).is_some())
        .collect();
    secondary_only.sort_by_key(|&k| secondary(&slots[k]));
    let shared: Vec<usize> = primary_keys
        .iter()
        .copied()
        .filter(|&a| secondary(&slots[a]).is_some())
        .collect();

    // The keys only primary has and the keys only secondary has, in each
    // gap before, between and after the shared keys.
    let mut gaps: Vec<(Vec<usize>, Vec<usize>)> = vec![Default::default(); shared.len() + 1];
    let mut gap = 0;
    for &k in &primary_keys {
        if secondary(&slots[k]).is_some() {
            gap += 1;
        } else {
            gaps[gap].0.push(k);
        }
    }
    for k in secondary_only {
        let pos = secondary(&slots[k]);
        let preceding = (0..shared.len())
            .filter(|&i| secondary(&slots[shared[i]]) < pos)
            .max_by_key(|&i| secondary(&slots[shared[i]]));
        let following = (0..shared.len())
            .filter(|&i| secondary(&slots[shared[i]]) > pos)
            .min_by_key(|&i| secondary(&slots[shared[i]]));
        let gap = match (preceding, following) {
            (Some(i), _) => i + 1,
            (None, Some(i)) => i,
            (None, None) => shared.len(),
        };
        gaps[gap].1.push(k);
    }

    let mut order = Vec::new();
    for (i, (primary_only, secondary_only)) in gaps.into_iter().enumerate() {
        let (first, second) = if primary_is_ours {
            (primary_only, secondary_only)
        } else {
            (secondary_only, primary_only)
        };
        order.extend(first);
        order.extend(second);
        order.extend(shared.get(i));
    }
    order
}

/// The runs of items that all three sequences share, from merge3, as
/// `(base, ours, theirs, length)`. The last one is empty and sits at the
/// end of all three.
fn sync_regions<T: HasValue>(b: &[T], o: &[T], t: &[T]) -> Vec<(usize, usize, usize, usize)> {
    fn values<T: HasValue>(items: &[T]) -> Vec<&Option<Value>> {
        items.iter().map(HasValue::value).collect()
    }
    let (bv, ov, tv) = (values(b), values(o), values(t));
    Merge3::new(&bv, &ov, &tv)
        .find_sync_regions()
        .into_iter()
        .map(|(zb, ze, ob, _, tb, _)| (zb, ob, tb, ze - zb))
        .collect()
}

/// How to reindent text taken from theirs: from the column of the
/// collection it came from to that of the collection in ours it goes into.
#[derive(Clone, Copy)]
struct Shift {
    from: usize,
    to: usize,
}

impl Shift {
    const NONE: Shift = Shift { from: 0, to: 0 };

    fn between(theirs: &Region, ours: &Region) -> Self {
        Self {
            from: theirs.indent,
            to: ours.indent,
        }
    }
}

/// The root collection of a document.
enum Root {
    Block(Region),
    Flow(FlowRegion),
}

impl Root {
    fn new(side: &Side, doc: &Document) -> Option<Self> {
        let node = root_node(doc)?;
        if let Some(region) = region(side, &node) {
            return Some(Root::Block(region));
        }
        let range = to_range(collection(&node)?.byte_range());
        let lines = side.line_start(range.start)..side.line_end(range.end);
        flow_region(side, &node, lines).map(Root::Flow)
    }

    /// The whole lines the collection takes up.
    fn lines(&self) -> Range<usize> {
        match self {
            Root::Block(region) => region.range.clone(),
            Root::Flow(flow) => flow.within.clone(),
        }
    }

    fn kind(&self) -> (bool, Kind) {
        match self {
            Root::Block(region) => (false, region.kind),
            Root::Flow(flow) => (true, flow.kind),
        }
    }
}

struct Merger<'a> {
    base: Side<'a>,
    ours: Side<'a>,
    theirs: Side<'a>,
    options: &'a MergeOptions,
    conflicts: Vec<Conflict>,
    path: Vec<String>,
}

impl<'a> Merger<'a> {
    fn new(
        base: &'a str,
        ours: &'a str,
        theirs: &'a str,
        options: &'a MergeOptions,
        path: Vec<String>,
    ) -> Self {
        Self {
            base: Side::new(base),
            ours: Side::new(ours),
            theirs: Side::new(theirs),
            options,
            conflicts: Vec::new(),
            path,
        }
    }

    fn theirs_text(&self, range: Range<usize>, shift: Shift) -> String {
        reindent(self.theirs.slice(range), shift.from, shift.to)
    }

    /// Pick a version of the given ranges without needing a conflict.
    fn choose(
        &self,
        b: Option<Range<usize>>,
        o: Range<usize>,
        t: Range<usize>,
        shift: Shift,
    ) -> Option<String> {
        let ours = self.ours.slice(o);
        match pick(
            b.map(|b| self.base.slice(b)),
            ours,
            self.theirs.slice(t.clone()),
        )? {
            Pick::Ours => Some(ours.to_string()),
            Pick::Theirs => Some(self.theirs_text(t, shift)),
        }
    }

    fn conflict(&mut self, base: Option<&str>, ours: &str, theirs: &str) -> String {
        self.conflicts.push(Conflict {
            path: self.path.clone(),
        });
        let marker = |c: char, label: Option<&String>| {
            let mut line = c.to_string().repeat(self.options.marker_size);
            if let Some(label) = label {
                line.push(' ');
                line.push_str(label);
            }
            line.push('\n');
            line
        };
        let section = |text: &str| join(vec![text.to_string()], true);
        let mut out = marker('<', self.options.ours_label.as_ref());
        out.push_str(&section(ours));
        out.push_str(&marker('|', self.options.base_label.as_ref()));
        out.push_str(&section(base.unwrap_or("")));
        out.push_str(&marker('=', None));
        out.push_str(&section(theirs));
        out.push_str(&marker('>', self.options.theirs_label.as_ref()));
        out
    }

    /// Merge the given whole-line ranges as a unit. If both sides changed
    /// them differently, merge them line by line when `by_line` is set,
    /// and otherwise write a conflict.
    fn merge_ranges(
        &mut self,
        b: Option<Range<usize>>,
        o: Range<usize>,
        t: Range<usize>,
        shift: Shift,
        by_line: bool,
    ) -> String {
        if let Some(text) = self.choose(b.clone(), o.clone(), t.clone(), shift) {
            return text;
        }
        let base = b.map(|b| self.base.slice(b));
        let ours = self.ours.slice(o);
        let theirs = self.theirs_text(t, shift);
        if by_line {
            self.merge_lines(base, ours, &theirs)
        } else {
            self.conflict(base, ours, &theirs)
        }
    }

    /// Merge whole lines of text line by line, as a plain text merge
    /// would. Only used for scalars and trivia, never for collections.
    fn merge_lines(&mut self, base: Option<&str>, ours: &str, theirs: &str) -> String {
        fn lines(text: &str) -> Vec<&str> {
            text.split_inclusive('\n').collect()
        }
        let (bl, ol, tl) = (lines(base.unwrap_or("")), lines(ours), lines(theirs));
        let regions = Merge3::new(&bl, &ol, &tl).merge_regions();

        let mut pieces = Vec::new();
        for region in regions {
            let piece = match region {
                MergeRegion::Unchanged { start, end } => bl[start..end].concat(),
                MergeRegion::Same { astart, aend } => ol[astart..aend].concat(),
                MergeRegion::A { start, end } => ol[start..end].concat(),
                MergeRegion::B { start, end } => tl[start..end].concat(),
                MergeRegion::Conflict {
                    zstart,
                    zend,
                    astart,
                    aend,
                    bstart,
                    bend,
                } => {
                    let base = zstart.zip(zend).map(|(s, e)| bl[s..e].concat());
                    self.conflict(
                        base.as_deref(),
                        &ol[astart..aend].concat(),
                        &tl[bstart..bend].concat(),
                    )
                }
            };
            pieces.push(piece);
        }
        join(pieces, ours.ends_with('\n'))
    }

    fn merge_files(&mut self) -> String {
        let docs = |side: &Side| side.file.documents().collect::<Vec<_>>();
        let (bd, od, td) = (docs(&self.base), docs(&self.ours), docs(&self.theirs));

        // Documents whose root collection can be merged structurally in all
        // three versions. The text around them is merged as a unit.
        let mut anchors = Vec::new();
        let mut unanchored = Vec::new();
        if bd.len() == od.len() && od.len() == td.len() {
            for ((b, o), t) in bd.iter().zip(&od).zip(&td) {
                let roots = (
                    Root::new(&self.base, b),
                    Root::new(&self.ours, o),
                    Root::new(&self.theirs, t),
                );
                match roots {
                    (Some(b), Some(o), Some(t)) if b.kind() == o.kind() && o.kind() == t.kind() => {
                        anchors.push((b, o, t))
                    }
                    _ => unanchored.extend([b, o, t]),
                }
            }
        } else {
            unanchored.extend(bd.iter().chain(&od).chain(&td));
        }
        // Text around the anchors is only merged line by line if it holds
        // no collections.
        let by_line = !unanchored
            .iter()
            .filter_map(|doc| root_node(doc))
            .any(|node| collection(&node).is_some());

        let mut pieces = Vec::new();
        let mut cursors = (0, 0, 0);
        for (b, o, t) in &anchors {
            let (bl, ol, tl) = (b.lines(), o.lines(), t.lines());
            pieces.push(self.merge_ranges(
                Some(cursors.0..bl.start),
                cursors.1..ol.start,
                cursors.2..tl.start,
                Shift::NONE,
                by_line,
            ));
            pieces.push(match (b, o, t) {
                (Root::Block(b), Root::Block(o), Root::Block(t)) => {
                    self.merge_region(Some(b), o, t)
                }
                (Root::Flow(b), Root::Flow(o), Root::Flow(t)) => {
                    let merged = self.merge_flow_in(Some(b), o, t, Shift::NONE);
                    merged.unwrap_or_else(|| {
                        self.merge_ranges(
                            Some(bl.clone()),
                            ol.clone(),
                            tl.clone(),
                            Shift::NONE,
                            false,
                        )
                    })
                }
                _ => unreachable!("anchors have the same kind of root"),
            });
            cursors = (bl.end, ol.end, tl.end);
        }
        pieces.push(self.merge_ranges(
            Some(cursors.0..self.base.text.len()),
            cursors.1..self.ours.text.len(),
            cursors.2..self.theirs.text.len(),
            Shift::NONE,
            by_line,
        ));
        join(pieces, self.ours.text.ends_with('\n'))
    }

    /// Merge three block collections of the same kind.
    fn merge_region(&mut self, b: Option<&Region>, o: &Region, t: &Region) -> String {
        match o.kind {
            Kind::Mapping => self.merge_mapping(b, o, t),
            Kind::Sequence => self.merge_sequence(b, o, t),
        }
    }

    fn merge_mapping(&mut self, b: Option<&Region>, o: &Region, t: &Region) -> String {
        let shift = Shift::between(t, o);
        let slots = match_keys(b.map(|b| &b.keys[..]), &o.keys, &t.keys);
        let Some(order) = key_order(&slots) else {
            // Not merged line by line, which could duplicate keys.
            let base = b.map(|b| self.base.slice(b.range.clone()));
            let theirs = self.theirs_text(t.range.clone(), shift);
            return self.conflict(base, self.ours.slice(o.range.clone()), &theirs);
        };

        // Keys that only base has were deleted on both sides, and are not in
        // `order`.
        let mut pieces = Vec::new();
        for k in order {
            let slot = &slots[k];
            self.path.push(slot.key.text.trim().to_string());
            let piece = self.merge_slot(
                b.zip(slot.base).map(|(r, i)| &r.segments[i]),
                slot.ours.map(|i| &o.segments[i]),
                slot.theirs.map(|i| &t.segments[i]),
                shift,
            );
            self.path.pop();
            pieces.push(piece);
        }
        pieces.push(self.merge_ranges(
            b.map(|b| b.tail.clone()),
            o.tail.clone(),
            t.tail.clone(),
            shift,
            true,
        ));
        join(pieces, self.ours.slice(o.range.clone()).ends_with('\n'))
    }

    fn merge_slot(
        &mut self,
        b: Option<&Segment>,
        o: Option<&Segment>,
        t: Option<&Segment>,
        shift: Shift,
    ) -> String {
        match (b, o, t) {
            (b, Some(o), Some(t)) => self.merge_entry(b, o, t, shift, Kind::Mapping),
            (None, Some(o), None) => self.ours.slice(o.full()).to_string(),
            (None, None, Some(t)) => self.theirs_text(t.full(), shift),
            // Deleted by them.
            (Some(b), Some(o), None) => {
                let (base, ours) = (self.base.slice(b.full()), self.ours.slice(o.full()));
                if ours == base || o.value == b.value {
                    String::new()
                } else {
                    self.conflict(Some(base), ours, "")
                }
            }
            // Deleted by us.
            (Some(b), None, Some(t)) => {
                let base = self.base.slice(b.full());
                if self.theirs.slice(t.full()) == base || t.value == b.value {
                    String::new()
                } else {
                    let theirs = self.theirs_text(t.full(), shift);
                    self.conflict(Some(base), "", &theirs)
                }
            }
            (_, None, None) => String::new(),
        }
    }

    /// Merge two block sequences against their base, diff3 style: runs of
    /// items that all three versions share split the sequences into chunks,
    /// which are merged separately.
    fn merge_sequence(&mut self, b: Option<&Region>, o: &Region, t: &Region) -> String {
        let shift = Shift::between(t, o);
        let bs = b.map_or(&[][..], |b| &b.segments[..]);
        let (os, ts) = (&o.segments[..], &t.segments[..]);

        let mut pieces = Vec::new();
        let (mut ib, mut io, mut it) = (0, 0, 0);
        for (zb, ob, tb, len) in sync_regions(bs, os, ts) {
            pieces.push(self.merge_chunk(&bs[ib..zb], &os[io..ob], &ts[it..tb], io, shift));
            for k in 0..len {
                self.path.push(format!("[{}]", ob + k));
                let piece = self.merge_entry(
                    Some(&bs[zb + k]),
                    &os[ob + k],
                    &ts[tb + k],
                    shift,
                    Kind::Sequence,
                );
                pieces.push(piece);
                self.path.pop();
            }
            (ib, io, it) = (zb + len, ob + len, tb + len);
        }
        pieces.push(self.merge_ranges(
            b.map(|b| b.tail.clone()),
            o.tail.clone(),
            t.tail.clone(),
            shift,
            true,
        ));
        join(pieces, self.ours.slice(o.range.clone()).ends_with('\n'))
    }

    /// Merge a run of block sequence items that differ between the
    /// versions. `first` is the index in ours of the first item of `o`.
    fn merge_chunk(
        &mut self,
        b: &[Segment],
        o: &[Segment],
        t: &[Segment],
        first: usize,
        shift: Shift,
    ) -> String {
        // Items changed in place, by one side or both. Merged one by one,
        // so that changes to comments or spelling are kept as well.
        if o.len() == b.len() && t.len() == b.len() {
            let mut pieces = Vec::new();
            for (i, ((b, o), t)) in b.iter().zip(o).zip(t).enumerate() {
                self.path.push(format!("[{}]", first + i));
                pieces.push(self.merge_entry(Some(b), o, t, shift, Kind::Sequence));
                self.path.pop();
            }
            return join(pieces, false);
        }

        let ours = self.ours.slice(span(o));
        if same_values(t, b) || same_values(o, t) {
            return ours.to_string();
        }
        if same_values(o, b) {
            return self.theirs_text(span(t), shift);
        }

        // Both inserted items at the same place. Keep ours first, then
        // whatever theirs added that ours did not.
        if b.is_empty() {
            let mut pieces = vec![ours.to_string()];
            for item in t {
                if !o.iter().any(|i| i.value == item.value) {
                    pieces.push(self.theirs_text(item.full(), shift));
                }
            }
            return join(pieces, false);
        }

        // Not merged line by line, which could interleave parts of
        // different items.
        let base = self.base.slice(span(b));
        let theirs = self.theirs_text(span(t), shift);
        self.conflict(Some(base), ours, &theirs)
    }

    /// Merge a block mapping entry or sequence item that both sides have.
    fn merge_entry(
        &mut self,
        b: Option<&Segment>,
        o: &Segment,
        t: &Segment,
        shift: Shift,
        kind: Kind,
    ) -> String {
        if let Some(text) = self.choose(b.map(Segment::full), o.full(), t.full(), shift) {
            return text;
        }

        // The comments above the entry and the entry itself are merged
        // separately, so that a new comment on one side does not clash with
        // a changed value on the other.
        let lead = self.merge_ranges(
            b.map(|b| b.lead.clone()),
            o.lead.clone(),
            t.lead.clone(),
            shift,
            true,
        );
        let body = match self.choose(
            b.map(|b| b.body.clone()),
            o.body.clone(),
            t.body.clone(),
            shift,
        ) {
            Some(body) => body,
            None => self.merge_values(b, o, t, shift, kind),
        };
        lead + &body
    }

    /// Merge the body of an entry or item that both sides changed.
    fn merge_values(
        &mut self,
        b: Option<&Segment>,
        o: &Segment,
        t: &Segment,
        shift: Shift,
        kind: Kind,
    ) -> String {
        if let Some(text) = self.merge_parts(b, o, t, shift) {
            return text;
        }
        // Both made the same change, perhaps spelled differently.
        if o.value == t.value {
            return self.ours.slice(o.body.clone()).to_string();
        }
        if let Some(text) = self.merge_nested(b, o, t, shift, kind) {
            return text;
        }
        let has_collection =
            o.has_collection() || t.has_collection() || b.is_some_and(Segment::has_collection);
        self.merge_ranges(
            b.map(|b| b.body.clone()),
            o.body.clone(),
            t.body.clone(),
            shift,
            !has_collection,
        )
    }

    /// Merge the collections held by an entry or item that both sides
    /// changed, if they are of the same kind and style. A missing base means
    /// the entry was added on both sides.
    fn merge_nested(
        &mut self,
        b: Option<&Segment>,
        o: &Segment,
        t: &Segment,
        shift: Shift,
        kind: Kind,
    ) -> Option<String> {
        let flows = (
            value_flow_region(&self.ours, o),
            value_flow_region(&self.theirs, t),
        );
        if let (Some(of), Some(tf)) = flows {
            let bf = match b {
                Some(b) => Some(value_flow_region(&self.base, b)?),
                None => None,
            };
            return self.merge_flow_in(bf.as_ref(), &of, &tf, shift);
        }
        match kind {
            Kind::Mapping => self.merge_entry_value(b, o, t, shift),
            Kind::Sequence => self.merge_item_value(b, o, t),
        }
    }

    /// Merge a mapping entry whose value is a block collection.
    fn merge_entry_value(
        &mut self,
        b: Option<&Segment>,
        o: &Segment,
        t: &Segment,
        shift: Shift,
    ) -> Option<String> {
        let bv = match b {
            Some(b) => Some(value_region(&self.base, b)?),
            None => None,
        };
        let ov = value_region(&self.ours, o)?;
        let tv = value_region(&self.theirs, t)?;
        if ov.kind != tv.kind || bv.as_ref().is_some_and(|bv| bv.kind != ov.kind) {
            return None;
        }
        // The key line and any trivia ahead of the nested collection. It
        // holds no collection itself, so it can be merged line by line.
        let header = self.merge_ranges(
            b.zip(bv.as_ref())
                .map(|(b, bv)| b.body.start..bv.range.start),
            o.body.start..ov.range.start,
            t.body.start..tv.range.start,
            shift,
            true,
        );
        Some(header + &self.merge_region(bv.as_ref(), &ov, &tv))
    }

    /// Merge a sequence item whose value is a block collection.
    ///
    /// The collection starts on the line of the item's dash, which the
    /// segments of a region cannot express. Instead, the dash (and anything
    /// else ahead of the value on its line) is blanked out, the resulting
    /// texts are merged as documents of their own, and ours' dash is put
    /// back.
    fn merge_item_value(
        &mut self,
        b: Option<&Segment>,
        o: &Segment,
        t: &Segment,
    ) -> Option<String> {
        let normalize = |side: &Side, seg: &Segment| {
            let value = to_range(seg.value.as_ref()?.node.byte_range());
            let prefix = side.slice(seg.body.start..value.start);
            let blanked = " ".repeat(prefix.chars().count());
            Some((
                prefix.to_string(),
                blanked + side.slice(value.start..seg.body.end),
            ))
        };
        let (_, base) = normalize(&self.base, b?)?;
        let (ours_prefix, ours) = normalize(&self.ours, o)?;
        let (theirs_prefix, theirs) = normalize(&self.theirs, t)?;
        if ours_prefix.trim_start() != theirs_prefix.trim_start() {
            return None;
        }
        let mut sub = Merger::new(&base, &ours, &theirs, self.options, self.path.clone());
        let merged = sub.merge_files();
        // If the merge could not keep the first line in place, there is no
        // line left to put the dash on.
        let rest = merged.strip_prefix(&" ".repeat(ours_prefix.chars().count()))?;
        self.conflicts.append(&mut sub.conflicts);
        Some(format!("{ours_prefix}{rest}"))
    }

    /// Merge three flow collections, together with the rest of the text
    /// they are merged as part of. Markers cannot go within a line, so
    /// rather than writing a conflict this returns `None` when there would
    /// be one, for the caller to write a conflict around the whole lines.
    fn merge_flow_in(
        &self,
        b: Option<&FlowRegion>,
        o: &FlowRegion,
        t: &FlowRegion,
        shift: Shift,
    ) -> Option<String> {
        let before = |f: &FlowRegion| f.within.start..f.range.start;
        let after = |f: &FlowRegion| f.range.end..f.within.end;
        let prefix = self.choose(b.map(before), before(o), before(t), shift)?;
        let suffix = self.choose(b.map(after), after(o), after(t), shift)?;
        let merged = self.merge_flow(b, o, t)?;
        Some(prefix + &merged + &suffix)
    }

    /// Merge three flow collections, or `None` if that needs a conflict.
    fn merge_flow(&self, b: Option<&FlowRegion>, o: &FlowRegion, t: &FlowRegion) -> Option<String> {
        if o.kind != t.kind || b.is_some_and(|b| b.kind != o.kind) {
            return None;
        }
        let open = self.choose(
            b.map(|b| b.open.clone()),
            o.open.clone(),
            t.open.clone(),
            Shift::NONE,
        )?;
        let close = self.choose(
            b.map(|b| b.close.clone()),
            o.close.clone(),
            t.close.clone(),
            Shift::NONE,
        )?;
        // Each merged item, with its index in ours if it is still where ours
        // had it, so that ours' separator can be kept.
        let items = match o.kind {
            Kind::Mapping => self.flow_mapping_items(b, o, t)?,
            Kind::Sequence => self.flow_sequence_items(b, o, t)?,
        };

        let default_sep = o
            .seps
            .first()
            .map(|r| self.ours.slice(r.clone()))
            .or_else(|| t.seps.first().map(|r| self.theirs.slice(r.clone())))
            .unwrap_or(", ");
        let mut out = open;
        for (i, (index, text)) in items.iter().enumerate() {
            if i > 0 {
                let sep = match (items[i - 1].0, index) {
                    (Some(prev), Some(index)) if *index == prev + 1 => {
                        self.ours.slice(o.seps[prev].clone())
                    }
                    _ => default_sep,
                };
                out.push_str(sep);
            }
            out.push_str(text);
        }
        out.push_str(&close);
        Some(out)
    }

    fn flow_mapping_items(
        &self,
        b: Option<&FlowRegion>,
        o: &FlowRegion,
        t: &FlowRegion,
    ) -> Option<Vec<(Option<usize>, String)>> {
        let slots = match_keys(b.map(|b| &b.keys[..]), &o.keys, &t.keys);
        let mut items = Vec::new();
        for k in key_order(&slots)? {
            let slot = &slots[k];
            let bi = b.zip(slot.base).map(|(b, i)| &b.items[i]);
            let oi = slot.ours.map(|i| &o.items[i]);
            let ti = slot.theirs.map(|i| &t.items[i]);
            match (bi, oi, ti) {
                (bi, Some(oi), Some(ti)) => {
                    items.push((slot.ours, self.merge_flow_item(bi, oi, ti)?))
                }
                (None, Some(oi), None) => {
                    items.push((slot.ours, self.ours.slice(oi.range.clone()).to_string()))
                }
                (None, None, Some(ti)) => {
                    items.push((None, self.theirs.slice(ti.range.clone()).to_string()))
                }
                // Deleted by one side, so the other must not have changed it.
                (Some(bi), Some(kept), None) => {
                    if !self.flow_item_unchanged(bi, kept, &self.ours) {
                        return None;
                    }
                }
                (Some(bi), None, Some(kept)) => {
                    if !self.flow_item_unchanged(bi, kept, &self.theirs) {
                        return None;
                    }
                }
                (_, None, None) => {}
            }
        }
        Some(items)
    }

    fn flow_item_unchanged(&self, base: &FlowItem, item: &FlowItem, side: &Side) -> bool {
        self.base.slice(base.range.clone()) == side.slice(item.range.clone())
            || base.value == item.value
    }

    fn flow_sequence_items(
        &self,
        b: Option<&FlowRegion>,
        o: &FlowRegion,
        t: &FlowRegion,
    ) -> Option<Vec<(Option<usize>, String)>> {
        let bs = b.map_or(&[][..], |b| &b.items[..]);
        let (os, ts) = (&o.items[..], &t.items[..]);
        let ours = |i: usize| (Some(i), self.ours.slice(os[i].range.clone()).to_string());
        let theirs = |item: &FlowItem| (None, self.theirs.slice(item.range.clone()).to_string());

        let mut items = Vec::new();
        let (mut ib, mut io, mut it) = (0, 0, 0);
        for (zb, ob, tb, len) in sync_regions(bs, os, ts) {
            let (bc, oc, tc) = (&bs[ib..zb], &os[io..ob], &ts[it..tb]);
            // As for block sequences, items changed in place are merged one
            // by one, so that changes to their spelling are kept.
            if oc.len() == bc.len() && tc.len() == bc.len() {
                for k in 0..bc.len() {
                    items.push((
                        Some(io + k),
                        self.merge_flow_item(Some(&bc[k]), &oc[k], &tc[k])?,
                    ));
                }
            } else if same_values(tc, bc) || same_values(oc, tc) {
                items.extend((io..ob).map(ours));
            } else if same_values(oc, bc) {
                items.extend(tc.iter().map(theirs));
            } else if bc.is_empty() {
                items.extend((io..ob).map(ours));
                items.extend(
                    tc.iter()
                        .filter(|i| !oc.iter().any(|o| o.value == i.value))
                        .map(theirs),
                );
            } else {
                return None;
            }
            for k in 0..len {
                items.push((
                    Some(ob + k),
                    self.merge_flow_item(Some(&bs[zb + k]), &os[ob + k], &ts[tb + k])?,
                ));
            }
            (ib, io, it) = (zb + len, ob + len, tb + len);
        }
        Some(items)
    }

    /// Merge a flow entry or item that both sides have.
    fn merge_flow_item(&self, b: Option<&FlowItem>, o: &FlowItem, t: &FlowItem) -> Option<String> {
        let ranges = (b.map(|b| b.range.clone()), o.range.clone(), t.range.clone());
        if let Some(text) = self.choose(ranges.0, ranges.1, ranges.2, Shift::NONE) {
            return Some(text);
        }
        if let Some(text) = self.merge_parts(b, o, t, Shift::NONE) {
            return Some(text);
        }
        if o.value == t.value {
            return Some(self.ours.slice(o.range.clone()).to_string());
        }
        let bf = match b {
            Some(b) => Some(item_flow_region(&self.base, b)?),
            None => None,
        };
        let of = item_flow_region(&self.ours, o)?;
        let tf = item_flow_region(&self.theirs, t)?;
        self.merge_flow_in(bf.as_ref(), &of, &tf, Shift::NONE)
    }

    /// Merge a one-line entry or item whose value is a scalar in three
    /// parts: the text ahead of the value, such as the key; the value; and
    /// the text after it, such as a comment. This lets changes to different
    /// parts of the same line combine. `None` if that needs a conflict.
    fn merge_parts<T: HasValue>(
        &self,
        b: Option<&T>,
        o: &T,
        t: &T,
        shift: Shift,
    ) -> Option<String> {
        fn parts<'v, T: HasValue>(
            side: &Side,
            item: &'v T,
        ) -> Option<(&'v Value, [Range<usize>; 3])> {
            let value = item.value().as_ref()?;
            let (span, v) = (item.span(), to_range(value.node.byte_range()));
            let one_line = !side
                .slice(span.clone())
                .trim_end_matches('\n')
                .contains('\n');
            let within = span.start <= v.start && v.end <= span.end;
            (one_line && within && !value.is_collection())
                .then(|| (value, [span.start..v.start, v.clone(), v.end..span.end]))
        }
        let b = match b {
            Some(b) => Some(parts(&self.base, b)?),
            None => None,
        };
        let (ov, o) = parts(&self.ours, o)?;
        let (tv, t) = parts(&self.theirs, t)?;
        let part = |i: usize| {
            (
                b.as_ref().map(|(_, b)| b[i].clone()),
                o[i].clone(),
                t[i].clone(),
            )
        };

        // The text ahead of the value starts the line, so it is the only
        // part that needs reindenting.
        let (bp, op, tp) = part(0);
        let prefix = self.choose(bp, op, tp, shift)?;
        let (bs, os, ts) = part(2);
        let suffix = self.choose(bs, os, ts, Shift::NONE)?;
        let (bv, ov_range, tv_range) = part(1);
        let base_value = b.as_ref().map(|(v, _)| *v);
        let value = match self.choose(bv, ov_range.clone(), tv_range.clone(), Shift::NONE) {
            Some(value) => value,
            // One side only respelled the value, so the other side's change
            // wins; or both made the same change, spelled differently.
            None if base_value == Some(ov) => self.theirs.slice(tv_range).to_string(),
            None if base_value == Some(tv) || ov == tv => self.ours.slice(ov_range).to_string(),
            None => return None,
        };
        Some(prefix + &value + &suffix)
    }
}
