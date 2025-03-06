<div align="center">
<h1>
<picture>
  <source media="(prefers-color-scheme: dark)" srcset="ideahelix_dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="ideahelix_light.svg">
  <img alt="IdeaHelix" height="128" src="ideahelix_light.svg">
</picture>
</h1>
</div>

[JetBrains IDEs'](https://www.jetbrains.com/ides/) plugin for a
[Helix](https://helix-editor.com)-like experience.

🚧 _Work in progress! See issues for more details._ 🚧

[LSP](https://github.com/microsoft/language-server-protocol) is not a silver bullet, and
there are still some projects and workflows that are better served by more heavyweight
solutions, with JB IDEs arguably being some of the best in that regard. Unfortunately,
this often forces us to leave an environment like Helix, which feels more like a right
hand than just an editor. The goal of this project is to bridge the gap between the
tooling provided by JetBrains and the editing model of Helix.

## Goals
- Helix users and their muscle memory should feel at home with no need to re-learn
  everything.

## Non-Goals
- **1:1 Emulation** – Achieving a perfect, one-to-one emulation of Helix is not the
  goal, at least for the foreseeable future. Reuse is preferred, and differences in
  caret implementations may lead to edge cases.  If you're looking for the complete Helix
  experience, there is Helix.
- **Plugin Best Practices Compliance** – Stability isn't guaranteed, as the plugin
  interrupts keyboard events with the author's limited knowledge of JetBrains IDEs and
  it likely doesn't follow plugin development best practices. Reaching ~150k lines while
  pursuing those, as IdeaVim does, is something to avoid. Contributions are welcome, as
  basic compatibility or UX improvements are always possible without overdoing it.

## Emulating Helix Carets

In Helix, carets are visually represented as blocks, but these blocks are not
just indicators -- they are actual one-character selections. This means the visual
representation directly corresponds to the underlying behavior: replacing or deleting a
character removes it from the document and places it into a register, just like a yank
operation. This leads to the first rule:

**Selections are always at least one character long, with a line caret positioned either
before the first character or before the last.**

To illustrate this, let's introduce a legend:

- `|` -- Line caret (default in Idea and many other non-TUI editors)
- `|x|` -- Block caret highlighting character `x` (as in Helix)
- `║` -- Selection boundaries

Here’s how selecting the word `hello` appears in Helix compared to IdeaHelix, with spaces
preserved to reflect caret positioning accurately:

```
Forward-facing selection:
Hx : ║h e l l|o|
IHx: ║h e l l|o║

Backward-facing selection:
Hx : |h|e l l o║
IHx: |h e l l o║
```

**A caret can only be in one of two positions: at the start of the selection or just
before the last selected character.**

### Edge Case: Empty Buffer

If the buffer is empty, no selection exists—this is the only exception to the first rule.
As soon as text is inserted, the standard behavior applies.

### Insertion mode

In Helix, a caret is effectively a one-character selection outside of insert mode and
behaves like a block caret within it. IdeaHelix ensures text is inserted at the same
position as in Helix but uses Idea’s native visual representation -- a line caret with
selections turned off.

Insert mode in IdeaHelix bypasses its own handlers, temporarily handing control back
to Idea. This simplifies insert mode behavior, allowing Idea’s features to work without
interference. As a bonus, this also enables the use of native keystroke configurations
within insert mode.

In Helix, selections remain active and may expand when entering insert mode via prepend
or append. However, due to Idea’s limitations, IdeaHelix must disable selections while
typing. This ensures seamless integration with Idea’s completion system, which relies
on the selection start rather than the actual caret position. Once insert mode ends,
selections are restored to match how they would appear in Helix.

```
Append:
Hx : ║h e l l o| |w o r l d
IHx:  h e l l o|  w o r l d

Prepend:
Hx : |h|e l l o║  w o r l d
IHx: |h e l l o   w o r l d
```

### Exiting Insertion Mode

When leaving insertion mode, selections reappear as follows:

```
Exiting append:
Hx : ║h e l l|o|  w o r l d
IHx: ║h e l l|o║  w o r l d

Exiting prepend (no change in either case):
Hx : |h|e l l o║  w o r l d
IHx: |h e l l o║  w o r l d
```

## Acknowledgments
- [Kakoune](https://kakoune.org) – for ruining my life by making every other editing
  style unbearable.
- [Iosevka](https://typeof.net/Iosevka/) – for being the font of choice, both in the
  logo and for daily use (licensed under the
  [SIL Open Font License](https://opensource.org/licenses/OFL-1.1)).
