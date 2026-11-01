# CAD In-Place Block Editor

Small AutoLISP utilities for editing or clearing attributes in a selected block.

## Commands

- `EATT` — click near an attribute grip to edit its value, or choose `Clear-box` to clear attributes inside a rectangle.
- `CLATT` — clear attributes inside a rectangle without opening the edit prompt.

Both commands accept a preselected block or prompt you to select one. The rectangle commands clear an attribute when its grip, insertion point, or alignment point falls inside the rectangle.

## Load and use

1. Open the LISP file in your CAD application with `APPLOAD`.
2. Select a block and run `EATT` or `CLATT` at the command line.
3. Test on a copy of your drawing first; these commands modify attribute values in the drawing.

The code is intended for AutoLISP-compatible CAD applications. Compatibility can vary by application and version.

## Contents

This repository contains only `Lisp/EditInPlace.lsp`. Project drawings, other scripts, and local logs are intentionally excluded.

## License

MIT. See [LICENSE](LICENSE).
