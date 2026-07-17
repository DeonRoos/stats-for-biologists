#!/usr/bin/env python3
"""
Strip Quarto's inlined MathJax "Polyfill service" <script> block from rendered
workshop HTML.

Why: Quarto's MathJax template bakes a large, self-contained polyfill shim
(for ancient browsers) into every rendered HTML. It is inlined and never
contacts cdn.polyfill.io, so it is harmless -- but it is dead weight and shares
a name with the compromised polyfill.io service. This script removes only that
<script> block, leaving the MathJax loader and everything else untouched.

The blob is re-emitted on EVERY `quarto render`, so run this after rendering.

Usage:
    python strip-polyfill.py                # scan ./ (run from docs/)
    python strip-polyfill.py path/to/dir    # scan a specific directory tree

Idempotent: files without the blob are left unchanged.
"""
import sys
import os

MARKER = "Polyfill service"
OPEN_TAG = "<script>"
CLOSE_TAG = "</script>"


def strip_file(path):
    with open(path, encoding="utf-8") as fh:
        text = fh.read()

    if MARKER not in text:
        return 0

    removed = 0
    while MARKER in text:
        idx = text.index(MARKER)
        start = text.rfind(OPEN_TAG, 0, idx)
        end = text.find(CLOSE_TAG, idx)
        if start == -1 or end == -1:
            print("  !! %s: found marker but no enclosing <script>...</script>; "
                  "skipping to avoid damage" % path)
            break
        end += len(CLOSE_TAG)
        # Also consume trailing newline left behind, if any.
        tail = end
        if tail < len(text) and text[tail] == "\n":
            tail += 1
        text = text[:start] + text[tail:]
        removed += 1

    if removed:
        with open(path, "w", encoding="utf-8") as fh:
            fh.write(text)
    return removed


def main():
    root = sys.argv[1] if len(sys.argv) > 1 else "."
    total_files = 0
    total_blocks = 0
    for dirpath, _, filenames in os.walk(root):
        for name in filenames:
            if not name.endswith(".html"):
                continue
            path = os.path.join(dirpath, name)
            n = strip_file(path)
            if n:
                total_files += 1
                total_blocks += n
                print("  stripped %d block(s): %s" % (n, path))
    print("Done: removed %d polyfill block(s) across %d file(s)."
          % (total_blocks, total_files))


if __name__ == "__main__":
    main()
