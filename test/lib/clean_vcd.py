#!/usr/bin/env python3

import sys

from collections import deque

if __name__ == "__main__":
    pending = deque()
    replaced = {}
    dumping = False

    for line in sys.stdin:
        # Find and drop dumped parameters.
        if line.startswith("$var "):
            parts = line.split()
            should_drop = parts[1] == "parameter"
            if not should_drop and not pending:
                print(line, end="")
                continue
            ident_old = parts[3]
            pending.append(ident_old)
            if should_drop:
                replaced[ident_old] = None
                continue
            ident_new = pending.popleft()
            parts[3] = ident_new
            replaced[ident_old] = ident_new
            print(" ".join(parts))

        # Pass through lines if we have no transformations to do.
        elif line.startswith("$dump"):
            dumping = True
            print(line, end="")
        elif line.startswith("$end"):
            # dumping = False
            print(line, end="")
        elif not dumping or not replaced:
            print(line, end="")
        elif line[0] == "#":
            print(line, end="")

        # Rename dumped variables.
        elif line.startswith("b"):
            value, ident = line.split()
            ident = replaced.get(ident, ident)
            if ident is not None:
                print(value, ident)
        elif line[0] in "01xz":
            value = line[0]
            ident = line[1:-1]
            ident = replaced.get(ident, ident)
            if ident is not None:
                print(value + ident)

        else:
            raise RuntimeError(f"Unhandled: {line.strip()}")
