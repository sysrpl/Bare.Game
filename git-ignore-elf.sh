#!/bin/sh
set -eu
cd "$(git rev-parse --show-toplevel)"
file=.gitignore
new=$file.new.$$
(
if [ -e "$file" ]; then
    cat "$file"
fi
find . -name .git -prune -o -type f ! -name '*.o' ! -name '*.so' \
    -print0 | xargs -0 file | grep ': *ELF ' | sed 's/:.*//' |
sed 's,^./,,'
) | perl -ne 'print if !$already{$_}++' >"$new"
mv "$new" "$file"
