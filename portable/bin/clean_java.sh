#!/bin/bash
PROCS=4
# TICKET="CONTIN-6952 : "
echo "dos2unix"
ag '\r' -l --java | xargs dos2unix
git commit -am "${TICKET}Applied dos2unix"

echo fix_c0303.py
find . -name "*.java" -not -path '*/\.git/*' -print0 | xargs -0 -L 1 -P "${PROCS}" fix_c0303.py
git commit -am "${TICKET}Remove trailing whitespace"

echo fix_mixed_tabs_and_spaces.py
ag -l '\t | \t' --java | xargs -L 1 -P "${PROCS}" fix_mixed_tabs_and_spaces.py
git commit -am "${TICKET}Fixed mixed tabs and spaces"

echo move_copyright_notice.py
find . -name "*.java" -not -path '*/\.git/*' -print0 | xargs -0 -L 1 -P "${PROCS}" move_copyright_notice.py
git commit -am "${TICKET}Moved copyright notices"

echo clean_java_comments.py
find . -name "*.java" -not -path '*/\.git/*' -print0 | xargs -0 -L 1 -P "${PROCS}" clean_java_comments.py
git commit -am "${TICKET}Cleaned up java comments"

echo clean_java_many_lines.py
find . -name "*.java" -not -path '*/\.git/*' -print0 | xargs -0 -L 1 -P "${PROCS}" clean_java_many_lines.py
git commit -am "${TICKET}Cleaned up excess blank lines"

echo fix_w391.py
find . -name "*.java" -not -path '*/\.git/*' -print0 | xargs -0 -L 1 -P "${PROCS}" fix_w391.py
git commit -am "${TICKET}All files should end with a newline"

echo clean_java_trailing_brace.py
find . -name "*.java" -not -path '*/\.git/*' -print0 | xargs -0 -L 1 -P "${PROCS}" clean_java_trailing_brace.py
git commit -am "${TICKET}Snug up final curly braces"

echo google-java-format --fix-imports-only
find . -name "*.java" -not -path '*/\.git/*' -print0 | xargs -0 -L 1 -P "${PROCS}" google-java-format --fix-imports-only
git commit -am "${TICKET}Google format all imports"
