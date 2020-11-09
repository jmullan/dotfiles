#!/bin/bash
PROCS=4
echo move_copyright_notice
find . -name "*.java" -not -path '*/\.git/*' | xargs -L 1 -P "${PROCS}" move_copyright_notice.py
echo clean_java_comments
find . -name "*.java" -not -path '*/\.git/*' | xargs -L 1 -P "${PROCS}" clean_java_comments.py
echo clean_java_many_lines
find . -name "*.java" -not -path '*/\.git/*' | xargs -L 1 -P "${PROCS}" clean_java_many_lines.py
echo fix_w391
find . -name "*.java" -not -path '*/\.git/*' | xargs -L 1 -P "${PROCS}" fix_w391.py
echo fix_c0303
find . -name "*.java" -not -path '*/\.git/*' | xargs -L 1 -P "${PROCS}" fix_c0303.py
echo clean_java_trailing_brace
find . -name "*.java" -not -path '*/\.git/*' | xargs -L 1 -P "${PROCS}" clean_java_trailing_brace.py
echo google-java-format
find . -name "*.java" -not -path '*/\.git/*' | xargs -L 1 -P "${PROCS}" google-java-format --fix-imports-only
