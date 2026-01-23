#!/bin/bash
find . -name "*.java" -not -path '*/\.git/*' -not -path '*/build/*' -print0
