#!/usr/bin/env -S python-venv --virtualenv dotfiles
import re
import sys
from argparse import ArgumentParser


_ignores = [
    "* commit",
    "[Gradle Release Plugin] - new version commit:",
    "[Gradle Release Plugin] - pre tag commit",
    "Merge pull request #",
    "git-p4",
    "integrating changelist",
    "integrate changelist",
    "Integrate changelist",
    "Squashed commit",
    "Merge master",
    "merge to master",
]

_ignore_matches = [
    r"^ *This reverts commit *[a-z0-9]{40}\. *$",
    r"^ *commit *[a-z0-9]{40} *$",
    r"^ *Author: .*",
    r"^ *Date: .*",
    r"^ *Merge: [a-z0-9]+ [a-z0-9]+ *$",
    r"^ *Merge branch .* into .*",
    r"Merge branch '[^']+' of",
    r"^ *\.\.\. *$",
    r"^ *\.\.\. and [0-9]+ more commits *$",
]


def include_line(line):
    return (
        line.startswith(" ")
        and not any(x in line for x in _ignores)
        and not any(re.search(regex, line) for regex in _ignore_matches)
    )


def format_line(line):
    line = line.strip()
    line = re.sub(r"^(\** *)*", "", line)
    line = re.sub(r"^(-* *)*", "", line)
    return "* %s" % line


def make_notes(release_version, release_commits, commits):
    release_note = []
    version_string = "v %s" % release_version
    release_note.append(version_string)
    release_note.append("-" * len(version_string))
    release_notes = []
    for commit in release_commits[release_version]:
        for commit_line in commits[commit]:
            if commit_line not in release_notes:
                release_notes.append(commit_line)
    release_note.append("\n".join(release_notes))
    release_note.append("")
    return "\n".join(release_note)


def main():
    """Turn a pandora git release log into a changelog"""
    parser = ArgumentParser()
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_argument("version", default="Unknown")
    args = parser.parse_args()
    changes = []

    lines = [line.rstrip() for line in sys.stdin]

    release_commits = {}
    commits = {}
    commit = None
    release_version = None
    has_build_version = any(
        (re.search(r"pre tag commit.*'(.*)'", line) for line in lines)
    )
    if not has_build_version:
        if args.version:
            release_version = args.version
        else:
            release_version = "Unknown"
        release_commits[release_version] = []
    for line in lines:
        matches = re.search(r"pre tag commit.*'(.*)'", line)
        if matches:
            if release_version and release_commits[release_version]:
                notes = make_notes(release_version, release_commits, commits)
                changes.append(notes)
            release_version = matches.group(1)
            release_commits[release_version] = []
        elif line.startswith("commit "):
            commit = line.split(" ")[1]
            commits[commit] = []
            if release_version:
                release_commits[release_version].append(commit)
        else:
            if include_line(line):
                commit_line = format_line(line)
                commits[commit].append(commit_line)

    if release_commits[release_version]:
        changes.append(make_notes(release_version, release_commits, commits))

    if changes:
        # changes = reversed(changes)
        print("\n".join(changes))


if __name__ == "__main__":
    main()
