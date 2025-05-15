#!/usr/bin/env -S python-venv --virtualenv dotfiles
import os

from argparse import ArgumentParser
import yaml

import jmullan_logging

def main():
    """Merge properties files"""
    parser = ArgumentParser()
    parser.add_argument(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_argument(
        "-c",
        "--crush",
        dest="crush",
        action="store_true",
        default=False,
        help="Crush into application.properties",
    )
    parser.add_argument(
        "--target", dest="target", default=None, help="crush into this file"
    )
    parser.add_argument("filenames", nargs="+")

    args = parser.parse_args()
    crush = args.crush
    target = args.target
    verbose = args.verbose
    values_by_file = {}
    defaults = {}
    for filename in args.filenames:
        values_by_file[filename] = {}
        if filename.endswith(".properties"):
            values_by_file[filename] = load_properties(filename)
        elif filename.endswith(".yml"):
            values_by_file[filename] = load_yaml(filename)
        for key, value in values_by_file[filename].items():
            defaults[key] = value
    changed_files = set()
    for filename, properties in values_by_file.items():
        for key, value in defaults.items():
            if key not in properties:
                properties[key] = value
                changed_files.add(filename)
    if crush:
        if target is None:
            target = "application.properties"
        crush_files(verbose, args.filenames, defaults, target)
    else:
        merge(verbose, changed_files, values_by_file)


def load_properties(filename: str) -> dict[str, str]:
    filesize = os.path.getsize(filename)
    with open(filename) as f:
        contents = f.read(filesize)
    values = {}
    for x in contents.split("\n"):
        x = x.strip()
        if not x.startswith("#") and len(x) > 0 and "=" in x:
            key, value = x.split("=", 1)
            values[key] = value
    return values


def load_yaml(filename: str) -> dict[str, str]:
    with open(filename) as f:
        data = yaml.safe_load(f)
    return flatten_nested(data)


def flatten_nested(data: dict, prefix: str | None = None) -> dict[str, str]:
    """Adds dots to all nested fields in dictionaries.

    Entries with different forms of nesting update.
    {"a": {"b": {"c": 4}} -> {"a.b.c": 4}
    {"a": {"b": 1}, "a.b": 2} -> {"a.b": 2}
    """
    output: dict[str, str] = {}
    if isinstance(data, dict):
       for k, v in data.items():
            if prefix is None or not len(prefix):
                key = f"{k}"
            else:
                key = f"{prefix}.{k}"
            subdata = flatten_nested(v, key)
            if subdata:
                output.update(subdata)
    else:
        if prefix is not None and len(prefix):
            output[prefix] = data
    return output


def get_new_contents(verbose, values):
    new_contents = ["%s=%s" % (key, value) for key, value in values.items()]
    new_contents = sorted(new_contents)

    new_contents = "\n".join(new_contents)
    if len(new_contents) > 0:
        new_contents = new_contents + "\n"
    return new_contents


def crush_files(verbose: bool, filenames: list[str], defaults: dict[str, str], target: str | None):
    application_filenames = set()
    application_filename: str | None = None
    dirs = set()
    if target:
        if os.path.isdir(target):
            print(f"{target} is a dir")
            application_filename = f"{target}/application.properties"
        elif os.path.exists(target):
            print(f"{target} is a file")
            application_filename = target
        else:
            dir = os.path.dirname(target)
            if os.path.isdir(dir):
                application_filename = target
    else:
        print(f"trying to find a dir in {filenames}")
        for filename in filenames:
            print(f"{filename=}")
            dir = os.path.dirname(filename)
            print(f"{dir=}")
            if not dir:
                dir = "./"
            dirs.add(dir)
            if filename in ["application.properties", "application.yml"]:
                application_filenames.add(filename)
                print(f"found {application_filenames=}")
                break
        if len(application_filenames) == 0:
            if len(dirs) != 1:
                print("Cannot pick a location for application.properties from ", dirs)
                exit(1)
            dir = list(dirs)[0]
            filename = f"{dir}/{target}"
            application_filenames.add(filename)
        print(application_filenames)
        if len(application_filenames) > 1:
            print("Too many application.properties files", application_filenames)
            exit(1)
        application_filename = list(application_filenames)[0]
    if application_filename is None:
        print("Cannot pick a location for application.properties from ", dirs)
        exit(1)

    print("Writing to ", application_filename)
    new_contents = get_new_contents(verbose, defaults)
    with open(application_filename, "w") as f:
        f.write(new_contents)
    for filename in filenames:
        if filename != application_filename:
            os.remove(filename)


def merge(verbose, changed_files, values_by_file):
    for filename in changed_files:
        new_contents = get_new_contents(verbose, values_by_file[filename])
        if verbose:
            print("updated file %s" % filename)
        with open(filename, "w") as f:
            f.write(new_contents)


if __name__ == "__main__":
    main()
