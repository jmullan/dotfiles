#!/usr/bin/env python-venv dotfiles
import os
import re

from optparse import OptionParser


def main():
    """Merge properties files"""
    changed = False
    parser = OptionParser()
    parser.add_option(
        "-v",
        "--verbose",
        dest="verbose",
        action="store_true",
        default=False,
        help="verbose is more verbose",
    )
    parser.add_option(
        "-c",
        "--crush",
        dest="crush",
        action="store_true",
        default=False,
        help="Crush into application.properties",
    )
    parser.add_option(
        "--target", dest="target", default=None, help="crush into this file"
    )
    (options, args) = parser.parse_args()
    options = options.__dict__
    verbose = options.get("verbose")
    crush = options.get("crush")
    target = options.get("target")

    values_by_file = {}
    defaults = {}
    for filename in args:
        values_by_file[filename] = {}
        contents = ""
        filesize = os.path.getsize(filename)
        with open(filename) as f:
            contents = f.read(filesize)
        for x in contents.split("\n"):
            x = x.strip()
            if not x.startswith("#") and len(x) > 0 and "=" in x:
                key, value = x.split("=", 1)
                values_by_file[filename][key] = value
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
        crush_files(verbose, args, defaults, target)
    else:
        merge(verbose, changed_files, values_by_file)


def get_new_contents(verbose, values):
    new_contents = ["%s=%s" % (key, value) for key, value in values.items()]
    new_contents = sorted(new_contents)

    new_contents = "\n".join(new_contents)
    if len(new_contents) > 0:
        new_contents = new_contents + "\n"
    return new_contents


def crush_files(verbose, filenames, defaults, target):
    application_filenames = set()
    dirs = set()
    for filename in filenames:
        dir = os.path.dirname(filename)
        if not dir:
            dir = "./"
        dirs.add(dir)
        if filename == "application.properties":
            application_filenames.add(filename)
            break
    if len(application_filenames) == 0:
        if len(dirs) != 1:
            print("Cannot pick a location for application.properties from ", dirs)
            exit(1)
        dir = list(dirs)[0]
        filename = f"{dir}/{target}"
        application_filenames.add(filename)
    if len(application_filenames) > 1:
        print("Too many application.properties files", application_filenames)
        exit(1)
    application_filename = list(application_filenames)[0]
    print("Writing to ", application_filename)
    new_contents = get_new_contents(verbose, defaults)
    with open(filename, "w") as f:
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
