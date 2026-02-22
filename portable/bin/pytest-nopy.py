#!/usr/bin/env python3
import sys
import doctest
import runpy
import pathlib

def test_file(filename):
    """Run doctests in the given file."""
    # Execute the whole file first
    global_namespace = runpy.run_path(filename)
    finder = doctest.DocTestFinder()
    runner = doctest.DocTestRunner()

    # Run docstring examples for all functions/classes in globals
    for name, obj in global_namespace.items():
        if getattr(obj, "__module__", None) == "<run_path>" and hasattr(obj, '__doc__'):
            for test in finder.find(obj, name=name, globs=global_namespace):
                runner.run(test)
    runner.summarize()
    sys.exit(runner.failures and 1 or 0)


def main(argv: list[str] | None = None):
    filenames = argv[1:]
    # Dynamically generate pytest tests for each file
    print(pathlib.Path.cwd())
    for filename in filenames:
        test_file(filename)
        # pytest.main(["-q", "-p", "no:warnings", "-k", f"test_file('{fn}')"], plugins=[lambda: test_file(fn)])

if __name__ == "__main__":
    main(sys.argv)
