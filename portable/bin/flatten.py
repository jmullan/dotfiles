#!/usr/bin/env -S python-venv --virtualenv dotfiles
from collections.abc import Iterable, Iterator
from functools import wraps
from typing import TypeVar

T = TypeVar("T")


def flatten(*items: T | Iterable[T]) -> Iterator[T]:
    for item in items:
        if isinstance(item, Iterable | tuple):
            yield from flatten(*item)
        else:
            yield item


def flatten_args(func):
    @wraps(func)
    def flat(*items: int | Iterable) -> int:
        f = flatten(items)
        return func(*f)

    return flat


@flatten_args
def my_sum(*items: int | Iterable) -> int:
    total = 0
    for item in items:
        total += item
    return total


print(my_sum(1))
print(my_sum(1, 2))
print(my_sum([1, 2]))
print(my_sum(x for x in [1, 2]))
print(my_sum([[1, 2], [3, 4]]))
