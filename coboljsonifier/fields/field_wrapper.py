from __future__ import annotations

from typing import List, Dict

from .field import Field


class FieldWrapper(Field):
    """
    Composite Pattern - Composite
    """

    def __init__(self):
        super(FieldWrapper, self).__init__(type, None, 0, 0)

        self._children: List[Field] = []

    def add(self, field: Field) -> None:
        # print('type ------> ', type(field))
        self._children.append(field)
        field.parent = self

    def remove(self, field: Field) -> None:
        self._children.remove(field)
        field.parent = None

    def is_complex(self) -> bool:
        return True

    def parse(self, data_in):
        data = data_in
        for child in self._children:
            data = child.parse(data)
        return data

    @property
    def value(self) -> Dict:
        response = {}
        for child in self._children:
            response.update(child.value)
        return response

    @property
    def size(self) -> int:
        size = 0
        for child in self._children:
            size += child.size
        return size

