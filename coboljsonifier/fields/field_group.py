from __future__ import annotations

from typing import List, Dict

from .field import Field


class FieldGroup(Field):

    """
    Composite Pattern - Composite
    """

    def __init__(self, type: str, name: str):
       super(FieldGroup, self).__init__(type, name)
       
       self._children: List[Field] = []

    def add(self, field: Field) -> None:
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

        return {self.name : response}


    @property
    def size(self) -> int:
        size = 0
        for child in self._children:
            size += child.size
        return size
