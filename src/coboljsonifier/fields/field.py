from __future__ import annotations

from abc import ABC, abstractmethod
from typing import Dict


class Field(ABC):

    """
    Composite Pattern
    Objetos em estruturas de árvores.
    """

    def __init__(self, type: str, name: str, size: int, decimals: int):
        self.type = type
        self.name = name
        self._size = size
        self.decimals = decimals
        self._value = None
        self.parent = None


    @property
    def parent(self) -> Field:
        return self._parent

    @parent.setter
    def parent(self, parent: Field):
        self._parent = parent
        
    def add(self, field: Field) -> None:
        pass

    def remove(self, field: Field) -> None:
        pass

    def is_complex(self) -> bool:
        return False

    @abstractmethod
    def parse(self, data) -> str:
        pass

    @property
    def value(self) -> Dict:
        return {self.name : self._value }

    @property
    def size(self) -> int:
        return self._size

    @size.setter
    def size(self, size):
        self._size = size
