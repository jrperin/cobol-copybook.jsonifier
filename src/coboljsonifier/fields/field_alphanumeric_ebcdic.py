from .field import Field


class FieldAlphanumericEbcdic(Field):
    """
    Composite Pattern - Leaf
    """

    def __init__(self, type: str, name: str, size: int):
        super(FieldAlphanumericEbcdic, self).__init__(type, name, size, 0)

    def parse(self, data_in):
        if not data_in:
            return

        self._value = data_in[:self._size].decode('cp500').strip()
        if not self._value:
            self._value = None

        return data_in[self._size:]

