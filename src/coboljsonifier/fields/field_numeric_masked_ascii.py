from .field import Field
from decimal import *

class FieldNumericMaskedAscii(Field):
    """
    Composite Pattern - Leaf
    """

    def __init__(self, type: str, name: str, size: int, decimals: int):
        super(FieldNumericMaskedAscii, self).__init__(type, name, size, decimals)

    def parse(self, data_in):
        if not data_in:
            return

        self._value = Decimal(data_in[:self.size])
        if self.decimals > 0:
            self._value = round(self._value, self.decimals)
        
        return data_in[self.size:]
