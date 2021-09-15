from .field import Field
from decimal import *

class FieldNumericBinary(Field):
    """
    Composite Pattern - Leaf
    """

    def __init__(self, type: str, name: str, size: int, decimals: int):
        super(FieldNumericBinary, self).__init__(type, name, size, decimals)

    def parse(self, data_in):

        if not data_in:
            return

        data = data_in[:self.size]
        result = int.from_bytes(data, "big", signed="True")
        if self.decimals > 0:
            result = round(Decimal(result / (10 ** self.decimals)), self.decimals)
        
        self._value = result

        return data_in[self.size:]

