from .field import Field
from decimal import *


class FieldNumericAscii(Field):
    """
    Composite Pattern - Leaf
    """

    def __init__(self, type: str, name: str, size: int, decimals: int):
        super(FieldNumericAscii, self).__init__(type, name, size, decimals)

    def parse(self, data_in):
        if not data_in:
            return

        if not data_in[:self._size].strip():
            return None

        try:
            self._value = int(data_in[:self._size].strip())
            if self.decimals > 0:
                self._value = round(Decimal(self._value / 10 ** self.decimals), self.decimals)
        except Exception as e:
            print(e)
            print(f"Erro ao tratar o campo: {self.name} com conteudo: [{data_in[:self._size]}]")
            raise

        return data_in[self._size:]

