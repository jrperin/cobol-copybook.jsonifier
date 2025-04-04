from .field import Field
from decimal import *

class FieldNumericEbcdic(Field):
    """
    Composite Pattern - Leaf
    """

    def __init__(self, type: str, name: str, size: int, decimals: int):
        super(FieldNumericEbcdic, self).__init__(type, name, size, decimals)

    def parse(self, data_in):
        result = ""

        if not data_in:
            return

        signal = 1
        data = data_in[:self.size]

        for i in range(len(data)):
            try:
                high, low = data[i] >> 4, data[i] & 0x0F
            except Exception as e:
                print(f'{self.name} - Error parsing field value ({self.name} - FieldNumericEbcdic)')
                print(e)
                raise
            if i == (len(data) - 1):
                if high not in (12, 13, 15):
                    raise ValueError(
                        f'{self.name} - Invalid data! Expected field signal: 12[C], 13[D], or 15[F]. Received {high}...')
                if high == 13:
                    signal = -1
            else:
                if high != 15:
                    raise ValueError(
                        f'{self.name} - Invalid data! Expected COMP3 high value is 15[F], but received = [{high}]. data = [{data}]..')

            result += str(low)
        result = int(result) * signal
        if self.decimals > 0:
            result = round(Decimal(result / 10 ** self.decimals), self.decimals)

        self._value = result

        return data_in[self.size:]
