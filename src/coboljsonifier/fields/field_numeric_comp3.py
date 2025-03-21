from .field import Field
from decimal import *

class FieldNumericComp3(Field):
    """
    Composite Pattern - Leaf
    """

    def __init__(self, type: str, name: str, size: int, decimals: int):
        super(FieldNumericComp3, self).__init__(type, name, size, decimals)

    def parse(self, data_in):

        if not data_in:
            return

        data = data_in[:self.size]
        data_len = (len(data) * 2) - 1
        if self.decimals > data_len:
            raise ValueError(f"{self.name} - Decimal ({self.decimals}) exceeds the field size ({data_len})")

        result = ''
        signal = 1
        cont = 0

        for item in data:
            try:
                high, low = item >> 4, item & 0x0F
            except Exception as e:
                print(f'{self.name} - Error parsing field value ({self.name} - FieldNumericComp3')
                print(e)
                raise

            # Integrity validations
            if high > 9:
                raise ValueError(f'{self.name} - Invalid data! Expected high value between 0 and 9. Received {high} - {data}...')
            if cont < len(data) - 1:
                if low > 9:
                    raise ValueError(f'{self.name} - Invalid data! Expected low value between 0 and 9. Received {low} - {data}...')
                result += str(high) + str(low)
            else:
                # checking signal (C:12:+, D:13-, F:15:no signal)
                if low not in (12, 13, 15):
                    raise ValueError(
                        f'{self.name} - Invalid data! Expected field signal: 12[C], 13[D], or 15[F]. Received {low} - {data}...')
                if low == 13:
                    signal = -1
                result += str(high)
            cont += 1

        result = int(result) * signal

        if self.decimals > 0:
            result = round(Decimal(result / 10 ** self.decimals), self.decimals)

        self._value = result

        return data_in[self.size:]
