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
            raise ValueError(f"{self.name} - Decimal ({self.decimals}) excede o tamanho do campo ({data_len})")

        result = ''
        signal = 1
        cont = 0

        for item in data:
            try:
                high, low = item >> 4, item & 0x0F
            except Exception as e:
                print(f'{self.name} - Erro no parse do valor do campo ({self.name} - FieldNumericComp3')
                print(e)
                raise

            # Validacoes de integridade
            if high > 9:
                raise ValueError(f'{self.name} - Dados inválidos! High value esperado entre 0 e 9. Recebido {high} - {data}...')
            if cont < len(data) - 1:
                if low > 9:
                    raise ValueError(f'{self.name} - Dados inválidos! LOW value esperado entre 0 e 9. Recebido {low} - - {data}...')
                result += str(high) + str(low)
            else:
                # checking signal (C:12:+, D:13-, F:15:no sinal)
                if low not in (12, 13, 15):
                    raise ValueError(
                        f'{self.name} - Dados inválidos! Signal do campo esperado: 12[C], 13[D] ou 15[F]. Recebido {low} - {data}...')
                if low == 13:
                    signal = -1
                result += str(high)
            cont += 1

        result = int(result) * signal

        if self.decimals > 0:
            result = round(Decimal(result / 10 ** self.decimals), self.decimals)

        self._value = result

        return data_in[self.size:]

