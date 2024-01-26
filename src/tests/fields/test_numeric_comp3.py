from coboljsonifier.fields.field import Field
import unittest
from decimal import Decimal

from coboljsonifier.fields.field_numeric_comp3 import FieldNumericComp3


class TestFieldNumericComp3(unittest.TestCase):

    comp3_positive_value  = b'\x12\x34\x56\x7C' # ->  1234567C (C = positive)
    comp3_negative_value  = b'\x12\x34\x56\x7D' # ->  1234567D (D = negative)
    comp3_no_signal_value = b'\x12\x34\x56\x7F' # ->  1234567F (F = no sinal)

    def test_parsing(self):

        # Testa valor 1234567C (C = positivo) sem casas decimais
        comp3obj = FieldNumericComp3('NUMERIC_COMP3', 'FIELD-NUMERIC-COMP3', 4, 0)
        comp3obj.parse(self.comp3_positive_value)
        self.assertEqual(comp3obj.value, {'FIELD-NUMERIC-COMP3': Decimal('1234567')})

        # Testa valor 1234567C (C = positivo) com 2 casas decimais
        comp3obj = FieldNumericComp3('NUMERIC_COMP3', 'FIELD-NUMERIC-COMP3', 4, 2)
        comp3obj.parse(self.comp3_positive_value)
        self.assertEqual(comp3obj.value, {'FIELD-NUMERIC-COMP3': Decimal('12345.67')})
        
        # Testa valor 1234567C (C = positivo) com 4 casas decimais
        comp3obj = FieldNumericComp3('NUMERIC_COMP3', 'FIELD-NUMERIC-COMP3', 4, 4)
        comp3obj.parse(self.comp3_positive_value)
        self.assertEqual(comp3obj.value, {'FIELD-NUMERIC-COMP3': Decimal('123.4567')})
        
        # Testa valor 1234567D (D = negativo) com 2 casas decimais
        comp3obj = FieldNumericComp3('NUMERIC_COMP3', 'FIELD-NUMERIC-COMP3', 4, 2)
        comp3obj.parse(self.comp3_negative_value)
        self.assertEqual(comp3obj.value, {'FIELD-NUMERIC-COMP3': Decimal('-12345.67')})
        
        # Testa valor 1234567F (F = s/ sinal) com 2 casas decimais
        comp3obj = FieldNumericComp3('NUMERIC_COMP3', 'FIELD-NUMERIC-COMP3', 4, 2)
        comp3obj.parse(self.comp3_no_signal_value)
        self.assertEqual(comp3obj.value, {'FIELD-NUMERIC-COMP3': Decimal('12345.67')})


    # Verify later how could check it...
    # def test_values(self):

    #     self.assertRaises(ValueError, FieldNumericComp3, b'\x12\x34\x56\x7C', 10)
        
    #     self.assertRaises(ValueError, FieldNumericComp3, b'\x12\x34\x56\x7A', 2)
        
    #     # Testa valor High maior que 9
    #     self.assertRaises(ValueError, FieldNumericComp3, b'\x12\xB4\x56\x7C', 2)
        
    #     # Testa valor LOW maior que 9
    #     self.assertRaises(ValueError, FieldNumericComp3, b'\x12\x3B\x56\x7C', 2)



if __name__ == '__main__':
    unittest.main()