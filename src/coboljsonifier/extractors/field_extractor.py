import re
from abc import ABC, abstractmethod

from .book_item import BookItem


class FieldExtractor(ABC):
    '''
    Chain of responsability para tratar os tipos dos campos.

    ===========================================================================
      TYPES                                    CLASSES
    ===========================================================================
      -[Vazio]--------------------------------------------------------------
      00  Empty Content                        Fieldempty
      -[Numeric Types without signal]---------------------------------------
      01) 9          - 9+                       FieldSimpleNumeric
      02) 9V99       - 9+V9+                    FieldSimpleNumericDecimals1
      03) 9V9(2)     - 9+V9\([0-9]+\)           FieldSimpleNumericDecimals2
      04) 9(12)      - 9\([0-9]+\)              FieldSimpleNumeric1
      05) 9(12)V99   - 9\([0-9]+\)V9+           FieldSimpleNumeric1Decimals1
      06) 9(12)V9(2) - 9\([0-9]+\)V\([0-9]+\)   FieldSimpleNumeric1Decimals2
      -[Numeric Types with signal]------------------------------------------
      07) S9(12)     - S9\([0-9]+\)             FieldSignalNumeric1
      08) S9(12)V99  - S9\([0-9]+\)V9+          FieldSignalNumeric1Decimals1
      09) S9(12)V9(2) - S9\([0-9]+\)V\([0-9]+\) FieldSignalNumeric1Decimals2
      -[Masked Numeric Types]-----------------------------------------------
      A1)-- +99999999999999.99  [\+|\-]?[9|Z]+\.?[9|Z]* 
          - +99999999999999                     FieldNumericMasked1
          - +ZZZZZZZZZZZZZ9.99
          - ZZZZZZZZZZZZZZ.ZZ
          - 99999999999999999
      -[Alphabetic]---------------------------------------------------------
      10) A(12)      - A\([0-9]+\)              FieldAlphabetic
      -[Alphanumeric]-------------------------------------------------------
      11) X(12)      - X\([0-9]+\)              FieldAlphanumeric
      -[Undefined]---------------------------------------------------------
      12) None of the above                     FieldUndefined
    ===========================================================================
  
    '''

    @abstractmethod
    def set_next(self, extractor):
        pass

    @abstractmethod
    def extract(self, book_item: BookItem):
        pass


class AbstractFieldExtractor(FieldExtractor):
    _next_extractor = None

    def set_next(self, extractor):
        self._next_extractor = extractor
        return extractor

    @abstractmethod
    def extract(self, book_item: BookItem):
        if self._next_extractor:
            return self._next_extractor.extract(book_item)

        return None


# 00
class FieldGroup(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        if book_item.type == 'GROUP':
            return book_item
        else:
            return super().extract(book_item)


# 00
class FieldArray(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        if book_item.type == 'ARRAY':
            return book_item
        else:
            return super().extract(book_item)


# 00
class FieldEmpty(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        if not book_item.format:
            return book_item
        else:
            return super().extract(book_item)


# 01
class FieldSimpleNumeric(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^(9+)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = len(m.group(1))
            book_item.decimals = 0
            return book_item
        else:
            return super().extract(book_item)


# 02
class FieldSimpleNumericDecimals1(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^(9+)V(9+)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = len(m.group(1)) + len(m.group(2))
            book_item.decimals = len(m.group(2))
            return book_item
        else:
            return super().extract(book_item)


# 03
class FieldSimpleNumericDecimals2(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^(9+)V9\(([0-9]+)\)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = len(m.group(1)) + int(m.group(2))
            book_item.decimals = int(m.group(2))
            return book_item
        else:
            return super().extract(book_item)


# 04
class FieldSimpleNumeric1(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^9\(([0-9]+)\)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = int(m.group(1))
            book_item.decimals = 0
            return book_item
        else:
            return super().extract(book_item)


# 05
class FieldSimpleNumeric1Decimals1(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^9\(([0-9]+)\)V(9+)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = int(m.group(1)) + len(m.group(2))
            book_item.decimals = len(m.group(2))
            return book_item
        else:
            return super().extract(book_item)


# 06
class FieldSimpleNumeric1Decimals2(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^9\(([0-9]+)\)V9\(([0-9]+)\)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = int(m.group(1)) + int(m.group(2))
            book_item.decimals = int(m.group(2))
            return book_item
        else:
            return super().extract(book_item)


# 07
class FieldSignalNumeric1(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^S9\(([0-9]+)\)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = int(m.group(1))
            book_item.decimals = 0
            return book_item
        else:
            return super().extract(book_item)


# 08
class FieldSignalNumeric1Decimals1(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^S9\(([0-9]+)\)V(9+)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = int(m.group(1)) + len(m.group(2))
            book_item.decimals = len(m.group(2))
            return book_item
        else:
            return super().extract(book_item)


# 09
class FieldSignalNumeric1Decimals2(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^S9\(([0-9]+)\)V9\(([0-9])+\)$", book_item.format)
        if m:
            book_item.type = "NUMERIC"
            book_item.size = int(m.group(1)) + int(m.group(2))
            book_item.decimals = int(m.group(2))
            return book_item
        else:
            return super().extract(book_item)

# A1 
class FieldNumericMasked1(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):

        #([\+|\-]?[9|Z]+\.?[9|Z]*)
        m = re.search(r"^([\+|\-]?[9|Z]+\.?[9|Z]*)$", book_item.format)
        if m:
            book_item.type = "NUMERIC_MASKED"
            book_item.size = len(m.group(1))
            if m.group(1).find('.') >= 0:
                decimals = m.group(1).split(".")[-1]
                book_item.decimals = len(decimals)
            
            return book_item
        else:
            return super().extract(book_item)

# 10
class FieldAlphabetic(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^A\(([0-9]+)\)$", book_item.format)
        if m:
            book_item.type = "ALPHABETIC"
            book_item.size = int(m.group(1))
            book_item.decimals = 0
            return book_item
        else:
            return super().extract(book_item)


# 11
class FieldAlphanumeric(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        m = re.search(r"^X\(([0-9]+)\)$", book_item.format)
        if m:
            book_item.type = "ALPHANUMERIC"
            book_item.size = int(m.group(1))
            book_item.decimals = 0
            return book_item
        else:
            return super().extract(book_item)


# 12
class FieldUndefined(AbstractFieldExtractor):
    def extract(self, book_item: BookItem):
        raise Exception(f"ERRO ao processar field \n\t==> [{book_item.format}]")

