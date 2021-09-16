import re
from abc import ABC, abstractmethod

from ..fields.field import Field


class StructureExtractor(ABC):
    '''
    Chain of responsability que extrai a estrutura
    de acordo com regex específico.
    '''

    @abstractmethod
    def set_next(self, extractor):
        pass

    @abstractmethod
    def extract(self, field: Field):
        pass


class AbstractStructureExtractor(StructureExtractor):
    _next_extractor = None

    def set_next(self, extractor):
        self._next_extractor = extractor
        return extractor

    @abstractmethod
    def extract(self, line: str, field: Field):
        if self._next_extractor:
            return self._next_extractor.extract(line, field)

        return None


class GroupStructureExtractor(AbstractStructureExtractor):
    def extract(self, line: str, field: Field):
        # Ex.: 
        # 03  VQBE-KEY.
        line = line.replace("USAGE", "").replace("usage", "")
        m = re.search(r"^.{6}[^(*)]\s*([0-9]+)\s+([\w|-]*)\s*\..*$", line)
        if m:
            # type, level, name, format, subformat
            field.type = "GROUP"
            field.level = int(m.group(1))
            field.name = m.group(2)
            field.format = ""
            field.subformat = ""
            return field
        else:
            return super().extract(line, field)


class ArrayStructureExtractor(AbstractStructureExtractor):
    def extract(self, line: str, field: Field):
        # Ex.: 
        # 01 WS-TABLE.
        #     05 WS-A OCCURS 10 TIMES. <--
        #         10 WS-B PIC A(10).
        #         10 WS-C OCCURS 5 TIMES.
        #             15 WS-D PIC X(6).
        line = line.replace("USAGE", "").replace("usage", "")
        m = re.search(r"^.{6}[^(*)]\s*([0-9]+)\s+([\w|-]*)\s+OCCURS\s+([0-9]+).*$", line)
        if m:
            field.type = "ARRAY"
            field.level = int(m.group(1))
            field.name = m.group(2)
            field.occurs = m.group(3)
            field.subformat = ""
            return field
        else:
            return super().extract(line, field)


class SimpleFieldStructureExtractor(AbstractStructureExtractor):
    def extract(self, line: str, field: Field):
        # Ex.:
        # 05  VQBE-NUM-CPF-CNPJ         PIC X(14).
        # 05  VQBE-NUM-CPF-CNPJ         PIC 999.
        # 05  VQBE-NUM-CPF-CNPJ         PIC 999V99.
        # 05  VQBE-NUM-CPF-CNPJ         PIC S9(07).
        # 05  VQBE-NUM-CPF-CNPJ         PIC S9(07)V99.
        # 05  VQBE-NUM-CPF-CNPJ         PIC S9(07)V9(2).
        line = line.replace("USAGE", "").replace("usage", "")
        m = re.search(r"^.{6}[^(*)]\s*([0-9]+)\s+([\w|-]*)\s+PIC\s+([\w|\d|\(|\)]*)\s*\..*$", line)
        if m:
            field.type = "FIELD"
            field.level = int(m.group(1))
            field.name = m.group(2)
            field.format = m.group(3)
            field.subformat = ""
            return field

        else:
            # Tratamento do formato especial MASKED
            #          05  VQLBMIG-VS-AMT            PIC +99999999999999.99.
            #          05  VQLBMIG-VS-AMT            PIC +99999999999999 .
            #          05  VQLBMIG-VS-AMT            PIC +ZZZZZZZZZZZZZ9.99 .
            #          05  VQLBMIG-VS-AMT            PIC +ZZZZZZZZZZZZZ9 .
            #          05  VQLBMIG-VS-AMT            PIC +ZZZZZZZZZZZZZZ.ZZ .
            #          05  VQLBMIG-VS-AMT            PIC +ZZZZZZZZZZZZZZ .
            #          05  VQLBMIG-VS-AMT            PIC +ZZZZZZZZZZZZZZ .     xxxyy
            #          05  VQLBMIG-VS-AMT            PIC +ZZZZZZZZZZZZZZ BINARY .     xxxyy
            #          05  VQLBMIG-VS-AMT            PIC ZZZZZZZZZZZZZZ BINARY .     xxxyy
            #          05  VQLBMIG-VS-AMT            PIC ZZZZZZZZZZZZZZ.99 BINARY .     xxxyy
            m = re.search(r"^.{6}[^(*)]\s*([0-9]+)\s+([\w|-]*)\s+PIC\s+([\+|\-]?[9|Z]+\.?[9|Z]*).*\..*$", line)
            if m:
                field.type = "FIELD"
                field.level = int(m.group(1))
                field.name = m.group(2)
                field.format = m.group(3)
                field.subformat = ""
                return field
            else:
                return super().extract(line, field)


class SubformatStructureExtractor(AbstractStructureExtractor):
    def extract(self, line: str, field: Field):
        # Ex.:
        # 05  VQBE-DAT-INI-ENDR             PIC S9(07) COMP-3.
        # 05  VQBE-DAT-INI-ENDR             PIC S9(07) BINARY.
        # 05  VQBE-DAT-INI-ENDR             PIC S9(07) COMP.
        # 05  VQBE-DAT-INI-ENDR             PIC S9(07) USAGE COMP-3. <-- USAGE nao previsto...
        line = line.replace("USAGE", "").replace("usage", "")
        m = re.search(r"^.{6}[^(*)]\s*([0-9]+)\s+([\w|-]*)\s+PIC\s+([\w|\(|\)]*)\s+([\w|-]*)\s*\..*$", line)
        if m:
            field.type = "FIELD"
            field.level = int(m.group(1))
            field.name = m.group(2)
            field.format = m.group(3)
            field.subformat = m.group(4)
            return field
        else:
            return super().extract(line, field)


class RedefinesStructureExtractor(AbstractStructureExtractor):
    def extract(self, line: str, field: Field):
        # Ex.:
        # 03  :AMSL:-AMBS-DATA    REDEFINES :AMSL:-DATA.
        m = re.search(r"^.*REDEFINES.*$", line)
        if m:
            raise Exception(f"Impossivel tratar book com REDEFINES \n\t==> {line}")
        else:
            return super().extract(line, field)


class UndefinedStructureExtractor(AbstractStructureExtractor):
    def extract(self, line: str, field: Field):
        raise Exception(f"ERRO ao processar linha \n\t==>{line}")

