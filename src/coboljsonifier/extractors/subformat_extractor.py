from abc import ABC, abstractmethod

from .book_item import BookItem


class SubformatExtractor(ABC):
    @abstractmethod
    def set_next(self, extractor):
        pass

    @abstractmethod
    def extract(self, book_item: BookItem):
        pass


class AbstractSubformatExtractor(SubformatExtractor):
    _next_extractor = None

    def set_next(self, extractor):
        self._next_extractor = extractor
        return extractor

    @abstractmethod
    def extract(self, book_item: BookItem):
        if self._next_extractor:
            return self._next_extractor.extract(book_item)

        return None


class SubformatEmpty(AbstractSubformatExtractor):
    def extract(self, book_item: BookItem):
        if not book_item.subformat:
            book_item.subformat = "NORMAL"
            return book_item
        else:
            return super().extract(book_item)


class SubformatComp3(AbstractSubformatExtractor):
    def extract(self, book_item: BookItem):
        if (book_item.type == "NUMERIC" or book_item.type == "SNUMERIC") \
            and book_item.subformat == "COMP-3":
                book_item.size = book_item.size // 2 + 1
                book_item.type = "NUMERIC_COMP3"
                return book_item
        else:
            return super().extract(book_item)


class SubformatBinary(AbstractSubformatExtractor):
    def extract(self, book_item: BookItem):
        if (book_item.type == "NUMERIC" or book_item.type == "SNUMERIC") \
            and book_item.subformat == "BINARY" or book_item.subformat == "COMP":
                if book_item.size > 0 and book_item.size <= 4:
                    book_item.size = 2
                elif book_item.size > 4 and book_item.size <= 9:
                    book_item.size = 4
                elif book_item.size > 9 and book_item.size <= 18:
                    book_item.size = 8
                else:
                    raise ValueError(f"Maior tamanho para BINARY = 18, informado = [{book_item.size}]")
                book_item.type = "NUMERIC_BINARY"
                return book_item
        else:
            return super().extract(book_item)


class SubformatUndefined(AbstractSubformatExtractor):
    def extract(self, book_item: BookItem):
        raise ValueError(f"Subformato nao desconhecido = [{book_item.subformat}].")

