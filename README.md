# COPYBOOK PARSER


* Where to put tests
   > http://pythonchb.github.io/PythonTopics/where_to_put_tests.html
   
* Site que ensina a empacotar de forma fácil
   > https://packaging.python.org/tutorials/packaging-projects/

* Site completo sobre módulos Python
    > https://docs.python.org/3/tutorial/modules.html

* Import relativo e absoluto
    > https://realpython.com/absolute-vs-relative-python-imports/

* tests
    > https://pythontesting.net/framework/specify-test-unittest-nosetests-pytest/

[CHANGELOG.md](CHANGELOG.md)

-----

<spam style="color:#cc0000">**Atenção:**</spam>

* Para manuseio de ojetos JSON utilizar a biblioteca <spam style="color:#009900">`simplejson`</spam>.

* A biblioteca <spam style="color:#cc0000">`json`</spam> tem **incompatibilidade** com os tipos <spam style="color:#0099ff">`Decimal`</spam> usados para precisao de valores monetários.

-----

## Para instalar local

```bash
$ python -m venv venv
$ source venv/bin/activate
$ pip install -r requirements.txt

```

## Para testar
**OBS:** TESTES ESTAO QUEBRADOS... PRECISA ARRUMAR...

**Por hora, os testes estão sendo feitos pelo `'parser_tester.py'`**

``` bash
$ python parser_tester.py
```

---
``` bash
# Executando apenas o teste
python -m unittest discover

# Executando teste e verificando cobertura
coverage run -m unittest discover
coverage report -m
coverage html
```

## Estrutura do Projeto
```
🌳 /cobol-copybook.jsonifier
├── 📄 CHANGELOG.md
├── 📄 README.md


FALTA ACERTAR ...


```

## Arquivos de exemplo

* ... acertar
 
 
``` text
:DT00:-ORG :DT00:-ACCT         :DT00:-STMT-ID-CODE
3/NUM      19/AN               4/BI              
(1-3)      (4-22)              (23-26)           
3--------- 4------------------ 5------------------
********************************* TOP OF DATA ****
         0                                               0
         4     0004002479000390216                -2016308
         4     0004002479000390216                -2016308
 
COPY DT00RL
002200    01    :DT00:-RECORD.                                 
002300     03  :DT00:-KEY.                                
002400         05  :DT00:-ORG-ACCT.                       
002500             07  :DT00:-ORG  PIC 999.               
002600             07  :DT00:-ACCT PIC X(19).             
002700         05  :DT00:-STMT-ID-CODE                    
002800                             PIC S9(7)       BINARY.

```
