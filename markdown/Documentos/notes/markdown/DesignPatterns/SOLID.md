- [Single Responsability Principle](#single-responsability-principle)
- [Open / Closed Principle](#open--closed-principle)
- [Liskov substitution principle](#liskov-substitution-principle)
- [Interface Segregation Principle](#interface-segregation-principle)
- [Dependency Inversion Principle](#dependency-inversion-principle)

<br/>

# Single Responsability Principle
> Princípio da responsabilidade única

Uma classe deve ter apenas um motivo para mudar 

**Exemplo de Violação:**
``` python
class Report:
    def __init__(self, content):
        self.content = content
    
    def save_to_file(self, filename):
        with open(filename, "w") as f:
            f.write(self.content)
    
    def print_report(self):
        print(self.content)
```

**Caso Correto:**
``` python
class Report:
    def __init__(self, content):
        self.content = content
   
class ReportPrinter:
    def print_report(self, report):
        print(report.content)

class ReportSaver:
    def save_to_file(self, report, filename):
        with open(filename, "w") as f:
            f.write(report.content)
```

<br/>

# Open / Closed Principle
> Princípio do aberto / fechado

Classes, objetos ou métodos devem estar abertos para extensão, mas fechados para modificações

**Exemplo de Violação (Modificando Lógica):**
``` python
class Discount:
    def apply(self, price, user_type):
        if user_type == "student":
            return price * 0.5
        if user_type == "vip":
            return price * 0.8
        return price
```

**Caso Correto (Extensível por Herança):**
``` python
class Discount:
    def apply(self, price):
        return price

class StudentDiscount(Discount):
    def apply(self, price):
        return price * 0.5

class VipDiscount(Discount):
    def apply(self, price):
        return price * 0.8
```

<br/>


# Liskov substitution principle
> Princípio da substituição de Liskov

Classes derivadas devem ser capazes de substituir totalmente classes-bases

**Exemplo de Violação (Quebrando o código da classe base):**
``` python
class Bird:
    def fly(self):
        print("Flying")

class Ostrich(Bird):
    def fly(self):
        raise Exception("Ostriches can't fly")
```

**Caso Correto (Subsitituindo o código da classe base):**
``` python
class Bird:
    def move(self):
        pass

class Sparrow(Bird):
    def move(self):
        print("Flying")

class Ostrich(Bird):
    def move(self):
        print("Running")
```

<br/>

# Interface Segregation Principle
> Princípio da segregação de interface

Os clientes não devem ser forçados a depender de interfaces que não utilizam

**Exemplo de Violação (Classe forçada a implementar métodos que não usa):**
``` python
class Animal:
    def fly(self): pass
    def swin(self): pass

class Dog(Animal):
    def fly(self): pass
    def swin(self): print("Swimming")
```

**Caso Correto:**
``` python
class Swimmer:
    def swin(self): pass

class Flyer:
    def fly(self): pass

class Dog(Swimmer):
    def swing(self): print("Swimming")

class Eagle(Flyer):
    def fly(self): print("Flying")
```

<br/>

# Dependency Inversion Principle
> Princípio da inversão de dependência

Módulos de alto nível não devem ser dependentes do módulos de baixo nível; ambos devem depender de abstrações. Detalhes devem depender de abstrações, não o inverso

**Exemplo de Violação (Classe depende de uma implementação concreta):**
``` python
class MySqlDatabase:
    def connect(self): print("Conectado ao MySQL")

class App:
    def __init__(self):
        self.db = MySqlDatabase()
```

**Caso Correto (Injeção de Dependência):**
``` python
class Database:
    def connect(self): pass

class MySqlDatabase(Databse):
    def connect(self): print("Conectado ao MySQL")

class App:
    def __init__(self, db: Database):
        self.db = db

# Injetando Dependência
db = MySqlDatabase()
app = App(db)
```