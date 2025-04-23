Na Programação POO, o termo *Factory* refere-se a uma classe ou método responsável por criar objetos

### Vantagens
1. Permitem criar um sistema com baixo acoplamento entre classes porque ocultam as classes que criam os objetos do código cliente

2. Facilitam a adição de novas classes ao código, porque o cliente não conhece e nem utiliza a implementação da classe (Utiliza a Factory)

3. Podem facilitar o processo de "cache" ou criação de "singletons" porque a factory pode retornar um objeto já criado para o cliente, ao invés de criar novos objetos sempre que o cliente precisar

### Desvantagens
1. Podem introduzir muitas classes no código

<br/>

# Simple Factory
Uma espécie de Factory Method parametrizado, ele pode não ser considerado como um padrão de projeto por si só e também pode quebrar os princípios SOLID

> Fornece uma interface para criar objetos, mas deixa que subclasses ou funções decidam qual classe instanciar

Ela encapsula a lógica de criação de objetos, especialmente quando ela é complexa ou depende de condições

### Quando usar
1. Você não quer que o cliente que usa o objeto não saiba detalhes de como criá-los
2. Existem múltiplos tipos de objetos derivados da mesma classe base
3. A lógica de criação depende de algum tipo de input

### Exemplo
> **Objetivo:** Criar diferentes tipos de veículos

```python
class Car:
    def drive(self): return "Dirigindo um carro"


class Bike:
    def drive(self): return "Pedalando uma bike"
```

> Agora com Factory Function (Method)

```python
def vehicle_factory(type):
    if type == "carro":
        return Car()

    if type == "bicicleta":
        return Bike()

   raise ValueError("Tipo desconhecido de veículo") 

vehicle = vehicle_factory("carro")
vehicle.drive() # Dirigindo um carro
```

> Versão com classe abstrata (interface)
``` python
from abc import ABC, abstractmethod

class Vehicle(ABC):
    @abstractmethod
    def drive: pass


class Car(Vehicle):
    def drive(self): return "Dirigindo um carro"


class Motorcycle():
    def drive(self): return "Pilotando uma moto"


class VehicleFactory:
    def create(type):
        if type == "carro":
            return Car()

        if type == "moto":
            return Motorcycle()

    raise ValueError("Tipo desconhecido de veículo") 
```