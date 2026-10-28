class Maquillaje {
    method capacidadDeSusto() = 3
}

class TrajeTerrorifico {
    method capacidadDeSusto() = 5
}

class TrajeTierno {
    method capacidadDeSusto() = 2
}

object estadoSano {
    method calcularActitud(cant){
        return cant
    }

    method comerCaramelos(cant, ninio) {
        if(cant > 10){
            ninio.cambiarDeEstado(empachado)
        }
    }

}

object empachado {
    method calcularActitud(cant) {
        return cant / 2 
    }

    method comerCaramelos(cant, ninio) {
        if(cant > 10){
            ninio.cambiarDeEstado(enCama)
        }
    }
}

object enCama {
    method calcularActitud(cant) {
        return 0
    }

    method comerCaramelos(cant, ninio) {
        throw new DomainException(message = "No puedo comer caramelos en cama")
    }

}
class Ninio inherits Asustador{
    const property elementos = []
    var property caramelos
    var actitud
    var estadoSalud

    method actitud(){
        return estadoSalud.calcularActitud(actitud)
    }

    method comerCaramelos(cantidad){
        if(caramelos >= cantidad){
            caramelos -= cantidad
            estadoSalud.comerCaramelos(cantidad, self)
        } else {
            throw new DomainException(message = "No tengo suficientes caramelos")
        }
    }

    method capacidadDeSusto(){
        return self.actitud() * self.capacidadDeSustoDeElementos()
    }

    method capacidadDeSustoDeElementos(){
        return elementos.sum({elemento => elemento.capacidadDeSusto()})
    }

    method recibirCaramelos(cant) {
        caramelos += cant
    }

}


class AdultoComun {
    const asustadores = []

    method tolerancia() = asustadores.filter({ninios => ninios.caramelos() > 15}).size() * 10
    method serAsustadoPor(ninio){
        asustadores.add(ninio)
        if(self.meAsusta(ninio.capacidadDeSusto())){
            ninio.recibirCaramelos(self.caramelosAEntregar())
        }
    }

    method meAsusta(capacidadDeSusto){
        return self.tolerancia() < capacidadDeSusto
    }

    method caramelosAEntregar() = self.tolerancia() / 2

}


class Abuelo inherits AdultoComun{
    override method meAsusta(capacidadDeSusto) = true
    override method caramelosAEntregar() = super() / 2
}

class AdultoNecio inherits AdultoComun {
    override method meAsusta(capacidadDeSusto) = false 
}

class Asustador {
    method asustarA(adulto){
        adulto.serAsustadoPor(self)
    }
}
class Legion inherits Asustador {
    const ninios = []

    method capacidadDeSusto(){
        return ninios.sum({ninio => ninio.capacidadDeSusto()})
    }

    method lider() {
        return ninios.max({ninio => ninio.capacidadDeSusto()})
    }
    method caramelos() {
        return ninios.sum({ninio => ninio.caramelos()})
    }

    method recibirCaramelos(cantidad){
        self.lider().recibirCaramelos(cantidad)
    }
}

class Barrio {
    const ninios = []

    method niniosMasRicos() {
       return ninios.sortBy({ninio => ninio.caramelos()}).take(3)
    }
}





