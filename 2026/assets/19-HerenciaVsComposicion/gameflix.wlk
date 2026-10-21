class Juego {
    const nombre
    const precio
    const categoria

    method jugar(horas, usuario)
    method esCategoria(unaCategoria){
        return categoria == unaCategoria
    } 
    method esBarato() = precio < 30

}

class Estrategia inherits Juego{
    override method jugar(horas, usuario){
        usuario.aumentarHumor(horas * 5)
    }
}

class Violento inherits Juego{
    override method jugar(horas,usuario){
        usuario.reducirHumor(10*horas)
    }
}

class MOBA inherits Juego {
    override method jugar(horas,usuario){
        usuario.pagar(30)
    }
}

class Terror inherits Juego {
    override method jugar(horas, usuario) {
        usuario.cambiarSubscripcion(new Infantil())
    }
}

class Usuario {
    var subscripcion
    var humor
    var plata
    var nombre

    method libreria() = subscripcion.libreria()
    method juegoRandom() = subscripcion.juegoRandom()
    method aumentarHumor(cant){
        humor += cant
    }

    method reducirHumor(cant){
        humor = 0.max(humor-cant)
    }

    method cambiarSubscripcion(nuevaSubscripcion) {
        subscripcion = nuevaSubscripcion
    }

    method costoSubscripcion() = subscripcion.costo()

    method pagarSubscripcion() {
        self.pagar(self.costoSubscripcion())
    }

    method puedePagar(montoAPagar) = plata >= montoAPagar

    method pagar(cant) {
        if(cant <= plata){
            plata -= cant
        } else {
            throw new NotEnoughFunds()
        }
    }
}

object gameflix {
    const usuarios = []
    const property juegos = []

    method cobrarSubscripciones() {
        usuarios.forEach({
            usuario => self.cobrarSubscripcion(usuario)})
        }

    method cobrarSubscripcion(usuario) {
        try {
            usuario.pagarSubscripcion()
        } catch e : NotEnoughFunds {
            usuario.cambiarSubscripcion(new Prueba())
        }
    }

    method cobrarSubscripcion2(usuario) {
        if(usuario.puedePagar(usuario.costoSubscripcion())){
            usuario.pagarSubscripcion()
        } else {
            usuario.cambiarSubscripcion(new Prueba())
        }
    }

}

class NotEnoughFunds inherits DomainException(message= "Fondos Insuficientes") {}

class Subscripcion {
    const property costo
    method libreria()
    method juegoRandom() = self.libreria().anyone() 
    method buscarJuego(nombre) = self.libreria().find({juego => juego.tieneNombre(nombre)})  
}

class Premium inherits Subscripcion(costo = 50) {

    override method libreria() = gameflix.juegos()
}

class SubscripcionCategorica inherits Subscripcion{
    var categoriaPermitida
    override method libreria() = gameflix.juegos().filter({juego => juego.esCategoria(categoriaPermitida)})

}
class Infantil inherits SubscripcionCategorica(categoriaPermitida = "INFANTIL", costo = 15) {
}
class Prueba inherits SubscripcionCategorica(categoriaPermitida = "DEMO", costo = 0) {

}
class Base inherits Subscripcion(costo = 25) {
    override method libreria() = gameflix.juegos.filter({juego => juego.esBarato()})
}

