class Dragon {
    var vida = 100
    const escamas = []
    var property tipoAtaque = ataqueNulo

    method crecer(){
        escamas.forEach({escama => escama.endurecer()})
        self.perderUnaEscama()
    }

    method perderUnaEscama(){
        escamas.remove(escamas.anyOne())
    }

    method atacar(objetivo){
        if(self.estoyVivo()){
            objetivo.recibirAtaque(tipoAtaque.poder())
        } else {
            throw new DomainException(message= "Estoy muerto")
        }
    }
    method recibirAtaque(daño) {
      self.perderVida(0.max(daño - self.defensa()))
    }

    method estoyVivo() = vida > 0
    method perderVida(cantidad) {
      vida = 0.max(vida - cantidad)
    }

    method defensa() = escamas.sum({escama => escama.defensa()})
    
    method poder() = tipoAtaque.poder(self)
    method cantidadEscamasDuras() = escamas.filter({escama => escama.esDura()}).size()
    method cantidadEscamas() = escamas.size()
    method defensaPromedio() = self.defensa() / self.cantidadEscamas()

    method aparear(pareja) {
        const ataqueBebe = [pareja,self].anyOne().tipoAtaque()
        const nuevasEscamas = []
        const cantidadEscamas = self.cantidadEscamas() + pareja.cantidadEscamas()
        cantidadEscamas.times({i => nuevasEscamas.add(new Escama(defensa = (self.defensaPromedio()) * 0.1)) })
        return new Dragon(escamas = nuevasEscamas , tipoAtaque = ataqueBebe)
    } 


}

class Escama {
    var property defensa

    method endurecer(){
        defensa += 10
    }
    method esDura() = defensa > 5
}

object ataqueNulo {
    method poder(dragon) = 0
}

object ataqueFuego {
    method poder(dragon) = dragon.cantidadEscamasDuras() * 5
}

class AtaqueHiperSonico {
    const decibeles

     method poder(dragon) = decibeles * 2
}

class AtaqueSolar{
    var estaCargado = false
    method poder(dragon) {
        estaCargado = estaCargado.not()
        if(estaCargado){
            return 500
        } else {
            return 0
        }
    }
}

class Manada {
    const dragones = []

    method poder() = dragones.sum({dragon => dragon.poder()})
    method deragonOP(manada) = manada.poder() < self.dragonMasPoderoso()
    method dragonOP(manada) = dragones.any({dragon => dragon.poder() > manada.poder()})
    method dragonMasPoderoso() = dragones.max({dragon => dragon.poder()})
}