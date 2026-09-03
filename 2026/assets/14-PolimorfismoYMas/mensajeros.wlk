object puenteBrooklyn {
    method dejaPasar(mensajero){
        return mensajero.peso() < 1000
    }
}

object matrix{
    method dejaPasar(mensajero){
        return mensajero.puedeLlamar()
    }
}

object chuckNorris {
    const peso = 900
    method puedeLlamar() = true
    method peso() = peso
    method puedeLlevar(paquete,destino){
        return paquete.estaPago() && destino.dejaPasar(self)
    } 
}

object roberto{
    var pesoPropio = 90
    var property vehiculo = camion 

    method puedeLlamar() = false
    method puedeLlevar(unPaquete,destino){
        return unPaquete.estaPago() && destino.dejaPasar(self)
    } 

    method peso(){
        return pesoPropio + vehiculo.peso()
    }
}

object bicicleta {
    method peso() = 1
}

object camion {
    var cantidadAcoplados = 1
    method peso() = cantidadAcoplados * 500
    method agregarAcoplado(){
        cantidadAcoplados +=1
    }
    method quitarAcoplado(){
        cantidadAcoplados = 0.max(cantidadAcoplados - 1) 
    }
}

object neo {
    var tieneCredito = false
    method peso() = 0
    method puedeLlamar() = tieneCredito

    method puedeLlevar(unPaquete,destino){
        return unPaquete.estaPago() && destino.dejaPasar(self)
    }
}

object paquete {
    var estaPago = false
    var property destino = matrix
    method estaPago(){
        return estaPago
    }

    method pagar() {
        estaPago = true
    }
    method puedeEntregarse(mensajero){
        return estaPago && destino.dejaPasar(mensajero)
    }
}

/* Contratar a un mensajero 
Despedir a un mensajero 
Despedir a todos los mensajeros 
Analizar si la mensajería es grande (si tiene más de dos mensajeros) 
Consultar si el paquete puede ser entregado por el primer empleado de la empresa de mensajería. 
Saber el peso del último mensajero de la empresa. 
Hacer algunos test significativos.  */

object empresa {
    var mensajeros = [neo, roberto] 

    method despedirUnMensajero(mensajero) {
        mensajeros.remove(mensajero)
    }
    method despedirATodos() {
        mensajeros.clear()
    }
    method esMensajeriaGrande(){
        return mensajeros.size() > 2
    }

    method puedeSerEntregado(unPaquete){
        return unPaquete.puedeEntregarse(mensajeros.first())
    }
    method pesoDelUltimoMensajero(){
        return mensajeros.last().peso()
    }
} 
