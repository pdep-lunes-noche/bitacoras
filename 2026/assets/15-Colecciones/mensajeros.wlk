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

object paquetito {
    const costo = 0
    var estaPago = false
    
    method costo(){
        return costo
    }

    method estaPago(){
        return estaPago
    }

    method pagar() {
        estaPago = true
    }

    method puedeEntregarse(unMensajero){
        return estaPago
    }
}

object paquetonViajero {
    const destinos = [matrix, puenteBrooklyn]
    const costo = 100
    var estaPago = false
    method estaPago(){
        return estaPago
    }
    method pagar(){
        estaPago = true
    }

    method costo(){
        return costo * destinos.size()
    }
    method puedeEntregarse(unMensajero){
        return estaPago && destinos.all({destino => destino.dejaPasar(unMensajero)})
    }
}

object paquete {
    const costo = 50
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
    const empleados = [neo, roberto, chuckNorris]
    const paquetesPendientes = []

    method contratar(unMensajero) {
        empleados.add(unMensajero)
    }
    
    method despedir(unMensajero) {
        empleados.remove(unMensajero)
    }

    method despedirAll(){
        empleados.clear()
    }

    method esGrande(){
        return empleados.size() > 2
    }

    method puedeSerEntregado(unPaquete){
        return unPaquete.puedeEntregarse(empleados.first())
        // return empleados.first().puedeEntregar(unPaquete)
    }

    method pesoUltimoEmpleado(){
        return empleados.last().peso()
    }

    method enviarPaquete(unPaquete){
        empleados.findOrElse(self.puedeEntregar(unPaquete),
        {paquetesPendientes.add(unPaquete)})
    }

    method esPaqueteEntregable(unPaquete){
        return empleados.any(self.puedeEntregar(unPaquete))
    }

    method mensajerosValidos(unPaquete){
        return empleados.findAll(self.puedeEntregar(unPaquete))
    }

    // Para evitar repetición de lógica con los closures.
    method puedeEntregar(unPaquete){
        return { mensajero => unPaquete.puedeEntregarse(mensajero) }
    }

    method tieneSobrepeso(){
        return empleados.average({empleado => empleado.peso()}) > 500
    }

    method enviarMuchosPaquetes(listaDePaquetes){
        listaDePaquetes.forEach({ 
            unPaquete => self.enviarPaquete(unPaquete)})
    }
}

object emisor {
    var plata = 100

    method enviarPaquete(unPaquete){
        if(self.puedePagar(unPaquete.costo())){
            self.pagarPaquete(unPaquete)
            empresa.enviarPaquete(unPaquete)
        }
    }

    method enviarMuchosPaquetesPermisivo(muchosPaquetes){
        muchosPaquetes.forEach({ unPaquete => self.enviarPaquete(unPaquete)})
    }

    method enviarMuchosPaquetesEstricto(muchosPaquetes){

        const costoTotal = muchosPaquetes.sum({unPaquete => unPaquete.costo()})
        if(self.puedePagar(costoTotal)){
            self.gastar(costoTotal)
            muchosPaquetes.forEach({unPaquete => unPaquete.pagar()})
            empresa.enviarMuchosPaquetes(muchosPaquetes)
        }
    }


    method pagarPaquete(unPaquete){
            self.gastar(unPaquete.costo())
            unPaquete.pagar()
    }
    method puedePagar(costo){
        return plata >= costo
    }

    method gastar(monto){
        plata -= monto
    }
}