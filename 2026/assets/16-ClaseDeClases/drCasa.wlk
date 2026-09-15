class Persona {
    var enfermedades = []
    var property nombre 
    var property temperatura
    var property cantCelulas

    method contraerEnfermedad(unaEnfermedad){
        enfermedades.add(unaEnfermedad)
    }

    method subirTemperatura(cant){
        temperatura = 45.min(temperatura + cant)
    }

    method pasarDia(){
        enfermedades.forEach({enfermedad => enfermedad.pasarDia(self)})
    }

    method matarCelulas(cant){
        if(cantCelulas >= cant){
            cantCelulas -= cant
        } else {
            throw new DomainException(message= "No me quedan celulas suficientes")
        }
    }
}

class Infecciosa {
    var property cantCelulasAfectadas 
    const property persona

    method causarEfecto(){
        persona.subirTemperatura(cantCelulasAfectadas / 1000)
    }

    method reproducirse(){
        cantCelulasAfectadas *= 2
    }
    method esAgresiva(){
        return cantCelulasAfectadas > persona.cantCelulas() * 0.1 
    }

    method pasarDia(){
        self.reproducirse()
        self.causarEfecto()
    }
}


class AutoInmune {
    var property cantDias = 0
    var property cantCelulasAfectadas
    const property persona 

    method causarEfecto(){
        persona.matarCelulas(cantCelulasAfectadas)
    }

    method pasarDia(){
        self.causarEfecto()
        cantDias += 1
    }

    method esAgresiva(){
        return cantDias > 30
    }

}