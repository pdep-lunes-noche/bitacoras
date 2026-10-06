class Persona {
    var enfermedades = []
    var property nombre 
    var property temperatura
    var property cantCelulas

    method matar(){
        temperatura = 0
    }

    method recibirMedicamento(dosis){
        enfermedades.forEach({enfermedad => enfermedad.atenuar(dosis)})
        enfermedades.filter({enfermedad => enfermedad.estaCurada()})
    }

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

class Enfermedad {

    method estaCurada() = cantCelulasAfectadas == 0
    var cantCelulasAfectadas
    method atenuar(dosis){
        cantCelulasAfectadas = 0.max(cantCelulasAfectadas - dosis * 15)
    }

    method pasarDia(persona)

    method causarEfecto(persona){
        persona.matarCelulas(cantCelulasAfectadas)
        self.efectoAdicional(persona)
    }
    method efectoAdicional(persona)
}

class Infecciosa inherits Enfermedad {

    override method efectoAdicional(persona){
        persona.subirTemperatura(cantCelulasAfectadas / 1000)
    }

    method reproducirse(){
        cantCelulasAfectadas *= 2
    }
    method esAgresiva(persona){
        return cantCelulasAfectadas > persona.cantCelulas() * 0.1 
    }

    override method pasarDia(persona){
        self.reproducirse()
        self.causarEfecto(persona)
    }
}

object laMuerte {
    
    method atenuar() {}
    method esAgresiva() = true
    method pasarDia(persona){
        persona.matar()
    }

    method estaCurada() = false
}

class AutoInmune inherits Enfermedad {
    var property cantDias = 0

    override method  efectoAdicional(persona){
        // aca iria el efecto adicional
    }
    override method pasarDia(persona){
        self.causarEfecto(persona)
        cantDias += 1
    }

    method esAgresiva(persona){
        return cantDias > 30
    }

}

class Medico inherits Persona {
    var dosis
    method atender(unaPersona){
        unaPersona.recibirMedicamento(dosis)
    }

    override method contraerEnfermedad(unaEnfermedad){
        super(unaEnfermedad)
        self.atender(self)
    }


}

class JefeDeDepartamento inherits Medico{
    const subordinados = []  

    override method atender(unaPersona){
        subordinados.anyOne().atender(unaPersona)
    }
}