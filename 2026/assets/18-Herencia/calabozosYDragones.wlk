class Mazmorra {
    const property dificultad

    method pasarDia(aventurero){
        self.costo(aventurero)
    }

    method costo(aventurero){
        aventurero.sacarVida(5*dificultad)
        aventurero.sacarEnergia(5*dificultad)
    }

    method esPeligroso(aventurero) = dificultad > aventurero.nivel() + 4
}

class MazamorraRecompensadora inherits Mazmorra{
    override method pasarDia(aventurero){
        super(aventurero)
        self.recompenso(aventurero)
    }

    method recompenso(aventurero)
}
class MazmorraMonstruo inherits MazamorraRecompensadora{
    override method recompenso(aventurero){
        aventurero.subirNivel()
    }
}

class MazmorraTrampa inherits MazamorraRecompensadora{
    override method recompenso(aventurero){
        aventurero.ganarOro(dificultad*15)
    }

}

class Taberna {
    var costo
    method pasarDia(aventurero){
        aventurero.recuperarVida(costo * 3)
        aventurero.recuperarEnergia(costo*3)
    }

    method esPeligroso(aventurero){
        return costo < 20
    }
}

class Aventurero {
    var nombre
    var vida
    var energia 
    var nivel
    var oro

    
}