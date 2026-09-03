object tom {
    var energia = 10

    method velocidad() {
        return 5 + (energia / 10)
    }

    method puedeAtrapar(presa) {
        return self.velocidad() > presa.velocidad()
    }

}

object jerry {
    var property peso = 3
    method velocidad(){
        return 10 - peso
    }
}

object robotRaton {
    const VELOCIDAD_ROBOT = 8
    method velocidad() = VELOCIDAD_ROBOT
}

object spike {
    var agresividad = 10
    var energia = 10

    method velocidad() = agresividad + 5 + (energia / 10)
    method puedeAtrapar(presa) {
        return self.velocidad() > presa.velocidad()
    }
}