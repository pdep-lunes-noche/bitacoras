estaEn(americaDelSur, argentina).
estaEn(americaDelSur, brasil).
estaEn(americaDelSur, chile).
estaEn(americaDelSur, uruguay).
estaEn(americaDelNorte, alaska).
estaEn(americaDelNorte, yukon).
estaEn(americaDelNorte, canada).
estaEn(americaDelNorte, oregon).
estaEn(asia, kamtchatka).
estaEn(asia, china).
estaEn(asia, siberia).
estaEn(asia, japon).
estaEn(oceania,australia).
estaEn(oceania,sumatra).
estaEn(oceania,java).
estaEn(oceania,borneo).

limitrofes(argentina,brasil).
limitrofes(argentina,chile).
limitrofes(argentina,uruguay).
limitrofes(uruguay,brasil).
limitrofes(alaska,kamtchatka).
limitrofes(alaska,yukon).
limitrofes(canada,yukon).
limitrofes(alaska,oregon).
limitrofes(canada,oregon).
limitrofes(siberia,kamtchatka).
limitrofes(siberia,china).
limitrofes(china,kamtchatka).
limitrofes(japon,china).
limitrofes(japon,kamtchatka).
limitrofes(australia,sumatra).
limitrofes(australia,java).
limitrofes(australia,borneo).
limitrofes(australia,chile).

% Usar este para saber si son limítrofes ya que es una relación simétrica
sonLimitrofes(X, Y) :- limitrofes(X, Y).
sonLimitrofes(X, Y) :- limitrofes(Y, X).

jugador(amarillo).
jugador(magenta).
jugador(negro).
%jugador(blanco).

alianza(amarillo,magenta).

%el numero son los ejercitos
ocupa(argentina, magenta, 5).
ocupa(chile, negro, 3).
ocupa(brasil, amarillo, 8).
ocupa(uruguay, magenta, 5).
ocupa(alaska, amarillo, 7).
ocupa(yukon, amarillo, 1).
ocupa(canada, amarillo, 10).
ocupa(oregon, amarillo, 5).
ocupa(kamtchatka, negro, 6).
ocupa(china, amarillo, 2).
ocupa(siberia, amarillo, 5).
ocupa(japon, amarillo, 7).
ocupa(australia, negro, 8).
ocupa(sumatra, negro, 3).
ocupa(java, negro, 4).
ocupa(borneo, negro, 1).

%puedenAtacarse/2 relaciona dos jugadores si 
%ocupan al menos un par de paises que son limitrofes

puedenAtacarse(Jugador1, Jugador2):-
    ocupa(PaisA, Jugador1, _),
    ocupa(PaisB, Jugador2, _),
    Jugador1 \= Jugador2, 
    sonLimitrofes(PaisA,PaisB).

    %loLiquidaron/1 se cumple para un jugador si no
    %ocupa ningun paìs

    loLiquidaron(Jugador):-
        jugador(Jugador),
        forall(ocupa(Pais,_,_), not(ocupa(Pais, Jugador,_))).

%estaTodoBien/2 relaciona dos jugadores que
% no pueden atacarse 
% o son aliados

sonAliados(J1, J2):- alianza(J1,J2).
sonAliados(J1,J2):- alianza(J2,J1).

estaTodoBien(Jugador1, Jugador2):-
    sonAliados(Jugador1,Jugador2).

estaTodoBien(Jugador1,Jugador2):-
    jugador(Jugador1),
    jugador(Jugador2),
    not(puedenAtacarse(Jugador1,Jugador2)).

%ocupaContinente/2 relaciona un jugador y un continente
% si el jugador ocupa todos los paìses del mismo

continente(Continente):-
    estaEn(Continente,_).

ocupaContinente(Jugador,Continente):-
    jugador(Jugador),
    continente(Continente),
    forall(estaEn(Continente,Pais), ocupa(Pais,Jugador,_)).

%elQueTieneMasEjercitos/1 se cumple para el jugador
% que ocupa el pais con mas ejercitos del mundo

elQueTieneMasEjercitos(Jugador):-
    ocupa(_,Jugador, EjercitoDelJugador),
    forall(ocupa(_,_,Ejercito), EjercitoDelJugador >= Ejercito).

%juntan3/ relaciona dos paises y una cantidad. La
% cantidad representa la suma de los ejercitos en ambos
% paises
juntan(Pais1, Pais2, Cantidad):-
    ocupa(Pais1, _, Ejercito1),
    ocupa(Pais2,_, Ejercito2),
    Pais1 \= Pais2,
    Cantidad is Ejercito1 + Ejercito2.

%seguroGanaContra/2 que relaciona dos paises limitrofes
% de diferentes jugadores y es cierto cuando
%el primero tiene mas del doble de ejercito que el segundo

seguroGanaContra(Pais1,Pais2):-
    sonLimitrofes(Pais1,Pais2),
    ocupa(Pais1,Jugador1,Ejercito1),
    ocupa(Pais2,Jugador2,Ejercito2),
    Jugador1 \= Jugador2,
    Ejercito1 > (Ejercito2 * 2).

%estaPeleado/1 que se cumple para los continentes en los
%cuales cada jugador ocupa algun pais del mismo
estaPeleado(Continente):-
    continente(Continente),
    forall(jugador(Jugador), jugadorOcupaEn(Jugador,Pais,Continente)).

jugadorOcupaEn(Jugador,Pais,Continente):-
    ocupa(Pais,Jugador,_),
    estaEn(Continente,Pais).

%seAtrinchero/1 que se cumple para los jugadores que 
%ocupan paises en un unico continente.

seAtrinchero(Jugador):-
    jugador(Jugador),
    not(
        (
            jugadorOcupaEn(Jugador,_,Continente1),
            jugadorOcupaEn(Jugador,_,Continente2),
            Continente1 \= Continente2
        )
    ).

%puedeConquistar/2 relaciona un jugador y un continente
%si no ocupa dicho continente, pero todos los paises
% del mismo que no tiene son limitrofes a alguno que ocupa
% y a su vez ese pais no es de un aliado

paisesQueNoOcupaJugadorEn(Jugador, Pais, Continente):-
    jugador(Jugador),
    estaEn(Continente,Pais),
    not(ocupa(Pais, Jugador, _)).

puedeConquistar(Jugador,Continente):-
    jugador(Jugador),
    continente(Continente),
    not(ocupaContinente(Jugador,Continente)),
    forall(paisesQueNoOcupaJugadorEn(Jugador,Pais,Continente), esPaisCapturable(Jugador,Pais)).

esPaisCapturable(Jugador,Pais):-
    sonLimitrofes(Pais,PaisB),
    ocupa(PaisB,Jugador,_),
    ocupa(Pais,Jugador2,_),
    Jugador \= Jugador2,
    not(sonAliados(Jugador,Jugador2)).

%cuantoAgregaParaGanarSeguro/3 que relaciona dos países limítrofes de 
%diferentes jugadores y una cantidad, y es cierto cuando esa 
%cantidad es la cantidad de ejércitos que tengo que ponerle al 
%primer país para que le gane seguro al segundo.

cuantoAgregaParaGanarSeguro(Pais1,Pais2,Cantidad):-
    seguroGanaContra(Pais1,Pais2),
    ocupa(Pais1,Jugador1,Ejercito1),
    ocupa(Pais2,Jugador2,Ejercito2),
    Jugador1 \= Jugador2,
    Cantidad is (Ejercito2 * 2).




