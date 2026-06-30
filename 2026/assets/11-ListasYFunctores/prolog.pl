%cabeza([Cabeza | _], Cabeza).
% nacimiento(pedro, 1996, 1, 20).
%nacimiento(pedro, fecha(1996,1,20)).
%nacimiento(macBookPro, plantaChina).

entrena(pedro, charmander).
entrena(pedro, pikachu).
entrena(pedro, mewtwo).

entrena(ash, pikachu).
entrena(ash, charizard).
entrena(ash, snorlax).

entrenador(Entrenador):-
    entrena(Entrenador,_).

equipo(Entrenador, Equipo):-
    entrenador(Entrenador),
    findall(Pokemon, entrena(Entrenador, Pokemon), Equipo).


%lanzallamas 50 de potencia tipo fuego -> Charmander
%mordida (ataque físico de 25) y 
movimiento(charmander, ataqueEspecial(lanzallamas, 50,fuego)).
movimiento(charmander, ataqueFisico(mordida,25)).
movimiento(charmander, ataqueFisico(rasguño, 5)).
movimiento(charizard, ataqueFisico(mordida,25)).
movimiento(pikachu, ataqueEspecial(impactrueno, 40, electrico)).
movimiento(pikachu, ataqueEstado(dormir)).

% Los Ataques Especiales fuego es potencia + 20
% Ataques especiales electricos es potencia * 2
% ataques fisicos son solo potencia
% cualquiera de estado es 0
dañoDeAtaque(ataqueEstado(_), 0).
dañoDeAtaque(ataqueFisico(_,Daño), Daño).
dañoDeAtaque(ataqueEspecial(_,Potencia,fuego), Daño):-
    Daño is Potencia + 20.
dañoDeAtaque(ataqueEspecial(_,Potencia,electrico), Daño):-
    Daño is Potencia * 2.

pokemon(Pokemon):-
    entrena(_,Pokemon).

dañoTotalPokemon(Pokemon, DañoTotal):-
    pokemon(Pokemon),
    findall(Daño, (movimiento(Pokemon, Movimiento), dañoDeAtaque(Movimiento, Daño)), ListaDeDaños ),
    sumlist(ListaDeDaños, DañoTotal).

dañoTotalEquipo(Entrenador, DañoTotalEquipo):-
    entrena(Entrenador, _),
    findall(Daño, (entrena(Entrenador, Pokemon), dañoTotalPokemon(Pokemon, Daño)), ListaDeDaños),
    sum_list(ListaDeDaños, DañoTotalEquipo).


    equipoMasFuerte(Entrenador1, Entrenador2):-
        entrena(Entrenador1, _),
        entrena(Entrenador2, _),
        dañoTotalEquipo(Entrenador1, DañoTotal1),
        dañoTotalEquipo(Entrenador2, DañoTotal2),
        Entrenador1 \= Entrenador2,
        DañoTotal1 > DañoTotal2.


%esPokemonPoderoso si tiene daño total > 100 
% o es legendario
% mewtwo es legendario

esLegendario(mewtwo).

esPokemonPoderoso(Pokemon):-
    esLegendario(Pokemon).

esPokemonPoderoso(Pokemon):-
    dañoTotalPokemon(Pokemon,Daño),
    Daño > 20.


%entrenadorPoderoso si tiene 3 o mas pokemones poderosos

entrenadorPoderoso(Entrenador):-
    entrena(Entrenador,_),
    findall(Pokemon, (entrena(Entrenador, Pokemon), esPokemonPoderoso(Pokemon)), ListaPokemones),
    length(ListaPokemones, Largo),
    Largo >= 3.

%esEntrenadorTramposo si en su equipo tiene un pokemon legendario

esEntrenadorTramposo(Entrenador):-
    entrena(Entrenador, Pokemon),
    esLegendario(Pokemon).

%esSuperTramposo si todos sus pokemones son legendarios.

esSuperTramposo(Entrenador):-
    entrenador(Entrenador),
    forall(entrena(Entrenador,Pokemon), esLegendario(Pokemon)).


%esMedioTramposo si tiene 2 pokemones legendarios

esMedioTramposo(Entrenador):-
    entrena(Entrenador, Pokemon1),
    esLegendario(Pokemon1),
    entrena(Entrenador, Pokemon2),
    esLegendario(Pokemon2),
    Pokemon1 \= Pokemon2.