leGusta(chuy, peces).

leGusta(duba, churrasco).
leGusta(duba, asado(entrania, limonada)).

leGusta(lita, ensalada([tomate, lechuga])).

leGusta(coty, pasto).

leGusta(manic, pasto).
leGusta(manic, churrasco).

tipo(peces, carne).
tipo(churrasco, carne).

tipo(tomate, vegetal).
tipo(lechuga, vegetal).
tipo(pasto, vegetal).

tipo(asado(PlatoPrincipal, _), carne) :-
    tipo(PlatoPrincipal, carne).

tipo(ensalada(Ingredientes), vegetal) :-
    forall(
        member(Ingrediente, Ingredientes),
        tipo(Ingrediente, vegetal)
    ).


lugar(desierto, [tomate, lechuga]).

lugar(bosque, [
    churrasco,
    churrasco,
    churrasco
]).

lugar(llanura, [pasto]).

lugar(playa_patagonica, [
    peces,
    peces,
    peces,
    peces,
    lechuga,
    lechuga,
    lechuga
]).

cantidadIngredientes(Lugar, Total) :-
    lugar(Lugar, Ingredientes),
    length(Ingredientes, Total).


tieneVegetal(Lugar) :-
    lugar(Lugar, Ingredientes),
    member(Ingrediente, Ingredientes),
    tipo(Ingrediente, vegetal).


esNutritivo(Lugar) :-
    cantidadIngredientes(Lugar, Cantidad),
    Cantidad > 2,
    tieneVegetal(Lugar).


recorrido(norte, desierto).

recorrido(desierto, llanura).

recorrido(llanura, bosque).

recorrido(bosque, playa_patagonica).

puedeLlegar(Origen, Destino) :-
    recorrido(Origen, Destino).

puedeLlegar(Origen, Destino) :-
    recorrido(Origen, Intermedio),
    puedeLlegar(Intermedio, Destino).


conoce(coty, lita).

conoce(lita, duba).

conoce(duba, manic).
conoce(duba, capy).

conoce(capy, manic).
conoce(capy, lita).
conoce(capy, coty).

seConocen(A, B) :-
    conoce(A, B).

seConocen(A, B) :-
    conoce(B, A).


caminoDeConocidos(Persona1, Persona2, Camino) :-
    caminoAux(
        Persona1,
        Persona2,
        [Persona1],
        Camino
    ).


% Caso base

caminoAux(Persona, Persona, _, [Persona]).


% Caso recursivo

caminoAux(Persona1, Persona2, Visitados, [Persona1 | Resto]) :-
    seConocen(Persona1, Intermedio),
    \+ member(Intermedio, Visitados),
    caminoAux(
        Intermedio,
        Persona2,
        [Intermedio | Visitados],
        Resto
    ).