escribio(rayuela, cortazar).
escribio(silmarilion, tolkien).
escribio(elHobbit,tolkien).
escribio(harryPotter1, jkRowling).
escribio(harryPotter2, jkRowling).
escribio(harryPotter3, jkRowling).
escribio(harryPotter4, jkRowling).
escribio(harryPotter5, jkRowling).
escribio(harryPotter6, jkRowling).
escribio(harryPotter7, jkRowling).
escribio(dragonBall,akiraToriyama).
escribio(onePiece,oda).

nacionalidad(japones,akiraToriyama).
nacionalidad(japones,oda).
nacionalidad(ingles, jkRowling).


progenitor(homero, bart).
progenitor(homero, lisa).
progenitor(homero, maggie).
progenitor(abe, homero).
progenitor(abe, jose).
progenitor(jose, pepe).
progenitor(mona, homero).
progenitor(jacqueline, marge).
progenitor(marge, bart).
progenitor(marge, lisa).
progenitor(marge, maggie).




esAutor(Autor):-
    escribio(_,Autor).

esManga(Obra):-
    escribio(Obra,Autor),
    nacionalidad(japones, Autor).
    
esMangaV2(Obra):-
    escribio(Obra,akiraToriyama).
esMangaV2(Obra):-
    escribio(Obra, oda).


hermano(Hermano1,Hermano2):-
    progenitor(Padre,Hermano1),
    progenitor(Padre,Hermano2),
    Hermano1 \= Hermano2.

tio(Tio,Sobrino):-
    hermano(Tio,Progenitor),
    progenitor(Progenitor, Sobrino).

abuelo(Abuelo,Nieto):-
    progenitor(Abuelo,Progenitor),
    progenitor(Progenitor,Nieto).

primo(Primo1,Primo2):-
    hermano(Progenitor1,Progenitor2),
    progenitor(Progenitor1, Primo1),
    progenitor(Progenitor2, Primo2),
    not(hermano(Primo1,Primo2)).

