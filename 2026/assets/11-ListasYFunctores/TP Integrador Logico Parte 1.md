# Trabajo Práctico Integrador — Paradigmas de Programación — Lógico

# Entrega 1
### Hechos, consultas y reglas. Principio de universo cerrado. Unificación. Múltiples respuestas. Pattern Matching. Negación. Cuantificación universal (forall). Inversibilidad.

## Introducción

La materia Paradigmas de Programación cuenta con un equipo docente compuesto por docentes, ayudantes y distintas responsabilidades académicas. A lo largo de la cursada se organizan clases, consultas, correcciones, evaluaciones y asignaciones de tareas entre los distintos integrantes de la cátedra.
Ellos nos han pedido que los ayudemos, desarrollando un programa que les ayude a organizarse a realizar sus labores diarios.

---

## Parte 1 — Organización de la cátedra

Se desea comenzar modelando el equipo de trabajo de la materia.

Los integrantes del equipo pueden cumplir varios roles en simultaneo, por ejemplo pueden ser docentes y ayudantes a la vez. Además, los ayudantes pueden estar asociados a uno o más docentes.

Por ejemplo:

* Alf, Pedro, Rocha y Facu son docentes.
* Naza, Fede, Facu, Lu, Marti, IvanJ, IvanP, Pedro, Santi, Mati, Mateo, Dario, Tuca y May son ayudantes de Alf.
* Fede y Tuca además ayudan a Rocha y a Facu.

---

### Punto A — Modelado base

Modelar la base de conocimiento inicial de la cátedra.

---

### Punto B — Docente Solitarios

Primero, nos interesa saber, dentro de la cátedra, quienes son los docentes solitarios. Los docentes solitarios son aquellos que no tienen
ayudantes. 

---

### Punto C — Poliamorosos

A su vez, queremos saber quienes son los ayudantes poliamorosos. Los ayudantes poliamorosos son aquellos que ayudan a múltiples docentes.

---

## Parte 2 — Disponibilidad y coordinación

Ahora se desea modelar la disponibilidad de las personas para participar de actividades de la cátedra.

Se sabe que:

* los docentes y ayudantes podrían decidir juntarse cualquier día en la semana según disponibilidad (lunes a domingo)
* algunas personas trabajan y otros tendrán otras responsabilidades.
* Las personas que trabajan no pueden participar durante días laborales. (Lunes a Viernes)

Además:

* salvo evidencia de lo contrario, las personas se consideran disponibles,


### Punto D — Modelado de disponibilidad

Nos interesa saber la disponibilidad de una persona en algún respectivo dia.

* Sabemos que los que trabajan son alf, pedro y santi.
* Naza solamente puede participar los mismos días que Pedro.
* May solo está disponible los días en los que todos los que estén disponibles para juntarse, sean ayudantes ó no haya docentes.

---

### Punto E — Pueden juntarse a corregir


Nos interesa saber cuando se puede juntar a corregir un equipo docente,
que se cumple cuando un docente y todos los integrantes de su equipo pueden reunirse a corregir un determinado día.

---

### Punto F — Docente Celoso

También nos interesa conocer a los docentes celosos, que son aquellos que no comparten a sus ayudantes con ningún otro docente.

---

## Parte 3 — Notas

### Punto G — Notas del equipo

Gracias a un soplón, hemos sido capaces de obtener las notas de los parciales del equipo docente. De cada ayudante o docente sabemos la nota que sacó en cada parcial de cada paradigma.

* Por ejemplo, sabemos que pedro se sacó un 9 en el parcial de lógico, un 8 en el parcial de funcional y un 10 en el parcial de objetos.
* Naza se sacó un 6 en lógico, un 7 en funcional y un 9 en objetos.
* Lu se sacó 8 en funcional, 9 en lógico y 10 en objetos.
* Tuca se sacó un 6 en funcional un 4 en lógico y un 2 en objetos.


### Punto H — Bochos

Queremos saber quienes son los ayudantes o docentes bochos, los bochos son aquellos que todas sus notas son de promoción y además su promedio de notas es mayor igual a 8.


### Punto I — Colado

Queremos saber quienes son los ayudantes colados, los colados son aquellos
que tienen al menos 1 parcial desaprobado (menor a 6).
