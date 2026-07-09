# Trabajo Práctico Integrador — Paradigmas de Programación — Lógico

# Entrega 3
### Recursividad con y sin listas. Explosión combinatoria.

## Parte 1 — Mentorías y recursividad

Dentro de la cátedra existen relaciones de mentoría e influencia académica entre las personas. Por ejemplo, una persona puede haber aprendido las bases de un paradigma de otra, quien a su vez aprendió de otra persona, y así sucesivamente.

### Punto A — Modelado de mentorías
Modelar las relaciones directas de aprendizaje e influencia entre los integrantes de la cátedra. 

Se sabe que:
* Naza aprendió de Alf.
* Alf aprendió de Pedro.
* Fede aprendió de Rocha y de Facu.
* Tuca aprendió de Fede.
* Marti aprendió de Naza.

---

### Punto B — Aprendizaje indirecto

Programar el predicado recursivo `aprendioIndirectamenteDe/2` que permita inferir si una persona fue influenciada académicamente por un mentor, ya sea porque fue su mentor directo o a través de una cadena de mentores intermedios.

| Consulta | Resultado esperado | Tipo |
|---|---|---|
| `aprendioIndirectamenteDe(marti, naza)` | `true` | `semidet` |
| `aprendioIndirectamenteDe(marti, pedro)` | `true` | `semidet` |
| `aprendioIndirectamenteDe(tuca, rocha)` | `true` | `semidet` |
| `aprendioIndirectamenteDe(alf, rocha)` | `false` | `semidet` |
| `aprendioIndirectamenteDe(tuca, X)` | `X = fede ; X = rocha ; X = facu` | `multidet (3 sol.)` |

---

## Parte 2 — Vías de escape: El partidito de fútbol

Para distenderse de las responsabilidades académicas, los integrantes de la cátedra suelen organizar partidos de fútbol 5 o fútbol 11. Sin embargo, armar el equipo no siempre es fácil: las convocatorias se expanden de boca en boca a través de la red de contactos.

Para este evento deportivo, una persona **conoce** a otra si:
* Trabajan juntos en el mismo equipo docente (es decir, una persona `ayudaA/2` al mismo docente que la otra o son sus ayudantes).
* Existe una relación de mentoría directa entre ambos (cualquiera sea el sentido).
* Son amigos.
* Son familiares.

Además, para el partido debemos considerar la disponibilidad del día y las obligaciones de cada uno:
* Los **amigos** están siempre disponibles ("son de fierro", no tienen restricciones de días).
* Los **familiares** solo están disponibles los fines de semana (sábado y domingo).
* Quienes pertenezcan al **equipo docente** se rigen bajo las reglas de disponibilidad ya pautadas en la Entrega 1.
* Ningún integrante del partido puede estar **sobrecargado** de tareas (según lo definido en la Entrega 2).

---

### Punto C — Vínculos fuera de la cátedra

Modelar las relaciones de amistad y familiares para las siguientes personas:
* Pedro es amigo de Juan y de Ana.
* Juan es amigo de Martín y de Lucas.
* Ana es amiga de Tomás.
* Tomás es amigo de Sofía.
* Martín es amigo de Laura.
* Naza es familiar de María y de Nico.

Además, implementar las reglas de disponibilidad para los amigos y familiares según el criterio mencionado anteriormente. Se pueden sumar más vínculos de ser necesario

---

### Punto D — Red de Conocidos (Alcanzabilidad)

Desarrollar el predicado `alcanzable/2` que determine si una persona puede llegar a contactar a otra a través de cualquier cantidad de intermediarios en su red de contactos (vínculos de la cátedra, mentorías, amigos o familiares).

---

### Punto E — Convocatoria: El equipo ideal

Llegó el momento de armar el partido. Dado un **Organizador** y un **Día** de la semana específico, queremos conocer todos los posibles equipos de fútbol (exactamente de 11 personas) que se pueden conformar.

Para que un equipo sea válido:
* Todos los miembros del equipo (incluyendo al organizador) deben ser alcanzables por la red de contactos del organizador.
* Todos los miembros deben estar disponibles el día del partido.
* Ninguno puede estar sobrecargado de tareas.
* El equipo final debe estar constituido por **exactamente 11 integrantes**.

| Consulta | Resultado esperado | Tipo |
|---|---|---|
| `equipoDeFutbol(pedro, sabado, Equipo)` | `Equipo = [pedro, juan, ana, martin, tomas, lucas, maria, nico, vale, emi, fede] ; ...` | `multidet (múltiples combinaciones de 11)` |
| `equipoDeFutbol(pedro, lunes, Equipo)` | `false` *(Muchos conocidos o familiares no pueden ir los lunes)* | `semidet` |

---