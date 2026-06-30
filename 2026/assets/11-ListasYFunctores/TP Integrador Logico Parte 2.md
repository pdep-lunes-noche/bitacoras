# Trabajo Práctico Integrador — Paradigmas de Programación — Lógico

# Entrega 2
### Aritmética. Individuos compuestos: Functores y polimorfismo. Listas.

## Introducción

Continuando con el modelado de la cátedra, ahora nos piden que incorporemos la información sobre las tareas que realizan sus integrantes y las responsabilidades de cada uno.

---

## Parte 1 — Tareas y asignaciones

Los integrantes de la cátedra tienen distintas tareas asignadas. Las tareas pueden ser:

* Dar clases de un paradigma determinado.
* Corregir parciales de un paradigma determinado.
* Corregir TPs de un paradigma determinado. Los TPs pueden ser grupales o individuales.
* Responder consultas.

Por ejemplo:

* IvanP da clases de lógico.
* Alf corrige el parcial de lógico.
* Rocha corrige el TP grupal de objetos.
* Fede corrige el TP grupal de lógico y además responde consultas.
* Naza responde consultas.
* Facu corrige el TP individual de funcional.
* Pedro da clases de funcional y corrige el parcial de funcional.

---

### Punto A — Modelado de asignaciones

Modelar las asignaciones de los integrantes utilizando functores que permitan representar los distintos tipos de tareas y sus características.

---

### Punto B — Tiene ejército de ayudantes y Docente con más ayudantes

Queremos saber quiénes son los docentes que tienen un ejército de ayudantes, es decir, aquellos que tienen más de 10 ayudantes a su cargo.


Además, necesitamos saber cuál es el docente que tiene la mayor cantidad de ayudantes a su cargo.

> 💡 *Tip: investiguen el predicado `max_member/2` de SWI-Prolog, podría serles muy útil para obtener el máximo de una lista.*

---

### Punto C — Está al pedo

Queremos saber quiénes son las personas que están al pedo, es decir, aquellas que están disponibles los 7 días de la semana.

---

### Punto D — Está sobrecargado

Consideramos que una tarea es pesada cuando se trata de corregir un parcial o de dar una clase. Queremos saber quiénes están sobrecargados, es decir, los integrantes que tienen más de 2 tareas pesadas asignadas.

---

## Parte 2 — Gustos y capacidades

### Punto E — Le gusta dar

Queremos modelar qué asignaciones le gustan a cada integrante de la cátedra. Las reglas son las siguientes:

* A Ivan le gusta cualquier asignación de lógico.
* A Pedro solo le gusta dar clases de funcional o de objetos.
* A Fede le gusta corregir cualquier cosa.
* A Marti le gusta corregir parciales.
* A Naza le gusta corregir TPs.
* A los ayudantes en general les gustan las asignaciones de funcional o de objetos, pero no las de lógico.

Para este punto puede ser útil definir primero un predicado que relacione una tarea con su paradigma. Notar que no todas las tareas tienen paradigma asociado.

---

### Punto F — Puede realizar

Queremos saber si una persona puede realizar una determinada tarea, según las siguientes reglas:

* Solo los docentes pueden corregir parciales.
* Solo los ayudantes pueden corregir TPs.
* Cualquier persona puede responder consultas.
* Los docentes siempre pueden dar clases.
* Los ayudantes pueden dar clases si existe un docente que esté dando esa misma clase, o si hay al menos 3 ayudantes asignados a esa clase.

---

### Punto G — Especialista en

Queremos saber en qué paradigma es especialista cada persona. Una persona es especialista en un paradigma si todas sus tareas que tienen paradigma asociado pertenecen a ese mismo paradigma.

---

### Punto H — Comparte ayudantes

Queremos saber cuándo dos docentes comparten al menos 5 ayudantes, es decir, cuando hay 5 ayudantes que estén en el equipo de ambos a la vez.

> 💡 *Tip: investiguen el predicado `intersection/3` de SWI-Prolog, podría serles muy útil para encontrar elementos comunes entre dos listas.*

---