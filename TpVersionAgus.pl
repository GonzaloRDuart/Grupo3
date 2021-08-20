materia(analisisMatematico1, 5, no).
materia(algebraYGeometriaAnalitica, 5, no).
materia(matematicaDiscreta, 3, no).
materia(sistemasYOrganizaciones, 3, si).
materia(algoritmosYEstructurasDeDatos, 5, no).
materia(arquitecturaDeComputadoras, 4, no).
materia(ingenieriaYSociedad, 2, no).
materia(quimica, 3, no).
materia(fisica1, 5, no).
materia(analisisMatematico2, 5, no).
materia(probabilidadYEstadistica, 3, no).
materia(analisisDeSistemas, 6, si).
materia(sintaxisYSemanticaDeLosLenguajes, 4, no).
materia(paradigmasDeProgramacion, 4, no).
materia(ingles1, 2, no).
materia(sistemasDeRepresentacion, 3, no).
materia(sistemasOperativos, 4, no).
materia(disenioDeSistemas, 6, si).
materia(fisica2, 5, no).
materia(matematicaSuperior, 4, no).
materia(gestionDeDatos, 4, no).
materia(legislacion, 2, no).
materia(economia, 3, no).
materia(ingles2, 2, no).
materia(redesDeInformacion, 4, no).
materia(administracionDeRecursos, 6, si).
materia(investigacionOperativa, 5, no).
materia(simulacion, 4, no).
materia(ingenieriaDeSoftware, 3, no).
materia(teoriaDeControl, 3, no).
materia(comunicaciones, 4, no).
materia(proyectoFinal, 6, si).
materia(inteligenciaArtificial, 3, no).
materia(administracionGerencial, 3, no).
materia(sistemasDeGestion, 4, no).

esNecesaria(analisisDeSistemas, sistemasYOrganizaciones).
esNecesaria(analisisDeSistemas, algoritmosYEstructurasDeDatos).
esNecesaria(analisisMatematico2, analisisMatematico1).
esNecesaria(analisisMatematico2, algebraYGeometriaAnalitica).
esNecesaria(sintaxisYSemanticaDeLosLenguajes, matematicaDiscreta).
esNecesaria(sintaxisYSemanticaDeLosLenguajes, algoritmosYEstructurasDeDatos).
esNecesaria(paradigmasDeProgramacion, matematicaDiscreta).
esNecesaria(paradigmasDeProgramacion, algoritmosYEstructurasDeDatos).
esNecesaria(probabilidadYEstadistica, analisisMatematico1).
esNecesaria(probabilidadYEstadistica, algebraYGeometriaAnalitica).
esNecesaria(disenioDeSistemas, analisisDeSistemas).
esNecesaria(disenioDeSistemas, paradigmasDeProgramacion).
esNecesaria(sistemasOperativos, matematicaDiscreta).
esNecesaria(sistemasOperativos, algoritmosYEstructurasDeDatos).
esNecesaria(sistemasOperativos, arquitecturaDeComputadoras).
esNecesaria(fisica2, analisisMatematico1).
esNecesaria(fisica2, fisica1).
esNecesaria(economia, analisisDeSistemas).
esNecesaria(gestionDeDatos, analisisDeSistemas).
esNecesaria(gestionDeDatos, paradigmasDeProgramacion).
esNecesaria(gestionDeDatos, sintaxisYSemanticaDeLosLenguajes).
esNecesaria(ingles2, ingles1).
esNecesaria(matematicaSuperior, analisisMatematico2).
esNecesaria(legislacion, analisisDeSistemas).
esNecesaria(legislacion, ingenieriaYSociedad).
esNecesaria(administracionDeRecursos, disenioDeSistemas).
esNecesaria(administracionDeRecursos, sistemasOperativos).
esNecesaria(administracionDeRecursos, economia).
esNecesaria(ingenieriaDeSoftware, probabilidadYEstadistica).
esNecesaria(ingenieriaDeSoftware, disenioDeSistemas).
esNecesaria(ingenieriaDeSoftware, gestionDeDatos).
esNecesaria(teoriaDeControl, quimica).
esNecesaria(teoriaDeControl, matematicaSuperior).
esNecesaria(comunicaciones, arquitecturaDeComputadoras).
esNecesaria(comunicaciones, analisisMatematico2).
esNecesaria(comunicaciones, fisica2).
esNecesaria(redesDeInformacion, sistemasOperativos).
esNecesaria(redesDeInformacion, comunicaciones).
esNecesaria(investigacionOperativa, probabilidadYEstadistica).
esNecesaria(investigacionOperativa, matematicaSuperior).
esNecesaria(simulacion, probabilidadYEstadistica).
esNecesaria(simulacion, matematicaSuperior).
esNecesaria(inteligenciaArtificial, investigacionOperativa).
esNecesaria(inteligenciaArtificial, simulacion).
esNecesaria(administracionGerencial, administracionDeRecursos).
esNecesaria(administracionGerencial, investigacionOperativa).
esNecesaria(sistemasDeGestion, administracionDeRecursos).
esNecesaria(sistemasDeGestion, investigacionOperativa).
esNecesaria(sistemasDeGestion, simulacion).
esNecesaria(proyectoFinal, legislacion).
esNecesaria(proyectoFinal, administracionDeRecursos).
esNecesaria(proyectoFinal, redesDeInformacion).
esNecesaria(proyectoFinal, ingenieriaDeSoftware).

esIntegradora(Materia):-
    materia(Materia, _, si).

esPesada(Materia):-
    materia(Materia, Duracion, _),
    Duracion = 6,
    esIntegradora(Materia).

esPesada(Materia):-
    materia(Materia, Duracion, _),
    Duracion >= 4,
    not(esIntegradora(Materia)).

tieneCorrelativa(Materia):-
    esNecesaria(Materia, _).

/*Saltan multiplicadas */

esInicial(Materia):-
    materia(Materia, _, _),
    not(tieneCorrelativa(Materia)).

tambienNecesito(Materia, SubMateria):-
    esNecesaria(Materia, SubMateria).
tambienNecesito(Materia, SubMateria):-
    esNecesaria(Materia, X),
    tambienNecesito(X, SubMateria).

/* Definicion recursiva, funciona pero puede devolver varias veces lo mismo si hay 2 materias necesarias que a su vez comparten materias necesarias.
Considerar utilizar multiples niveles de tambienNecesito en vez de recursividad.
Ej:
tambienNecesito(Materia, Submateria):-
    esNecesaria(Materia, Submateria).
tambienNecesito(Materia, Submateria):-
    esNecesaria(Materia, X),
    esNecesatia(X, Submateria).
tambienNecesito(Materia, Submateria):-
    esNecesaria(Materia, X1),
    esNecesaria(X1, X2),
    esNecesaria(X2, Submateria).
tambienNecesito(Materia, Submateria):-
    esNecesaria(Materia, X1),
    esNecesaria(X1, X2),
    esNecesaria(X2, X3),
    esNecesaria(X3, Submateria).
Verificar si haría falta otro nivel. */

habilitaMaterias(Materia, Habilitadas):-
    esNecesaria(Habilitadas, Materia).

/* agregar que la inversibilidad cubra todas las correlativas como en el punto anterior. TRANSITIVIDAD. */

curso(vero, Materia, 8, modo(anual, 2019)):-
    esInicial(Materia).
curso(alan, sistemasYOrganizaciones, 6, modo(anual, 2020)).
curso(alan, analisisMatematico1, 6, modo(anual, 2020)).
curso(alan, analisisDeSistemas, 2, modo(anual, 2019)).
curso(alan, analisisDeSistemas, 9, modo(anual, 2020)).
curso(alan, fisica1, 2, modo(anual, 2019)).
curso(naruto, sistemasYOrganizaciones, 6, modo(anual, 2018)).
curso(naruto, quimica, 2, modo(cuatrimestral, 2020, 1)).
curso(naruto, quimica, 6, modo(cuatrimestral, 2020, 2)).
curso(naruto, fisica1, 8, modo(anual, 2019)).
curso(naruto, matematicaDiscreta, 5, modo(anual, 2019)).
curso(naruto, matematicaDiscreta, 8, modo(cuatrimestral, 2020, 1)).
curso(veraniego, matematicaDiscreta, 5, modo(anual, 2017)).
curso(veraniego, matematicaDiscreta, 8, modo(verano, 2018)).
curso(veraniego, analisisDeSistemas, 8, modo(verano, 2019)).
curso(veraniego, sistemasYOrganizaciones, 8, modo(verano, 2020)).
final(alan, sistemasYOrganizaciones, 4).
final(alan, ingles1, 2).
final(vero, ingles2, 10).

cursada(Alumno, Materia):-
    curso(Alumno, Materia, Nota, _),
    Nota >= 6.
cursada(Alumno, Materia):-
    final(Alumno, Materia, Nota),
    Nota >= 6.

/*despues poner nombre mas claro. Es la cursada aprobada, no solo si curso.
Revisar tambien tema de cursada/finales.  */

aprobada(Alumno, Materia):-
    curso(Alumno, Materia, Nota, _),
    Nota > 7.
aprobada(Alumno, Materia):-
    final(Alumno, Materia, Nota),
    Nota >= 6.

/* Falta terminar de modelar estudiantes. 
Diferenciar final libre de rendir final? */

anioDeCursada(Alumno, Materia, Anio):-
    curso(Alumno, Materia, _, modo(anual, Anio)).
anioDeCursada(Alumno, Materia, Anio):-
    curso(Alumno, Materia, _, modo(cuatrimestral, Anio, _)).
anioDeCursada(Alumno, Materia, Anio):-
    curso(Alumno, Materia, _, modo(verano, AnioCalendario)),
    Anio is AnioCalendario - 1.

/* La consola no maneja bien el caso de quimica. Y tira error luego de indicar el año de SyO. Arreglar */

recurso(Alumno, Materia):-
    curso(Alumno, Materia, _, Cursada1),
    curso(Alumno, Materia, _, Cursada2),
    Cursada1 \= Cursada2.

/* Despues ver si se nos ocurre nombre mas lindo para Cursada1 y 2 */

estudiante(naruto).
estudiante(vero).
estudiante(alan).
estudiante(veraniego).

invictus(Estudiante):-
    estudiante(Estudiante),
    not(recurso(Estudiante, _)).

promociona(Estudiante, Materia):-
    curso(Estudiante, Materia, Nota, _),
    Nota >= 8.

buenasCursadas(Estudiante):-
    estudiante(Estudiante),
    forall(curso(Estudiante, Materia, _, _), promociona(Estudiante, Materia)).

repechaje(Estudiante):-
    curso(Estudiante, Materia, Nota1, modo(anual, Anio1)),
    Nota1 < 6,
    curso(Estudiante, Materia, Nota2, modo(cuatrimestral, Anio2, 1)),
    Nota2 >= 8,
    Anio2 is Anio1 + 1.

/*Comparar con otras funciones.
Podemos crear un predicado para no repetir Anio2 is Anio1 + 1 y que sea mas declarativo */

recursaInmediatamente(Estudiante, Materia):-
    curso(Estudiante, Materia, _, modo(cuatrimestral, Anio, 1)),
    curso(Estudiante, Materia, _, modo(cuatrimestral, Anio, 2)).
recursaInmediatamente(Estudiante, Materia):-
    curso(Estudiante, Materia, _, modo(cuatrimestral, Anio1, 2)),
    curso(Estudiante, Materia, _, modo(cuatrimestral, Anio2, 1)),
    Anio2 is Anio1 + 1.
recursaInmediatamente(Estudiante, Materia):-
    curso(Estudiante, Materia, _, modo(cuatrimestral, Anio1, 2)),
    curso(Estudiante, Materia, _, modo(anual, Anio2)),
    Anio2 is Anio1 + 1.
recursaInmediatamente(Estudiante, Materia):-
    curso(Estudiante, Materia, _, modo(anual, Anio1)),
    curso(Estudiante, Materia, _, modo(anual, Anio2)),
    Anio2 is Anio1 + 1.
recursaInmediatamente(Estudiante, Materia):-
    curso(Estudiante, Materia, _, modo(anual, Anio1)),
    curso(Estudiante, Materia, _, modo(cuatrimestral, Anio2, 1)),
    Anio2 is Anio1 + 1.

/* falta agregar las condiciones de cursada de verano */

sinDescanso(Estudiante):-
    estudiante(Estudiante),
    forall(recurso(Estudiante, Materia), recursaInmediatamente(Estudiante, Materia)),
    recurso(Estudiante, Materia).

/* al tratar de aplicarla de forma inversible devuelve los alumnos correctamente pero en muchas repeticiones*/

primerAnioDeCursada(Estudiante, PrimerAnio):-
    anioDeCursada(Estudiante, _, PrimerAnio),
    forall(anioDeCursada(Estudiante, _, Anio), PrimerAnio =< Anio).

ultimoAnioDeCursada(Estudiante, UltimoAnio):-
    anioDeCursada(Estudiante, _, UltimoAnio),
    forall(anioDeCursada(Estudiante, _, Anio), UltimoAnio >= Anio).

cursaEseVerano(Estudiante, Anio):-
    curso(Estudiante, _, _, modo(verano, AnioCalendario)),
    Anio is AnioCalendario - 1.

seLoQueHicisteElVeranoPasado(Estudiante):-
    primerAnioDeCursada(Estudiante, PrimerAnio),
    ultimoAnioDeCursada(Estudiante, UltimoAnio),
    forall(between(PrimerAnio, UltimoAnio, Anio), cursaEseVerano(Estudiante, Anio)).

unicoPerfil(Estudiante):-
    sinDescanso(Estudiante),
    not(invictus(Estudiante)),
    not(repechaje(Estudiante)),
    not(buenasCursadas(Estudiante)),
    not(seLoQueHicisteElVeranoPasado(Estudiante)).
unicoPerfil(Estudiante):-
    not(sinDescanso(Estudiante)),
    invictus(Estudiante),
    not(repechaje(Estudiante)),
    not(buenasCursadas(Estudiante)),
    not(seLoQueHicisteElVeranoPasado(Estudiante)).
unicoPerfil(Estudiante):-
    not(sinDescanso(Estudiante)),
    not(invictus(Estudiante)),
    repechaje(Estudiante),
    not(buenasCursadas(Estudiante)),
    not(seLoQueHicisteElVeranoPasado(Estudiante)).
unicoPerfil(Estudiante):-
    not(sinDescanso(Estudiante)),
    not(invictus(Estudiante)),
    not(repechaje(Estudiante)),
    buenasCursadas(Estudiante),
    not(seLoQueHicisteElVeranoPasado(Estudiante)).
unicoPerfil(Estudiante):-
    not(sinDescanso(Estudiante)),
    not(invictus(Estudiante)),
    not(repechaje(Estudiante)),
    not(buenasCursadas(Estudiante)),
    seLoQueHicisteElVeranoPasado(Estudiante).

esPar(Numero):-
    mod(Numero, 2) is 0.

valorDesempenio(_, modo(verano, Anio), 5):-
    esPar(Anio).
valorDesempenio(Nota, modo(verano, Anio), Desempenio):-
    not(esPar(Anio)),
    Desempenio is Nota / 2.
valorDesempenio(Nota, modo(anual, _), Desempenio):-
    Desempenio is Nota.
valorDesempenio(Nota, modo(cuatrimestral, _, Numero), Desempenio):-
    Desempenio is Nota - Numero.

listaNotas(Estudiante, Notas, CantidadNotas):-
    findall(Nota, curso(Estudiante, _, Nota, _), Notas),
    length(Notas, CantidadNotas).

listaModos(Estudiante, Modos, CantidadModos):-
    findall(Modo, curso(Estudiante, _, _, Modo), Modos),
    length(Modos, CantidadModos).

indiceDesempenio(Estudiante, Notas, Modos, Indice, ResultadoDesempenio):-
    nth1(Modos, Indice, Modo),
    nth1(Notas, Indice, Nota),
    valorDesempenio(Nota, Modo, ResultadoDesempenio).

/*
listaDesempenios(Estudiante):-
    listaNotas(Estudiante, Notas, CantidadNotas),
    listaModos(Estudiante, Modos, CantidadModos),
    findall()
    Como seguir?
*/