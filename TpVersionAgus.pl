/* IMPORTANTE:
Este programa contiene consultas que generan listas, las cuales pueden llegar a ser extensas. 
Antes de generar cualquier consulta, recomendamos fuertemente ingresar en consola el siguiente comando:

set_prolog_flag(answer_write_options,[max_depth(0)]).

Esto permite que la consola muestre las listas completas en vez de, por defecto, respetar un limite de elementos.  */

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
/* El primer parametro corresponde al nombre de la materia. El segundo parametro a la cantidad de horas semanales de cursada. El tercero a si es integradora o no */

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
/* Indica que para cursar la materia pasada como primer parametro, antes es necesario cursar la materia pasada como segundo parametro */

habilitaA(Materia, PosMateria):-
    esNecesaria(PosMateria, Materia).
/* Toma como parametros 2 materias e indica si la segunda materia es necesaria para cursar la primer materia */

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

listaMateriasConCorrelativas(Materias):-
    findall(Materia, tieneCorrelativa(Materia), Materias1),
    list_to_set(Materias1, Materias).
/* Su parametro corresponde a la lista de todas las materias con correlativas */

esInicial(Materia):-
    materia(Materia, _, _),
    not(tieneCorrelativa(Materia)).

tambienNecesito(Materia, SubMateria):-
    esNecesaria(Materia, SubMateria).
tambienNecesito(Materia, SubMateria):-
    esNecesaria(Materia, X),
    tambienNecesito(X, SubMateria).
/*Toma como parametro 2 materias e indica si para cursar la primera necesito la segunda, ya sea directa o indirectamente */

listaNecesarias(Materia, SubMaterias):-
    findall(SubMateria, tambienNecesito(Materia, SubMateria), SubMaterias1),
    list_to_set(SubMaterias1, SubMaterias).
/* Indica si el segundo parametro es la lista de materias necesarias, tanto directa como indirectamente, para cursar la materia pasada en el primer parametro.*/

habilitaMaterias(Materia, Posterior):-
    habilitaA(Materia, Posterior).
habilitaMaterias(Materia, Posterior):-
    habilitaA(Materia, X),
    habilitaMaterias(X, Posterior).
/* Toma como parametro 2 materias, e indica si cursar la primer materia es condicion para cursar la segunda */

listaHabilitadas(Materia, Habilitadas):-
    findall(Habilitada, habilitaMaterias(Materia, Habilitada), Habilitadas1),
    list_to_set(Habilitadas1, Habilitadas).
/* Indica si el segundo parametro es la lista de materias que habilita, tanto directa como indirectamente, cursar la materia pasada en el primer parametro.*/

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
curso(veraniego, quimica, 6, modo(anual, 2016)).
curso(veraniego, fisica1, 6, modo(verano, 2017)).
curso(veraniego, matematicaDiscreta, 2, modo(anual, 2017)).
curso(veraniego, matematicaDiscreta, 8, modo(verano, 2018)).
curso(muchasRecursadas, quimica, 2, modo(anual, 2016)).
curso(muchasRecursadas, quimica, 3, modo(cuatrimestral, 2017, 1)).
curso(muchasRecursadas, quimica, 4, modo(cuatrimestral, 2017, 2)).
curso(muchasRecursadas, quimica, 5, modo(anual, 2018)).
curso(muchasRecursadas, fisica1, 2, modo(verano, 2018)).
curso(muchasRecursadas, fisica1, 2, modo(anual, 2018)).
curso(algunasRecursadas, quimica, 2, modo(anual, 2016)).
curso(algunasRecursadas, quimica, 3, modo(cuatrimestral, 2017, 2)).
curso(algunasRecursadas, fisica1, 2, modo(anual, 2017)).
curso(algunasRecursadas, fisica1, 10, modo(cuatrimestral, 2018, 1)).
curso(atr, quimica, 10, modo(cuatrimestral, 2016, 1)).
curso(atr, fisica1, 10, modo(cuatrimestral, 2016, 2)).
final(alan, sistemasYOrganizaciones, 4).
final(alan, ingles1, 2).
final(vero, ingles2, 10).

cursadaAprobada(Alumno, Materia):-
    curso(Alumno, Materia, Nota, _),
    Nota >= 6.
cursadaAprobada(Alumno, Materia):-
    final(Alumno, Materia, Nota),
    Nota >= 6.

materiaAprobada(Alumno, Materia):-
    curso(Alumno, Materia, Nota, _),
    Nota > 7.
materiaAprobada(Alumno, Materia):-
    final(Alumno, Materia, Nota),
    Nota >= 6.

anioDeCursada(Alumno, Materia, Anio):-
    curso(Alumno, Materia, _, modo(anual, Anio)).
anioDeCursada(Alumno, Materia, Anio):-
    curso(Alumno, Materia, _, modo(cuatrimestral, Anio, _)).
anioDeCursada(Alumno, Materia, Anio):-
    curso(Alumno, Materia, _, modo(verano, AnioCalendario)),
    Anio is AnioCalendario - 1.
/* Determina si el alumno mencionado en el primer parametro curso la materia pasada como segundo parametro en el a??o pasado como tercer parametro */

listaAniosDeCursada(Alumno, Materia, Anios):-
    findall(Anio, anioDeCursada(Alumno, Materia, Anio), Anios2),
    list_to_ord_set(Anios2, Anios).
/* Verifica que el tercer parametro corresponda a una lista de todos los a??os en los cuales el alumno del primer parametro curso la materia pasada como segundo parametro. 
Especialmente util para recursantes */

recurso(Alumno, Materia):-
    curso(Alumno, Materia, _, Cursada1),
    curso(Alumno, Materia, _, Cursada2),
    Cursada1 \= Cursada2.

listaRecursadas(Alumno, Materias):-
    findall(Materia, recurso(Alumno, Materia), Materias1),
    list_to_set(Materias1, Materias).
/* Verifica si el segundo parametro corresponde a la lista de todas las materias que recurso el alumno pasado como primer parametro.
Utilizar si se conoce el alumno. */

estudiante(naruto).
estudiante(vero).
estudiante(alan).
estudiante(veraniego).
estudiante(muchasRecursadas).
estudiante(atr).
estudiante(algunasRecursadas).

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


/* A continuaci??n, funcion auxiliar que determina los casos en los cuales se considera que un alumno (1er parametro) recurso de inmediato una materia (2do parametro) */
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
recursaInmediatamente(Estudiante, Materia):-
    curso(Estudiante, Materia, _, modo(verano, Anio1)),
    curso(Estudiante, Materia, _, modo(anual, Anio2)),
    Anio2 = Anio1.
recursaInmediatamente(Estudiante, Materia):-
    curso(Estudiante, Materia, _, modo(anual, Anio1)),
    curso(Estudiante, Materia, _, modo(cuatrimestral, Anio2, 1)),
    Anio2 = Anio1.
/* ----------------------------------------------------------------------------------------------------------------------------------------------------- */

sinDescanso(Estudiante):-
    estudiante(Estudiante),
    forall(recurso(Estudiante, Materia), recursaInmediatamente(Estudiante, Materia)),
    recurso(Estudiante, Materia).

listaEstudiantesSinDescanso(Estudiantes):-
    findall(Estudiante, sinDescanso(Estudiante), Estudiantes1),
    list_to_set(Estudiantes1, Estudiantes).
/*Su unico parametro es la lista de estudiantes que cumplen con el perfil de Sin Descanso */

/* A continuaci??n, funciones auxiliares para seLoQueHicisteElVeranoPasado */
primerAnioDeCursada(Estudiante, PrimerAnio):-
    anioDeCursada(Estudiante, _, PrimerAnio),
    forall(anioDeCursada(Estudiante, _, Anio), PrimerAnio =< Anio).

ultimoAnioDeCursada(Estudiante, UltimoAnio):-
    anioDeCursada(Estudiante, _, UltimoAnio),
    forall(anioDeCursada(Estudiante, _, Anio), UltimoAnio >= Anio).

cursaEseVerano(Estudiante, Anio):-
    curso(Estudiante, _, _, modo(verano, AnioCalendario)),
    Anio is AnioCalendario - 1.
/* --------------------------------------------------------------------- */

seLoQueHicisteElVeranoPasado(Estudiante):-
    primerAnioDeCursada(Estudiante, PrimerAnio),
    ultimoAnioDeCursada(Estudiante, UltimoAnio),
    forall(between(PrimerAnio, UltimoAnio, Anio), cursaEseVerano(Estudiante, Anio)).

listaSeLoQueHicisteElVeranoPasado(Estudiantes):-
    findall(Estudiante, seLoQueHicisteElVeranoPasado(Estudiante), Estudiantes1),
    list_to_set(Estudiantes1, Estudiantes).

/* Su unico parametro es la lista de estudiantes que cumple con el perfil se lo que hiciste el verano pasado */


/* Funcion Auxiliar que determina si un alumno tiene un perfil */
tienePerfil(Alumno, sinDescanso):-
    sinDescanso(Alumno).
tienePerfil(Alumno, seLoQueHicisteElVeranoPasado):-
    seLoQueHicisteElVeranoPasado(Alumno).
tienePerfil(Alumno, repechaje):-
    repechaje(Alumno).
tienePerfil(Alumno, buenasCursadas):-
    buenasCursadas(Alumno).
tienePerfil(Alumno, invictus):-
    invictus(Alumno).
/* ---------------------------------------------------------  */

perfilUnico(Alumno):-
    estudiante(Alumno),
    findall(Perfil, tienePerfil(Alumno, Perfil), Conjunto1),
    list_to_set(Conjunto1, Conjunto2),
    length(Conjunto2, Cantidad),
    Cantidad =:= 1.


/* Funciones Auxiliares para determinar el desempe??o academico de un estudiante */
esPar(N):- 
    mod(N,2) =:= 0.

indice(Alumno, Materia, Nota):-
    curso(Alumno, Materia, Nota, modo(anual, _)).
indice(Alumno, Materia, Nota1):-
    curso(Alumno, Materia, Nota2, modo(cuatrimestral, _, Cuatrimestre)),
    Nota1 is Nota2 - Cuatrimestre.
indice(Alumno, Materia, 5):-
    curso(Alumno, Materia, _, modo(verano, Anio)),
    esPar(Anio).
indice(Alumno, Materia, Nota1):-
    curso(Alumno , Materia, Nota2, modo(verano, Anio)),
    not(esPar(Anio)),
    Nota1 is Nota2 // 2.
/* ----------------------------------------------------------------------------- */

desempenioAcademico(Alumno, Promedio):-
    estudiante(Alumno),
    findall(Nota, indice(Alumno, _, Nota), Notas),
    length(Notas, Cantidad),
    sum_list(Notas, Total),
    Promedio is Total // Cantidad.