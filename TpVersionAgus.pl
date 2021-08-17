
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

curso(vero, Materia, 8):-
    esInicial(Materia).
final(vero, ingles2, 10).

curso(alan, sistemasYOrganizaciones, 6).
curso(alan, analisisMatematico1, 6).
curso(alan, analisisDeSistemas, 2).
curso(alan, analisisDeSistemas, 9).
curso(alan, fisica1, 2).
final(alan, sistemasYOrganizaciones, 4).
final(alan, ingles1, 2).


cursada(Alumno, Materia):-
    curso(Alumno, Materia, Nota),
    Nota >= 6.
cursada(Alumno, Materia):-
    final(Alumno, Materia, Nota),
    Nota >= 6.

aprobada(Alumno, Materia):-
    curso(Alumno, Materia, Nota),
    Nota > 7.
aprobada(Alumno, Materia):-
    final(Alumno, Materia, Nota),
    Nota >= 6.

