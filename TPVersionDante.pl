<<<<<<< HEAD
materia(analisisMatematicoI,5).
materia(algebra,5).
materia(matematicaDiscreta,3).
materia(sistemasYOrganizaciones,3).
materia(algoritmos,5).
materia(arquitectura,4).
materia(ingenieriaYSociedad,2).
materia(quimica,3).
materia(fisicaI,5).
materia(analisisMatematicoII,5).
materia(proba,3).
materia(analisisDeSistemas,6).
materia(sintaxis,4).
materia(pdep,4).
materia(inglesI,2).
materia(sistemasDeRepresentacion,3).
materia(sistemasOperativos,4).
materia(disenioDeSistemas,6).
materia(fisicaII,5).
materia(matematicaSuperior,4).
materia(gestionDeDatos,4).
materia(legislacion,2).
materia(economia,3).
materia(inglesII,2).
materia(redesDeInformacion,4).
materia(administracionDeRecursos,6).
materia(investigacionOperativa,5).
materia(simulacion,4).
materia(ingenieriaDeSoftware,3).
materia(teoriaDeControl,3).
materia(comunicaciones,4).
materia(proyectoFinal,6).
materia(inteligenciaArtificial,3).
materia(administracionGerencial,3).
materia(sistemasDeGestion,4).


integradora(sistemasYOrganizaciones).
integradora(analisisDeSistemas).
integradora(disenioDeSistemas).
integradora(administracionDeRecursos).
integradora(proyectoFinal).


correlativa(analisisDeSistemas, sistemasYOrganizaciones).
correlativa(analisisDeSistemas, algoritmos).
correlativa(analisisMatematicoII, analisisMatematicoI).
correlativa(analisisMatematicoII, algebra).
correlativa(sintaxis, matematicaDiscreta).
correlativa(sintaxis, algoritmos).
correlativa(pdep, matematicaDiscreta).
correlativa(pdep, algoritmos).
correlativa(proba, analisisMatematicoI).
correlativa(proba, algebra).
correlativa(disenioDeSistemas, analisisDeSistemas).
correlativa(disenioDeSistemas, pdep).
correlativa(sistemasOperativos, matematicaDiscreta).
correlativa(sistemasOperativos, algoritmos).
correlativa(sistemasOperativos, arquitectura).
correlativa(fisicaII, analisisMatematicoI).
correlativa(fisicaII, fisicaI).
correlativa(economia, analisisDeSistemas).
correlativa(gestionDeDatos, analisisDeSistemas).
correlativa(gestionDeDatos, pdep).
correlativa(gestionDeDatos, sintaxis).
correlativa(inglesII, inglesI).
correlativa(matematicaSuperior, analisisMatematicoII).
correlativa(legislacion, analisisDeSistemas).
correlativa(legislacion, ingenieriaYSociedad).
correlativa(administracionDeRecursos, disenioDeSistemas).
correlativa(administracionDeRecursos, sistemasOperativos).
correlativa(ingenieriaDeSoftware, proba).
correlativa(ingenieriaDeSoftware, disenioDeSistemas).
correlativa(ingenieriaDeSoftware, gestionDeDatos).
correlativa(teoriaDeControl, quimica).
correlativa(teoriaDeControl, matematicaSuperior).
correlativa(comunicaciones, arquitectura).
correlativa(comunicaciones, analisisMatematicoII).
correlativa(comunicaciones, fisicaII).
correlativa(redesDeInformacion, sistemasOperativos).
correlativa(redesDeInformacion, comunicaciones).
correlativa(investigacionOperativa, proba).
correlativa(investigacionOperativa, matematicaSuperior).
correlativa(simulacion, proba).
correlativa(simulacion, matematicaSuperior).
correlativa(inteligenciaArtificial, simulacion).
correlativa(inteligenciaArtificial, investigacionOperativa).
correlativa(administracionGerencial, administracionDeRecursos).
correlativa(administracionGerencial, investigacionOperativa).
correlativa(sistemasDeGestion, administracionDeRecursos).
correlativa(sistemasDeGestion, investigacionOperativa).
correlativa(sistemasDeGestion, simulacion).
correlativa(proyectoFinal, legislacion).
correlativa(proyectoFinal, administracionDeRecursos).
correlativa(proyectoFinal, redesDeInformacion).
correlativa(proyectoFinal, ingenieriaDeSoftware).



esPesada(Materia):-
    materia(Materia, Horas),
    integradora(Materia),
    Horas >= 6.

esPesada(Materia):-
    materia(Materia, Horas),
    not(integradora(Materia)),
    Horas >= 4.

esInicial(Materia):-
    materia(Materia, _),
    not(correlativa(Materia,_)).

necesariaParaCursar(Materia, Requerida):-
    materia(Materia, _),
    correlativa(Materia, Requerida).

habilita(Materia, Habilitada):-
    materia(Materia, _),
    correlativa(Habilitada, Materia).

/*estudiantes*/

alumno(vero, curso(Materia, 8)):-
    esInicial(Materia).

alumno(vero, rindio(inglesII, 10)).

alumno(alan, curso(sistemasYOrganizaciones, 6)).
alumno(alan, curso(analisisMatematicoI, 6)).
alumno(alan, curso(analisisDeSistemas,2)).
alumno(alan, curso(analisisDeSistemas, 9)).
alumno(alan, curso(fisica, 2)).
alumno(alan, rindio(sistemasYOrganizaciones, 4)).
alumno(alan, rindioLibre(inglesI, 2)).

aprueba(Materia, rindio(Materia, Nota)):-
    Nota > 5.
aprueba(Materia, rindioLibre(Materia, Nota)):-
    Nota > 5.
aprueba(Materia, curso(Materia, Nota)):-
    Nota > 7.

firmo(Materia, curso(Materia, Nota)):-
    Nota > 5.
firmo(Materia, Detalles):-
    aprueba(Materia, Detalles).

curso(Alumno, Materia):-
    alumno(Alumno, Detalles),
    firmo(Materia, Detalles). 

aprobo(Alumno, Materia):-
    alumno(Alumno, Detalles),
    aprueba(Materia, Detalles).

/*modalidades*/

cursada(elias, sistemasYOrganizaciones, anual(2015)).
cursada(elias, quimica, cuatrimestral(2015, primer)).
cursada(elias, quimica, cuatrimestral(2015, segundo)).
cursada(elias, fisicaI, verano(2016)).

reCurso(Alumno, Materia):-
    cursada(Alumno, Materia, Fecha1),
    cursada(Alumno, Materia, Fecha2),
    Fecha1 \= Fecha2.
    
/* perfiles de estudiantes*/

sinDescanso(Alumno, Materia):-
    cursada(Alumno, Materia, cuatrimestral(Anio, primer)),
    cursada(Alumno, Materia, cuatrimestral(Anio, segundo)).

sinDescanso(Alumno, Materia):-
    cursada(Alumno, Materia, cuatrimestral(Anio1, segundo)),
    cursada(Alumno, Materia, anual(Anio2)),
    Anio2 is Anio1 + 1.

sinDescanso(Alumno, Materia):-
    cursada(Alumno, Materia, cuatrimestral(Anio1, segundo)),
    cursada(Alumno, Materia, cuatrimestral(Anio2, primer)),
    Anio2 is Anio1 + 1.



invictus(Alumno):-  
    not(reCurso(Alumno, _)).

repechaje(Alumno):-
    not(aprobo(Alumno, Materia)).

buenasCursadas(Alumno):-
    forall(cursada(Alumno, Materia, _), aprobo(Alumno, Materia)).
=======
materia(analisisMatematicoI,5).
materia(algebra,5).
materia(matematicaDiscreta,3).
materia(sistemasYOrganizaciones,3).
materia(algoritmos,5).
materia(arquitectura,4).
materia(ingenieriaYSociedad,2).
materia(quimica,3).
materia(fisicaI,5).
materia(analisisMatematicoII,5).
materia(proba,3).
materia(analisisDeSistemas,6).
materia(sintaxis,4).
materia(pdep,4).
materia(inglesI,2).
materia(sistemasDeRepresentacion,3).
materia(sistemasOperativos,4).
materia(disenioDeSistemas,6).
materia(fisicaII,5).
materia(matematicaSuperior,4).
materia(gestionDeDatos,4).
materia(legislacion,2).
materia(economia,3).
materia(inglesII,2).
materia(redesDeInformacion,4).
materia(administracionDeRecursos,6).
materia(investigacionOperativa,5).
materia(simulacion,4).
materia(ingenieriaDeSoftware,3).
materia(teoriaDeControl,3).
materia(comunicaciones,4).
materia(proyectoFinal,6).
materia(inteligenciaArtificial,3).
materia(administracionGerencial,3).
materia(sistemasDeGestion,4).


integradora(sistemasYOrganizaciones).
integradora(analisisDeSistemas).
integradora(disenioDeSistemas).
integradora(administracionDeRecursos).
integradora(proyectoFinal).


correlativa(analisisDeSistemas, sistemasYOrganizaciones).
correlativa(analisisDeSistemas, algoritmos).
correlativa(analisisMatematicoII, analisisMatematicoI).
correlativa(analisisMatematicoII, algebra).
correlativa(sintaxis, matematicaDiscreta).
correlativa(sintaxis, algoritmos).
correlativa(pdep, matematicaDiscreta).
correlativa(pdep, algoritmos).
correlativa(proba, analisisMatematicoI).
correlativa(proba, algebra).
correlativa(disenioDeSistemas, analisisDeSistemas).
correlativa(disenioDeSistemas, pdep).
correlativa(sistemasOperativos, matematicaDiscreta).
correlativa(sistemasOperativos, algoritmos).
correlativa(sistemasOperativos, arquitectura).
correlativa(fisicaII, analisisMatematicoI).
correlativa(fisicaII, fisicaI).
correlativa(economia, analisisDeSistemas).
correlativa(gestionDeDatos, analisisDeSistemas).
correlativa(gestionDeDatos, pdep).
correlativa(gestionDeDatos, sintaxis).
correlativa(inglesII, inglesI).
correlativa(matematicaSuperior, analisisMatematicoII).
correlativa(legislacion, analisisDeSistemas).
correlativa(legislacion, ingenieriaYSociedad).
correlativa(administracionDeRecursos, disenioDeSistemas).
correlativa(administracionDeRecursos, sistemasOperativos).
correlativa(ingenieriaDeSoftware, proba).
correlativa(ingenieriaDeSoftware, disenioDeSistemas).
correlativa(ingenieriaDeSoftware, gestionDeDatos).
correlativa(teoriaDeControl, quimica).
correlativa(teoriaDeControl, matematicaSuperior).
correlativa(comunicaciones, arquitectura).
correlativa(comunicaciones, analisisMatematicoII).
correlativa(comunicaciones, fisicaII).
correlativa(redesDeInformacion, sistemasOperativos).
correlativa(redesDeInformacion, comunicaciones).
correlativa(investigacionOperativa, proba).
correlativa(investigacionOperativa, matematicaSuperior).
correlativa(simulacion, proba).
correlativa(simulacion, matematicaSuperior).
correlativa(inteligenciaArtificial, simulacion).
correlativa(inteligenciaArtificial, investigacionOperativa).
correlativa(administracionGerencial, administracionDeRecursos).
correlativa(administracionGerencial, investigacionOperativa).
correlativa(sistemasDeGestion, administracionDeRecursos).
correlativa(sistemasDeGestion, investigacionOperativa).
correlativa(sistemasDeGestion, simulacion).
correlativa(proyectoFinal, legislacion).
correlativa(proyectoFinal, administracionDeRecursos).
correlativa(proyectoFinal, redesDeInformacion).
correlativa(proyectoFinal, ingenieriaDeSoftware).



esPesada(Materia):-
    materia(Materia, Horas),
    integradora(Materia),
    Horas >= 6.

esPesada(Materia):-
    materia(Materia, Horas),
    not(integradora(Materia)),
    Horas >= 4.

esInicial(Materia):-
    materia(Materia, _),
    not(correlativa(Materia,_)).

necesariaParaCursar(Materia, Requerida):-
    materia(Materia, _),
    correlativa(Materia, Requerida).

habilita(Materia, Habilitada):-
    materia(Materia, _),
    correlativa(Habilitada, Materia).

/*estudiantes*/

alumno(vero, curso(Materia, 8)):-
    esInicial(Materia).

alumno(vero, rindio(inglesII, 10)).

alumno(alan, curso(sistemasYOrganizaciones, 6)).
alumno(alan, curso(analisisMatematicoI, 6)).
alumno(alan, curso(analisisDeSistemas,2)).
alumno(alan, curso(analisisDeSistemas, 9)).
alumno(alan, curso(fisica, 2)).
alumno(alan, rindio(sistemasYOrganizaciones, 4)).
alumno(alan, rindioLibre(inglesI, 2)).

aprueba(Materia, rindio(Materia, Nota)):-
    Nota > 5.
aprueba(Materia, rindioLibre(Materia, Nota)):-
    Nota > 5.
aprueba(Materia, curso(Materia, Nota)):-
    Nota > 7.

firmo(Materia, curso(Materia, Nota)):-
    Nota > 5.
firmo(Materia, Detalles):-
    aprueba(Materia, Detalles).

curso(Alumno, Materia):-
    alumno(Alumno, Detalles),
    firmo(Materia, Detalles). 

aprobo(Alumno, Materia):-
    alumno(Alumno, Detalles),
    aprueba(Materia, Detalles).

/*modalidades*/

cursada(elias, sistemasYOrganizaciones, anual(2015)).
cursada(elias, quimica, cuatrimestral(2015, primer)).
cursada(elias, quimica, cuatrimestral(2015, segundo)).
cursada(elias, fisicaI, verano(2016)).

reCurso(Alumno, Materia):-
    cursada(Alumno, Materia, Fecha1),
    cursada(Alumno, Materia, Fecha2),
    Fecha1 \= Fecha2.
    
/* perfiles de estudiantes*/

sinDescanso(Alumno, Materia):-
    cursada(Alumno, Materia, cuatrimestral(Anio, primer)),
    cursada(Alumno, Materia, cuatrimestral(Anio, segundo)).

sinDescanso(Alumno, Materia):-
    cursada(Alumno, Materia, cuatrimestral(Anio1, segundo)),
    cursada(Alumno, Materia, anual(Anio2)),
    Anio2 is Anio1 + 1.

sinDescanso(Alumno, Materia):-
    cursada(Alumno, Materia, cuatrimestral(Anio1, segundo)),
    cursada(Alumno, Materia, cuatrimestral(Anio2, primer)),
    Anio2 is Anio1 + 1.



invictus(Alumno):-  
    not(reCurso(Alumno, _)).

repechaje(Alumno):-
    not(aprobo(Alumno, Materia)).

buenasCursadas(Alumno):-
    forall(cursada(Alumno, Materia, _), aprobo(Alumno, Materia)).
>>>>>>> c84c7741e236f5e27a6866b3525b8de9d39024a5
