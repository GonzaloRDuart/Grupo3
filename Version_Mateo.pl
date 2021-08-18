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

estudiante(vero).
estudiante(alan).
estudiante(lucio).
integradora(sistemasYOrganizaciones).
integradora(analisisDeSistemas).
integradora(disenioDeSistemas).
integradora(administracionDeRecursos).
integradora(proyectoFinal).


correlativas(analisisDeSistemas,cursadas(sistemasYOrganizaciones,algoritmos)).
correlativas(analisisMatematicoII,cursadas(analisisMatematicoI,algebra)).
correlativas(sintaxis,cursadas(matematicaDiscreta,algoritmos)).
correlativas(pdep,cursadas(matematicaDiscreta,algoritmos)).
correlativas(proba,cursadas(analisisMatematicoI,algebra)).
correlativas(disenioDeSistemas,cursadas(analisisDeSistemas,pdep)).
correlativas(sistemasOperativos,cursadas(matematicaDiscreta,algoritmos,arquitectura)).
correlativas(fisicaII,cursadas(analisisMatematicoI,fisicaI)).
correlativas(economia,cursadas(analisisDeSistemas)).
correlativas(gestionDeDatos,cursadas(analisisDeSistemas,pdep,sintaxis)).
correlativas(inglesII,cursadas(inglesI)).
correlativas(matematicaSuperior,cursadas(analisisMatematicoII)).
correlativas(legislacion,cursadas(analisisDeSistemas,ingenieriaYSociedad)).
correlativas(administracionDeRecursos,cursadas(disenioDeSistemas,sistemasOperativos,economia)).
correlativas(ingenieriaDeSoftware,cursadas(proba,disenioDeSistemas,gestionDeDatos)).
correlativas(teoriaDeControl,cursadas(quimica,matematicaSuperior)).
correlativas(comunicaciones,cursadas(arquitectura,analisisMatematicoII,fisicaII)).
correlativas(redesDeInformacion,cursadas(sistemasOperativos,comunicaciones)).
correlativas(investigacionOperativa,cursadas(proba,matematicaSuperior)).
correlativas(simulacion,cursadas(proba,matematicaSuperior)).
correlativas(inteligenciaArtificial,cursadas(investigacionOperativa,simulacion)).
correlativas(administracionGerencial,cursadas(administracionDeRecursos,investigacionOperativa)).
correlativas(sistemasDeGestion,cursadas(administracionDeRecursos,investigacionOperativa,simulacion)).
correlativas(proyectoFinal,cursadas(legislacion,administracionDeRecursos,redesDeInformacion,ingenieriaDeSoftware)).

esPesada(Materia):-
    materia(Materia,Horas), Horas>4.
esPesada(Materia):-
    integradora(Materia),
    materia(Materia,Horas), 
    Horas>6.

esInicial(Materia):-
    materia(Materia,_),not(correlativas(Materia,_)).

paraCursar(Materia,Correlativas):-
    materia(Materia,_),correlativas(Materia,cursadas(Correlativas)).

/* paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas3(Materia1,Materia2,Materia3,Materia4)),correlativas(Materia1),correlativas(Materia2).

paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas1(Materia1,Materia2)),correlativas(Materia1),correlativas(Materia2).

paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas4(Materia1,Materia2)),correlativas(Materia1),correlativas(Materia2). */

%Los alumnos

aprobo(Alguien,Materia):-
    promociono(Alguien,Materia).

aprobo(Alguien,Materia):-
  not(promociono(Alguien,Materia)),final(Alguien,Materia,Nota),Nota>6. %el not promociono creo que se puede sacar

final(vero,inglesII,10).
final(alan,sistemasYOrganizaciones,4).
final(alan,inglesI,2).

cursada(Alguien,Materia):-
    curso(Alguien,Materia,Nota,_),Nota>=6.

curso(vero,Materia,8,modalidad(2018,anual)):-
    esInicial(Materia).

curso(alan,sistemasYOrganizaciones,6,modalidad(2018,anual)).
curso(alan,analisisMatematicoI,6,modalidad(2018,anual)).
curso(alan,analisisDeSistemas,2,modalidad(2019,anual)). %no las curso, porque no aprobo la cursada, tengo que ver que hago con esta y fisica 1
curso(alan,analisisDeSistemas,9,modalidad(2020,anual)).
curso(alan,fisicaI,2,modalidad(2018,anual)).
curso(mateo,sistemasYOrganizaciones,6,modalidad(2015,anual)).
curso(mateo,quimica,3,modalidad(2015,primerCuatrimestre)).
curso(mateo,quimica,6,modalidad(2015,segundoCuatrimestre)).
curso(mateo,fisicaI,6,modalidad(2015,cursoDeVerano)).
curso(julian,quimica,2,modalidad(2016,anual)).
curso(julian,quimica,3,modalidad(2017,primerCuatrimestre)).
curso(julian,quimica,4,modalidad(2017,segundoCuatrimestre)).
curso(julian,quimica,5,modalidad(2018,primerCuatrimestre)).
curso(julian,fisicaI,2,modalidad(2018,cursoDeVerano)).
curso(julian,fisicaI,2,modalidad(2018,anual)).
curso(lucio,quimica,2,modalidad(2016,anual)).
curso(lucio,quimica,3,modalidad(2017,segundoCuatrimestre)).
curso(lucio,fisicaI,2,modalidad(2017,anual)).
curso(lucio,fisicaI,10,modalidad(2018,primerCuatrimestre)).
curso(milton,analisisMatematicoI,8,modalidad(2017,cursoDeVerano)).
curso(milton,analisisMatematicoII,9,modalidad(2018,cursoDeVerano)).
promociono(Alguien,Materia):-
    curso(Alguien,Materia,Nota,_), Nota > 7.

recurso(Estudiante,Materia):-
    materia(Materia,_),curso(Estudiante,Materia,_,modalidad(UnAnio,_)) ,curso(Estudiante,Materia,_,modalidad(OtroAnio,_)), UnAnio \= OtroAnio.

recurso(Estudiante,Materia):-
    materia(Materia,_),curso(Estudiante,Materia,_,modalidad(Anio,Modalidad)) ,curso(Estudiante,Materia,_,modalidad(Anio,OtraModalidad)), Modalidad \= OtraModalidad. 

%Perfiles

sinDescanso(Estudiante):-
    recurso(Estudiante,_),
    forall((curso(Estudiante,Materia,_,modalidad(Anio,UnaModalidad)),(curso(Estudiante,Materia,_,modalidad(UnAnio,OtraModalidad)))),(Anio==UnAnio,OtraModalidad\=UnaModalidad)). 

invictus(Estudiante):-
    not(recurso(Estudiante,_)).

repechaje(Estudiante):-
    curso(Estudiante,Materia,Nota,modalidad(Anio,anual)),curso(Estudiante,Materia,_,modalidad(OtroAnio,primerCuatrimestre)),promociono(Estudiante,Materia),Nota<6,OtroAnio is Anio+1.

buenasCursadas(Estudiante):-%como la hago inversible
    estudiante(Estudiante),
    forall(curso(Estudiante,Materia,_,_),promociono(Estudiante,Materia)).

veraniego(Estudiante):-
    estudiante(Estudiante),
    forall(curso(Estudiante,_,_,modalidad(Anio,_),curso(Estudiante,_,_,modalidad(Anio,cursoDeVerano)))).