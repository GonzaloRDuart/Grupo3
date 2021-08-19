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
estudiante(milton). 

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

/* correlativas(analisisDeSistemas,cursadas(sistemasYOrganizaciones,algoritmos)).
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
 */
esPesada(Materia):-
    materia(Materia,Horas), Horas>4.

esPesada(Materia):-
    integradora(Materia),
    materia(Materia,Horas), 
    Horas>6.

esInicial(Materia):-
    materia(Materia,_),
    not(correlativa(Materia,_)).

paraCursar(Materia,Correlativas):-
    materia(Materia,_),
    correlativa(Materia,cursadas(Correlativas)).

/* paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas3(Materia1,Materia2,Materia3,Materia4)),correlativas(Materia1),correlativas(Materia2).

paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas1(Materia1,Materia2)),correlativas(Materia1),correlativas(Materia2).

paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas4(Materia1,Materia2)),correlativas(Materia1),correlativas(Materia2). */

%Los alumnos

final(vero,inglesII,10).
final(alan,sistemasYOrganizaciones,4).
final(alan,inglesI,2).

aprobo(Alguien,Materia):-
    promociono(Alguien,Materia).

aprobo(Alguien,Materia):-
  not(promociono(Alguien,Materia)),
  final(Alguien,Materia,Nota),Nota>6. %el not promociono creo que se puede sacar


cursada(Alguien,Materia):-
    curso(Alguien,Materia,Nota,_,_),
    Nota>=6.

curso(vero,Materia,8,2018,anual):-
    esInicial(Materia).

curso(alan,sistemasYOrganizaciones,6,2018,anual).
curso(alan,analisisMatematicoI,6,2018,anual).
curso(alan,analisisDeSistemas,2,2019,anual). %no las curso, porque no aprobo la cursada, tengo que ver que hago con esta y fisica 1
curso(alan,analisisDeSistemas,9,2020,anual).
curso(alan,fisicaI,2,2018,anual).
curso(mateo,sistemasYOrganizaciones,6,2015,anual).
curso(mateo,quimica,3,2015,primerCuatrimestre).
curso(mateo,quimica,6,2015,segundoCuatrimestre).
curso(mateo,fisicaI,6,2015,cursoDeVerano).
curso(julian,quimica,2,2016,anual).
curso(julian,quimica,3,2017,primerCuatrimestre).
curso(julian,quimica,4,2017,segundoCuatrimestre).
curso(julian,quimica,5,2018,primerCuatrimestre).
curso(julian,fisicaI,2,2018,cursoDeVerano).
curso(julian,fisicaI,2,2018,anual).
curso(lucio,quimica,2,2016,anual).
curso(lucio,quimica,3,2017,segundoCuatrimestre).
curso(lucio,fisicaI,2,2017,anual).
curso(lucio,fisicaI,10,2018,primerCuatrimestre).
curso(milton,fisicaI,7,2017,primerCuatrimestre).
curso(milton,analisisMatematicoI,8,2017,cursoDeVerano).
curso(milton,analisisMatematicoII,9,2018,cursoDeVerano).

promociono(Alguien,Materia):-
    curso(Alguien,Materia,Nota,_,_), 
    Nota > 7.

cuandoCurso(Estudiante,Materia,Anio):-
    curso(Estudiante,Materia,_,Anio,_).

cuandoCurso(Estudiante,Materia,Anio):-
    curso(Estudiante,Materia,_,Anio,cursoDeVerano), Anio is Anio-1.

recurso(Estudiante,Materia):-
    materia(Materia,_),
    curso(Estudiante,Materia,_,UnAnio,_) ,
    curso(Estudiante,Materia,_,OtroAnio,_), 
    UnAnio \= OtroAnio.

recurso(Estudiante,Materia):-
    materia(Materia,_),
    curso(Estudiante,Materia,_,Anio,Modalidad),
    curso(Estudiante,Materia,_,Anio,OtraModalidad), 
    Modalidad \= OtraModalidad. 

    recurso(Estudiante,Materia):-
        materia(Materia,_),
        curso(Estudiante,Materia,_,Anio,Modalidad) ,
        curso(Estudiante,Materia,_,Anio,OtraModalidad), 
        Modalidad \= OtraModalidad. 
%Perfiles

sinDescanso(Estudiante):-
    estudiante(Estudiante),
 /*    recurso(Estudiante,_), */
    forall(
        (curso(Estudiante,Materia,_,Anio,UnaModalidad),
        curso(Estudiante,Materia,_,UnAnio,OtraModalidad)),
        (UnAnio == Anio ,OtraModalidad\=UnaModalidad)). 

invictus(Estudiante):-
    estudiante(Estudiante),
    not(recurso(Estudiante,_)).

repechaje(Estudiante):-
    curso(Estudiante,Materia,Nota,Anio,anual),
    curso(Estudiante,Materia,_,OtroAnio,primerCuatrimestre),
    promociono(Estudiante,Materia),
    Nota<6,OtroAnio is Anio+1.

buenasCursadas(Estudiante):-%como la hago inversible
    estudiante(Estudiante),
    forall(curso(Estudiante,Materia,_,_,_),
    promociono(Estudiante,Materia)).

veraniego(Estudiante):-
    estudiante(Estudiante),
    forall(
    curso(Estudiante,_,_,Anio,_),
    curso(Estudiante,_,_,Anio,cursoDeVerano)). 