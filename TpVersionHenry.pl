%materia(nombre,horas)
materia(analisis_Matematico_I,5).
materia(algebra_y_Geometria_Analitica,5).
materia(matematica_Discreta,3).
materia(sistemas_y_Organizaciones,3).
materia(algoritmos_y_Estructuras_de_Datos,5).
materia(arquitectura_de_Computadoras,4).
materia(ingenieria_y_Sociedad,2).
materia(quimica,3).
materia(fisica_I,5).
materia(analisis_Matematico_II,5).
materia(probabilidad_y_Estadistica,3).
materia(analisis_de_Sistemas,6).
materia(sintaxis_y_Semantica_de_los_Lenguajes,4).
materia(paradigmas_de_Programacion,4).
materia(ingles_I,2).
materia(sistemas_de_Representacion,3).
materia(sistemas_Operativos,4).
materia(disenio_de_Sistemas,6).
materia(fisica_II,5).
materia(matematica_Superior,4).
materia(gestion_de_Datos,4).
materia(legislacion,2).
materia(economia,3).
materia(ingles_II,2).
materia(redes_de_Informacion,4).
materia(administracion_de_Recursos,6).
materia(investigacion_Operativa,5).
materia(simulacion,4).
materia(ingenieria_de_Software,3).
materia(teoria_de_Control,3).
materia(comunicaciones,4).
materia(proyecto_Final,6).
materia(inteligencia_Artificial,3).
materia(administracion_Gerencial,3).
materia(sistemas_de_Gestion,4).

%correlativa(correlativa superior,correlativa inferior)
correlativa(analisis_de_Sistemas,sistemas_y_Organizaciones).
correlativa(analisis_de_Sistemas,algoritmos_y_Estructuras_de_Datos).
correlativa(analisis_Matematico_II,analisis_Matematico_I).
correlativa(analisis_Matematico_II,algebra_y_Geometria_Analitica).
correlativa(sintaxis_y_Semantica_de_los_Lenguajes,algoritmos_y_Estructuras_de_Datos).
correlativa(sintaxis_y_Semantica_de_los_Lenguajes,matematica_Discreta).
correlativa(paradigmas_de_Programacion,matematica_Discreta).
correlativa(paradigmas_de_Programacion,algoritmos_y_Estructuras_de_Datos).
correlativa(probabilidad_y_Estadistica,analisis_Matematico_I).
correlativa(probabilidad_y_Estadistica,algebra_y_Geometria_Analitica).
correlativa(disenio_de_Sistemas,analisis_de_Sistemas).
correlativa(disenio_de_Sistemas,paradigmas_de_Programacion).
correlativa(sistemas_Operativos,matematica_Discreta).
correlativa(sistemas_Operativos,algoritmos_y_Estructuras_de_Datos).
correlativa(sistemas_Operativos,arquitectura_de_Computadoras).
correlativa(fisica_II,analisis_Matematico_I).
correlativa(fisica_II,fisica_I).
correlativa(economia,analisis_de_Sistemas).
correlativa(gestion_de_Datos,analisis_de_Sistemas).
correlativa(gestion_de_Datos,paradigmas_de_Programacion).
correlativa(gestion_de_Datos,sintaxis_y_Semantica_de_los_Lenguajes).
correlativa(ingles_II,ingles_I).
correlativa(matematica_Superior,analisis_Matematico_II).
correlativa(legislacion,analisis_de_Sistemas).
correlativa(legislacion,ingenieria_y_Sociedad).
correlativa(administracion_de_Recursos,disenio_de_Sistemas).
correlativa(administracion_de_Recursos,sistemas_Operativos).
correlativa(administracion_de_Recursos,economia).
correlativa(ingenieria_de_Software,probabilidad_y_Estadistica).
correlativa(ingenieria_de_Software,disenio_de_Sistemas).
correlativa(ingenieria_de_Software,gestion_de_Datos).
correlativa(teoria_de_Control,quimica).
correlativa(teoria_de_Control,matematica_Superior).
correlativa(comunicaciones,arquitectura_de_Computadoras).
correlativa(comunicaciones,analisis_Matematico_II).
correlativa(comunicaciones,fisica_II).
correlativa(redes_de_Informacion,sistemas_Operativos).
correlativa(redes_de_Informacion,comunicaciones).
correlativa(investigacion_Operativa,probabilidad_y_Estadistica).
correlativa(investigacion_Operativa,matematica_Superior).
correlativa(simulacion,probabilidad_y_Estadistica).
correlativa(simulacion,matematica_Superior).
correlativa(inteligencia_Artificial,investigacion_Operativa).
correlativa(inteligencia_Artificial,simulacion).
correlativa(administracion_Gerencial,administracion_de_Recursos).
correlativa(administracion_Gerencial,investigacion_Operativa).
correlativa(sistemas_de_Gestion,administracion_de_Recursos).
correlativa(sistemas_de_Gestion,investigacion_Operativa).
correlativa(sistemas_de_Gestion,simulacion).
correlativa(proyecto_Final,legislacion).
correlativa(proyecto_Final,administracion_de_Recursos).
correlativa(proyecto_Final,redes_de_Informacion).
correlativa(proyecto_Final,ingenieria_de_Software).

%libre(nombre)
libre(ingles_I).
libre(ingles_II).

%integradora(nombre)
integradora(sistemas_y_Organizaciones).
integradora(analisis_de_Sistemas).
integradora(disenio_de_Sistemas).
integradora(administracion_de_Recursos).
integradora(proyecto_Final).

%Punto 1, Materia pesada
materiaPesada(Materia) :- materia(Materia,Horas),integradora(Materia),Horas=6.
materiaPesada(Materia) :- materia(Materia,Horas),not(integradora(Materia)),Horas>=4.

%Punto 2, Correlativas
materiaInicial(Materia) :- materia(Materia,_),not(correlativa(Materia,_)).
materiaNecesariaParaCursar(Materia,MateriaReq) :- 
    materia(Materia,_),correlativa(Materia,MateriaReq).
materiaNecesariaParaCursar(Materia,Correlativa) :- 
    materia(Materia,_),
    correlativa(Materia,MateriaRequerida),
    materiaNecesariaParaCursar(MateriaRequerida,Correlativa).
materiaQueHabilita(Materia,Correlativa) :- correlativa(Correlativa,Materia).

%Punto 3, los estudiantes, materias cursadas y aprobadas
%cursada(Alumno,Materia,Nota,modo(año,modalidad))
cursada(vero,Materia,Nota,modo(2020,anual)) :- materiaInicial(Materia),Nota is 8.
cursada(alan,sistemas_y_Organizaciones,6,modo(2019,anual)).
cursada(alan,analisis_Matematico_I,6,modo(2019,anual)).
cursada(alan,analisis_de_Sistemas,2,modo(2020,anual)).
cursada(alan,analisis_de_Sistemas,9,modo(2021,verano)).
cursada(alan,fisica_I,2,modo(2020,anual)).

cursada(juan,sistemas_y_Organizaciones,6,modo(2015,anual)).
cursada(juan,quimica,4,modo(2015,cuatrimestral(1))).
cursada(juan,quimica,8,modo(2015,cuatrimestral(2))).
cursada(juan,fisica_I,7,modo(2016,verano)).

cursada(pepe,quimica,2,modo(2016,anual)).
cursada(pepe,quimica,3,modo(2017,cuatrimestral(1))).
cursada(pepe,quimica,4,modo(2017,cuatrimestal(2))).
cursada(pepe,quimica,5,modo(2018,anual)).
cursada(pepe,fisica_I,2,modo(2018,verano)).
cursada(pepe,fisica_I,2,modo(2018,anual)).

cursada(gaby,quimica,2,modo(2016,anual)).
cursada(gaby,quimica,3,modo(2017,cuatrimestral(2))).
cursada(gaby,fisica_I,2,modo(2017,anual)).
cursada(gaby,fisica_I,10,modo(2018,cuatrimestral(1))).

cursada(veraniego,quimica,6,modo(2016,anual)).
cursada(veraniego,fisica_I,6,modo(2017,verano)).
cursada(veraniego,matematica_Discreta,2,modo(2017,anual)).
cursada(veraniego,matematica_Discreta,8,modo(2018,verano)).

cursada(estudianteATR,quimica,10,modo(2016,cuatrimestral(1))).
cursada(estudianteATR,fisica_I,10,modo(2016,cuatrimestal(2))).

final(vero,ingles_II,10).
final(alan,sistemas_y_Organizaciones,4).
final(alan,ingles_I,2).

materiaCursada(Alumno,Materia) :- materiaAprobada(Alumno,Materia). %Para que considere cursadas aprobadas
materiaCursada(Alumno,Materia) :- cursada(Alumno,Materia,Nota,_),Nota>5.
materiaAprobada(Alumno,Materia) :- final(Alumno,Materia,Nota),Nota>5. 
materiaAprobada(Alumno,Materia) :- cursada(Alumno,Materia,Nota,_), Nota>7. %Promocion
materiaAprobada(Alumno,Materia) :- final(Alumno,Materia,Nota),libre(Materia),Nota>5. %Aprobado por libre

%Punto 4 y 5, modalidades (1.Primer cuatri,2.Segundo cuatri, 3.Verano)

alumno(vero).
alumno(alan).
alumno(juan).
alumno(pepe).
anioCursada(Alumno,Materia,Anio) :- 
    alumno(Alumno),
    cursada(Alumno,Materia,_,modo(Anio,Modo)),Modo\=verano.

anioCursada(Alumno,Materia,Anio) :-
    alumno(Alumno),
    cursada(Alumno,Materia,_,modo(Aniocursada,Modo)),
    Modo=verano,Anio is Aniocursada-1.

recursoMateria(Alumno,Materia) :-
    alumno(Alumno),
    cursada(Alumno,Materia,_,Modalidad),
    cursada(Alumno,Materia,_,OtraModalidad),
    Modalidad\=OtraModalidad.

%Punto 6, perfiles de estudiantes

%sinDescanso

invictus(Alumno) :- not(recursoMateria(Alumno,_)).

repechaje(Alumno) :- alumno(Alumno),
repruebaAnual(Alumno,Materia,Anio),
condicionRepechaje(Alumno,Materia,AnioSiguiente),
AnioSiguiente is Anio+1.

repruebaAnual(Alumno,Materia,Anio) :-
    cursada(Alumno,Materia,Nota1,modo(Anio,anual)),Nota1<6.

condicionRepechaje(Alumno,Materia,Anio) :-
    cursada(Alumno,Materia,Nota2,modo(Anio,cuatrimestral(1))),
    Nota2>7.

buenasCursadas(Alumno) :-
    forall(cursada(Alumno,_,Nota,_),Nota>7).
