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
    materia(Materia,Horas), 
    not(integradora(Materia)),
    Horas>=4.

esPesada(Materia):-
    integradora(Materia),
    materia(Materia, Horas),
    Horas = 6.

esInicial(Materia):-
    materia(Materia,_),not(correlativas(Materia,_)).

paraCursar(Materia,Correlativas):-
    materia(Materia,_),correlativas(Materia,Correlativas).


/* paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas3(Materia1,Materia2,Materia3,Materia4)),correlativas(Materia1),correlativas(Materia2).

paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas1(Materia1,Materia2)),correlativas(Materia1),correlativas(Materia2).

paraCursar(Materia):-
    materia(Materia,_),correlativas(Materia,cursadas4(Materia1,Materia2)),correlativas(Materia1),correlativas(Materia2). */
