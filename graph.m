(*STATI INIZIALI SQED*)

(*s1= {{p[1],0}, {p[2],-1}};
s2 = {{  p[3], 0}, {p[1]+p[2] - p[3], -1}};

S1 = {{p[1],1}, {p[2],-1}};
S2 = {{  p[3], 1}, {p[1]+p[2] - p[3], -1}};*)

(*S1 = {{p[1],1}, {p[2],-1}};
S2 = {{p[3], 1}, {p[4], -1}, {p[1]+p[2]-p[3]-p[4],0}};*)

(*S1 = {{p[1], 1}, {p[2],-1}};
S2 = {{p[3],0}, {p[4],0}, {p[5],1}, {p[1] +p[2]-p[3]-p[4]-p[5], -1}};*)


(*STATI INIZIALI QED*)
(*s1= {{p[1],0}, {p[2],-1}};
s2 = {{  p[3], 0}, {p[1]+p[2] - p[3], -1}};*)
masses = {0, Me, 0, Me};

S1 = {{p[1],1}, {p[2],-1}};
S2 = {{  p[3], 1}, {p[1]+p[2] - p[3], -1}};
Masses = {Me,Me,Me,Me};
 
(*S1 = {{p[1],1}, {p[2],-1}};
S2 = {{p[3], 1}, {p[4], -1}, {p[1]+p[2]-p[3]-p[4],0}};*)

(*S1 = {{p[1], 1}, {p[2],-1}};
S2 = {{p[3],0}, {p[4],0}, {p[5],1}, {p[1] +p[2]-p[3]-p[4]-p[5], -1}};*)






(* GENERAZIONE DEI GRAFI **************************************************************************************************************************
 **************************************************************************************************************************************************
 *)


G3 = {{1,-1}, {2,-1}, {3,-1}}

TADPOLE = {{1,-1},{-1,-1}}


NLOOP = 0;



GeneraGrafi3[n_Integer] := Module[ {actualn,listagrafi, listagrafi3},
 actualn=3;
 listagrafi={G3};
While[actualn<n,
          listagrafi=Flatten[Flatten[Map[Vertici3, listagrafi],1],1];
           actualn+=1;
           ];
                                  
 listagrafi
];



GeneraGrafi[n_Integer] := Module[ {actualn,listagrafi, listagrafi3, listagrafi4},
    actualn=3;
    listagrafi={G3};
    NLOOP = 0;
    While[actualn<n,
    listagrafi3=Flatten[Flatten[Map[Vertici3, listagrafi],1],1];
    listagrafi4=Flatten[Map[Vertici4, listagrafi],1];
        listagrafi=Join[listagrafi3,listagrafi4];
        actualn+=1;
        ];
        
    listagrafi
];

GrafiOneLoop[n_Integer] := Module[  {provvisoria,quanti, listagrafi,listagrafi3, listagrafi4 },
            provvisoria= {TADPOLE};
            NLOOP = 1;
            actualn = 1;
            While[actualn<n,
                 listagrafi3=Flatten[Flatten[Map[Vertici3, provvisoria],1],1];
                 listagrafi4=Flatten[Map[Vertici4, provvisoria],1];
                 provvisoria=Join[listagrafi3,listagrafi4];
                 actualn+=1;
           ];
          actualn= 1;
          quanti = Length[provvisoria];
          listagrafi = {};
          While[ actualn<=quanti ,
                If[FreeQ[listagrafi, provvisoria[[actualn]]],
                  listagrafi=  Append[listagrafi, provvisoria[[actualn]]];
                   actualn +=1,
                   actualn +=1
                ];
      ];
 
    listagrafi
];



ImprovedCounter[g_, i_] := Module[ {soluzione},
            If[ MemberQ[g, {i,i}],
            soluzione = Count[g, {i,x_} | {x_,i} ] +1,
            soluzione =  Count[g, {i,x_} | {x_,i} ];
            ];
                                  
 soluzione
];


Vertici4[g_] := Module[ {risultato, value,i, min, surpl},
           (* value = Table[{i, Count[g, {i,x_} | {x_,i} ]}, {i,min = Min[g],-1}];*)
            value = Table[{i, ImprovedCounter[g,i]}, {i,min = Min[g],-1}]; (*Altrimenti non conta due volte il tadpole*)
            value = Cases[value, {x_, y_} /; y =!= 4];
            value = value //. {{a___,{x_,y_},b___} -> {a,x,b}};
            risultato = Map[Append[g,{Max[g]+1,#}]&, value] ;
    risultato
    ];

    

Vertici3[g_] := Module[ {risultato, min, max},
        min = Min[g]-1;
        max = Max[g]+1;
        risultato = Map[Vertice3[g,min,max,#] &, g];
        risultato
    ];

 Vertice3[g_,a_,b_,c_] := Module[ {risultato,p, pippo},
        p = Position[g, c];
        risultato = Map[ReplacePart[g, pippo[{c[[1]],a}, {b,a}, {c[[2]],a}], #] /. pippo -> Sequence &, p] ;
        
        risultato
    ];










(* DRESSED GRAPHS* ****************************************************************************************************************************
***********************************************************************************************************************************************
 *)
(*ApplicaSoloP[g_] := Module[ {soluzione},
        soluzione = Map[Apply[p,#] &,g];
        soluzione
    ];

ApplicaSoloQ[g_] := Module[ {soluzione},
        soluzione = Map[Apply[Q,#] &, g];
        soluzione
];
            *)

 
Impulsi[g_] := Module[ {soluzione, eliminare, cercoloop, proposta, temp, sostituire},
			
            Which[ NLOOP === 0,
                   soluzione = Map[Apply[p,#] &, g],
                  
                   NLOOP === 1,
                   soluzione = Map[Apply[p,#] &, g];
                   eliminare=  Position[g, {a_, b_} /; a>0];
                   cercoloop = Delete[g, eliminare];
                   proposta = {};
                   proposta = Cases[cercoloop, {a_,b_} /; ( Count[cercoloop, {a,c_}] +  Count[cercoloop, {c_,a}] )>=2 && (Count[cercoloop, {b,c_}] + Count[cercoloop, {c_, b}]) >=2 ];
                   While[ proposta =!= cercoloop,
                       cercoloop = proposta;
                       proposta = Cases[cercoloop, {a_,b_} /; (Count[cercoloop, {a,c_}] + Count[cercoloop, {c_,a}]) >=2 && (Count[cercoloop, {b,c_}] + Count[cercoloop, {c_, b}]) >=2 ];
                         ];
                  cercoloop = Cases[proposta, {-1,a_}] [[1]];
                  temp = Apply[p, cercoloop];
                  sostituire = Position[soluzione, temp];
                  sostituire =Flatten[sostituire][[1]];
                  soluzione[[sostituire]] = Apply[k, cercoloop];
                  ];
    
			soluzione
            ];


 Cariche[g_] := Module[ {soluzione, eliminare, cercoloop, proposta, temp, sostituire},
                       
            Which[ NLOOP === 0,
                  soluzione = Map[Apply[Q,#] &, g],
                  
                   NLOOP === 1,
                  soluzione = Map[Apply[Q,#] &, g];
                  eliminare=  Position[g, {a_, b_} /; a>0];
                  cercoloop = Delete[g, eliminare];
                  proposta = {};
                  proposta = Cases[cercoloop, {a_,b_} /; ( Count[cercoloop, {a,c_}] +  Count[cercoloop, {c_,a}] )>=2 && (Count[cercoloop, {b,c_}] + Count[cercoloop, {c_, b}]) >=2 ];
                  While[ proposta =!= cercoloop,
                        cercoloop = proposta;
                        proposta = Cases[cercoloop, {a_,b_} /; (Count[cercoloop, {a,c_}] + Count[cercoloop, {c_,a}]) >=2 && (Count[cercoloop, {b,c_}] + Count[cercoloop, {c_, b}]) >=2 ];
                    ];
                    cercoloop = Cases[proposta, {-1,a_}] [[1]];
                    temp = Apply[Q, cercoloop];
                    sostituire = Position[soluzione, temp];
                    sostituire =Flatten[sostituire][[1]];
                    soluzione[[sostituire]] = Apply[Qk, cercoloop];
            ];
soluzione
];



 Colore[g_] := Module[ {soluzione},
            soluzione = Map[Apply[c,#] &, g];
            soluzione
            ];
 
 Lorentz[g_] := Module[ {soluzione},
            soluzione = Map[ Apply[l,#] &, g];
            soluzione
            ];

 Masse[g_] := Module[ {soluzione},
                    soluzione = Map[ Apply[m,#] &, g];
                    soluzione
            ];


 LeggiConservazione[g_] := Module[ {min, max, impulsi, cariche, equazioni ,parziale1, parziale2, parzialeP, parzialeK, incognite, soluzioneP, soluzioneQ, soluzione, globale,quazioniQ, parzialeQ, parzialeQK, incogniteQ, globaleQ},
                    max = Max[g];
                    min = Min[g];
                    impulsi = Impulsi[g];
                    
                    cariche = Cariche[g];

                    equazioni = Table[
                                parzialeP= Join[Cases[impulsi,p[a_,i]], -Cases[impulsi,p[i,a_]]];
                                parzialeK = Join[Cases[impulsi,k[a_,i]], -Cases[impulsi,k[i,a_]]];
                                parziale1 = Join[parzialeP, parzialeK];
                                Apply[Plus, parziale1] == 0,
                                {i,min,-1}
                        ];
                    equazioni = equazioni //. { p[i_ /; i>0,j_] -> p[i]};
                    incognite = Cases[ equazioni, p[a_,b_], Infinity];
                    globale = p[max] -> - Sum[p[i], {i,1,max-1}] ;
                    equazioni = equazioni /. globale;
                    
                    soluzioneP = Solve[equazioni, incognite];
                    soluzioneP = Append[soluzioneP, globale];
                                  
                    equazioniQ = Table[
                                parzialeQ= Join[Cases[cariche,Q[a_,i]], -Cases[cariche,Q[i,a_]]];
                                parzialeQK = Join[Cases[cariche,Qk[a_,i]], -Cases[cariche,Qk[i,a_]]];
                                parziale2 = Join[parzialeQ, parzialeQK];
                                Apply[Plus, parziale2] == 0,
                                {i,min,-1}
                        ];
                    equazioniQ = equazioniQ //. { Q[i_ /; i>0,j_] -> Q[i]};
                    incogniteQ = Cases[ equazioniQ, Q[a_,b_], Infinity];
                    globaleQ = Q[max] -> - Sum[Q[i], {i,1,max-1}] ;
                    equazioniQ = equazioniQ /. globaleQ;
                    
                    soluzioneQ = Solve[equazioniQ, incogniteQ];
                    soluzioneQ = Append[soluzioneQ, globaleQ];
                    soluzione = Flatten[Join[ soluzioneP, soluzioneQ],1];
soluzione
];
 
 
 
(*LeggiConservazione[g_, max_, min_] := Module[ {impulsi, cariche, equazioni ,parziale, parzialeP, parzialeK, incognite, soluzioneP, soluzioneQ, soluzione, globale},
                    
                   
                    equazioni = Table[
                                parziale = Join[Cases[g,f_[a_,i]], -Cases[g,f_[i,a_]]];
                                Apply[Plus, parziale] == 0,
                                {i,min,-1}
                        ];
                    equazioni = equazioni //. { f_[i_ /; i>0,j_] -> f[i]};
                    incognite = Cases[ equazioni, f_[a_Integer,b_Integer], Infinity];
                    globale = f_[max] -> - Sum[f[i], {i,1,max-1}] ;
                    equazioni = equazioni /. globale;
                    
                    soluzione = Solve[equazioni, incognite];
                    soluzione = Append[soluzione, globale];

soluzione
];*)



(* AUMENTANDO IL NUMERO DI PROPRIETA' SARA' NECESSARIO RIVEDERE TUTTI GLI INDICI *)

CorrectOrder[ pr_] := Module[ {soluzione, temp},
            
                If[Flatten[Position[ pr, m[a__]]] === {3} || Flatten[Position[ pr, -m[a__]]] === {3},
               temp = pr[[3]];
               soluzione = pr;
               soluzione[[3]] = soluzione[[1]];
               soluzione[[1]] = soluzione[[2]];
               soluzione[[2]] = temp,
               soluzione = pr;
               ];
               
soluzione
];

 
Vestigrafo[g_] := Module[ {max, min,v,  impulsi, masse, lorentz, vertici, grafo, soluzione, esterne, propr, propagatore, propagatori, leggi, properties, propProperties,pr, function, loop, proprQ, proprL, proprM, proprP, vert},
                         
                          max = Max[g];
                          min = Min[g];
                          leggi = LeggiConservazione[g];
                         
                          vertici = Table[
                                impulsi = Impulsi[g];
                                cariche = Cariche[g];
                                v = Join[ Cases[impulsi, f_[i,a_]], Cases[impulsi, f_[b_,i]]];
                                impulsi = v;
                                v = Join[Cases[cariche,f_[i,a_]], Cases[cariche, f_[b_,i]]];
                                cariche = v;
                                v = Join[Cases[g, {i,a_}], Cases[ g, {b_,i}]];
                                lorentz = Lorentz[v];
                                masse= Masse[v];
                                impulsi = impulsi /. p[a_, b_] :> p[a] /; a > 0  && b < 0;
                                impulsi = impulsi /. k[a_, b_] :> k[a] /; a > 0  && b < 0;
                                cariche = cariche /. Q[a_, b_] :> Q[a] /; a > 0  && b < 0;
                                lorentz = lorentz /. l[a_, b_] :> l[a] /; a > 0  && b < 0;
                                masse = masse /. m[a_, b_] :> m[a] /; a>0 && b<0;
                                impulsi = impulsi /. p[i, b_]  :>  - p[i,b];
                                cariche = cariche /. Q[i,b_] :>  - Q[i,b];
                                masse = masse /. m[i,b_] :> -m[i,b];
                                lorentz = lorentz /. l[i, b_] :> l[b];
                                lorentz = lorentz /. l[b_, i] :> l[b];
                                properties = (lorentz+impulsi+cariche + masse ) /. Plus -> List;
                                    
                                If[ NLOOP === 1,
                                  properties= Map[CorrectOrder, properties],
                                  properties
                                ];

                                properties = properties //. leggi ;
                                          
                                vert = Apply[vertice[i], properties],
                                {i,min, -1}
                             ];
                         
                         
                         
                               
                          esterne = Cases[g, {a_,b_} /; a>0];
                          proprQ = esterne //. List[x___,{a_,b_},y___] :> List[x,a,y];
                          esterne = Map[ Apply[lineaext, #] &, esterne];
                          proprQ= Map[Q, proprQ];
                          proprP= proprQ //. Q -> p;
                          proprM= proprQ //. Q -> m;
                          proprL= proprQ //. Q -> l;
                          extProperties=(proprL+proprQ+proprP+ proprM) /. Plus -> List;
                          ext =  Table[
                                    Apply[esterne[[i]], extProperties[[i]]],
                                    {i,1,Length[esterne]}
                                ];
                          
                          propagatori = Cases[Impulsi[g], f_[a_,b_] /; a <0 && b < 0];
                          proprP = propagatori;
                          propagatori = Cases[Cariche[g], f_[a_,b_] /;  a <0 && b < 0];
                          proprQ = propagatori;
                          propagatori = Map[Apply[prop,#] &, propagatori];
                          proprM= Map[Apply[m,#]&, propagatori];
                          proprL= proprM //. m[a_, b_] :> function[l[a],l[b]];
                          propProperties=proprL+proprP+proprQ+proprM /. Plus -> List;
                          propProperties = propProperties /. function -> List;
                          propProperties = propProperties //. leggi;
                          pr = Table[
                                     Apply[propagatori[[i]], propProperties[[i]]],
                                     {i,1,Length[propagatori]}
                                     ];
                          
                          soluzione = Apply[Times, ext] Apply[Times, vertici] Apply[Times, pr];
                          soluzione
];
 

 








               
                       


                           
                           
                           
                           
                           
                           
                          
                          
    
                          
(* PROCESS ASSIGNMENT SQED *********************************************************************************************************************************
*******************************************************************************************************************************************************
 *)
                          
                                                    

ProcessSQED[s1_, s2_, loop_] := Module[{g,leggi,diagrammi, NExt, Controllo, lista, listaV,s2u},
                     NExt= Length[s1]+Length[s2];
                     s2u = - s2 ;
                     NLOOP = loop;
                     Controllo = Apply[Plus,s1] + Apply[Plus,s2u]; (*conservazione globale*)
                     If[Controllo === {0,0},
                        Which[ NLOOP === 0,
                              lista = GeneraGrafi[NExt];
                              listaV = Map[Vestigrafo, lista];
                              proposta = Map[AssegnaProcessoSQED[s1,s2u,#]&, listaV];
                              diagrammi = Map[ControllaDiagrammiSQED[#]&,proposta];
                              eliminare = Position[diagrammi, {}];
                              diagrammi=Delete[diagrammi, eliminare],
                               NLOOP === 1,
                                lista = GrafiOneLoop[NExt];
                                listaV = Map[Vestigrafo, lista];
                                proposta = Flatten[Map[AssegnaProcessoSQED[s1,s2u,#]&, listaV]];
                                diagrammi = Map[ControllaDiagrammiSQED[#]&,proposta];
                                eliminare = Position[diagrammi, {}];
                               diagrammi=Delete[diagrammi, eliminare];
                         ];
                        diagrammi,
                        diagrammi=0;
                      ];
diagrammi
];
                                         

                           
AssegnaProcessoSQED[a_,b_, g_] := Module[ {soluzione, soluzioni, numIniziali, numFinali, subs1, subs2, subs,i},
                                        numIniziali = Length[a];
                                        numFinali = Length[b];
                                        subs1 = Table[
                                                       {Q[i] -> a[[i,2]],p[i] -> a[[i,1]] },
                                                        {i,1,numIniziali}
                                                        ];
                                        subs2 = Table[
                                                       {Q[i+numIniziali] -> b[[i,2]], p[i+numIniziali] -> b[[i,1]]},
                                                       {i,1,numFinali}
                                                       ];
                                        subs = Join[Flatten[subs1], Flatten[subs2]];
                                         Which[ NLOOP === 0,
                                               soluzione = g /. subs,
                                               NLOOP === 1,
                                               soluzione = g /. subs;
                                               soluzioni = Table[
                                                                 {soluzione /. Qk[c_,d_] -> i},
                                                                 {i,-1,1}
                                                ];
                                               soluzione = soluzioni;
                                        ];
                                       
    soluzione
];
                           
ControllaDiagrammiSQED[g_] := Module[ {soluzione, cond1, cond2, cond3,cond4},
                    cond1=FreeQ[g, lineaext[a_,b_][c__,d_] /; d>1 || d<-1];
                    cond2=FreeQ[g, prop[a_,b_][c___,e_] /; e>1 || e< -1];
                    cond3 = FreeQ[g, vertice[a_][{b__,0}, {c__,0}, {e__,0}]];
                    cond4 = FreeQ[g, vertice[a_][{b__,0},{c__,0}, {e__,0}, {f__,0}]];
                        
                    If[cond1 && cond2 && cond3 && cond4,
                       soluzione = g,
                       soluzione = {}
                    ];
soluzione
 ];
                          
     
                          
                        
           
                           
                           
                           
                           
 
(*FEYNMAN RULES FOR SQED *********************************************************************************************************************
**********************************************************************************************************************************************
 *)

AssegnaRegoleSQED[d_, in_, out_] := Module[ {propfotoni, g, mu, soluzione, propscalari, esterne,
                                                   verticiTre, veticiQuattro},
            
            esterne = Cases[d, lineaext[a__][b__]];
            esterne = Map[RegoleEsterneSQED, esterne];
            propscalari = Cases[d, prop[a_,b_][c__,f_] /; f =!= 0];
            propscalari = Map[propagatoreScalareSQED, propscalari];
            propfotoni = Cases[d, prop[a_,b_][c__,0]];
            propfotoni = Map[propagatoreFotoneSQED, propfotoni];
            verticiTre = Cases[d, vertice[a_][b___] /; Length[vertice[a][b]] === 3 ];
            verticiTre = Map[RegoleTreSQED, verticiTre];
            verticiQuattro =Cases[d, vertice[a_][b___] /; Length[vertice[a][b]] === 4 ];
            verticiQuattro = Map[RegoleQuattroSQED, verticiQuattro];
           
                                                
            soluzione = Apply[Times,esterne] Apply[Times,propfotoni] Apply[Times, propscalari] Apply[Times, verticiTre] Apply[Times, verticiQuattro];
    soluzione
];



RegoleEsterneSQED[a_] := Module[ {soluzione},
            If[a[[-1]] =!= 0,
               soluzione = 1,
               soluzione = eps[a[[3]]][a[[1]]]
               ];
  soluzione
];

(*RegoleEsterneInSQED[a_] := Module[ {soluzione},
               If[a[[-1]] =!= 0,
                  soluzione = 1,
                  soluzione = eps[a[[3]]][a[[1]]]
                ];
 soluzione
 ];
                                                                                
 RegoleEsterneOutSQED[a_] := Module[ {soluzione},
       If[a[[-1]] =!= 0,
              soluzione = 1,
          soluzione = Conjugate[eps[a[[3]]]][a[[1]]]
        ];
soluzione
];*)
                                                      
 propagatoreFotoneSQED[a_] := Module[ {soluzione, tens},
        tens = a[[1]] /. List -> g;
       soluzione = - I tens / a[[3]]^2;
soluzione
];
                                                                               
propagatoreScalareSQED[a_] := Module[ {soluzione},
       soluzione =  I/(a[[3]]^2 - a[[2]]^2 );
soluzione
 ];
                                                     

RegoleTreSQED[a_] := Module[ {soluzione, ferm1, ferm2, fot, diff},
      ferm1=Flatten[ Cases[a, {b__,-1}]];
      ferm2 = Flatten[Cases[a, {b__, 1}]];
      fot = Flatten[Cases[a,{b__,0}]];
      diff = ferm1[[3]] - ferm2[[3]];
      soluzione = I e diff[fot[[1]]];
                                                   
soluzione
];
                                                                                 

RegoleQuattroSQED[a_] := Module[ {soluzione, ferm, tens, indici},
    ferm = Cases[a, {b__,d_} /; d =!= 0];
    If[Length[ferm] === 4,
       soluzione = - I Lambda,
       indici = Cases[a, {b_, c___,0}];
       indici= indici /. {b_,c___,0} -> b;
       tens = g[indici[[1]],indici[[2]]];
       (*tens = tens /. {g[l[b_]][l[c_]] -> g[l[b][c]] };*)
       soluzione = I e^2 tens
    ];
                                                       
soluzione
];
     





 
(*PROCESS ASSIGNMENT QED *********************************************************************************************************************
**********************************************************************************************************************************************
 *)

ProcessQED[s1_, s2_, masse_] := Module[{g,leggi,diagrammi, NExt, Controllo, lista, listaV,s2u},
                     NExt= Length[s1]+Length[s2];
                     s2u = - s2 ;
                     
                     Controllo = Apply[Plus,s1] + Apply[Plus,s2u]; (*conservazione globale*)
                     If[Controllo === {0,0},
                            lista = GeneraGrafi3[NExt];
                            listaV = Map[Vestigrafo, lista];
                            proposta = Map[AssegnaProcessoQED[s1,s2u,masse,#]&, listaV];
                            diagrammi = Map[ControllaDiagrammiQED[#]&,proposta];
                            eliminare = Position[diagrammi, {}];
                            diagrammi=Delete[diagrammi, eliminare],
                            diagrammi = 0;
                         ];
                        
            
                      
diagrammi
];


AssegnaProcessoQED[a_,b_,masse_, g_] := Module[ {soluzione, soluzioni, numIniziali, numFinali, subs1, subs2, subs,i},
                                        numIniziali = Length[a];
                                        numFinali = Length[b];
                                        subs1 = Table[
                                                       {Q[i] -> a[[i,2]],p[i] -> a[[i,1]], m[i] -> masse[[i]] },
                                                        {i,1,numIniziali}
                                                        ];
                                        subs2 = Table[
                                                       {Q[i+numIniziali] -> b[[i,2]], p[i+numIniziali] -> b[[i,1]], m[i+numIniziali] -> masse[[i+numIniziali]]},
                                                       {i,1,numFinali}
                                                       ];
                                        subs = Join[Flatten[subs1], Flatten[subs2]];
                                        soluzione = g /. subs;
                                      
                                       
    soluzione
];

ControllaDiagrammiQED[g_] := Module[ {soluzione, cond1, cond2, cond3,cond4},
                    cond1=FreeQ[g, lineaext[a_,b_][c__,d_] /; d>1 || d<-1];
                    cond2=FreeQ[g, prop[a_,b_][c___,e_] /; e>1 || e< -1];
                    cond3 = FreeQ[g, vertice[a_][{b__,0}, {c__,0}, {e__,0}]];
                    cond4 = FreeQ[g, vertice[a_][{b__,e_},{c__,f_}, {d__,h_}] /; e=!= 0 && f =!= 0 && h=!= 0];
                        
                    If[cond1 && cond2 && cond3 && cond4,
                       soluzione = g,
                       soluzione = {}
                    ];
soluzione
 ];
      



 
 
(*FEYNMAN RULES FOR QED **********************************************************************************************************************
**********************************************************************************************************************************************
 *)
NormalOrdering = 0;
SetAttributes[g,Orderless];

AssegnaRegoleQED[d_, in_, out_] := Module[ {vertici, esterneF,LeptoniPosIn, FermionIn, LeptoniPosOut, FermionOut, propfotoni, soluzione, subs, InvertiImpulso},
            
                                          
            
            esterneF = Cases[d, lineaext[a__][b__,0]];
            esterneF = Map[FotoniEsterniQED, esterneF];
            propfotoni = Cases[d, prop[a_,b_][c__,0]];
            propfotoni = Map[propagatoreFotoneQED, propfotoni];

            LeptoniPosIn = Cases[d,lineaext[a_,b_][c__,1] /; a<=in ];
            FermionIn = Map[InFermionicPath[d,# ,in]&,LeptoniPosIn];
            
             
            LeptoniPosOut = Cases[d,lineaext[a_,b_][c__,1] /; a>in];
            FermionOut = Map[ OutFermionicPath[d,#, in]&, LeptoniPosOut];
            
            FermionIn = Apply[Times,FermionIn];
            subsIn := { v___ FermionChain[a__, SpinorU[x_, m12_]] FermionChain[Bar[SpinorV[y_, m21_]],c__] /; x===-y :> v I FermionChain[a, (DiracSlash[-x] + Abs[m12])/(x^2-m12^2), c] };
            FermionIn = FermionIn //. subsIn;
                                          
            FermionOut = Apply[Times,FermionOut];
            subsOut := {v___ FermionChain[a__, SpinorV[x_,m12_]] FermionChain[Bar[SpinorU[y_, m21_]],c__] /; x===-y :> v I FermionChain[a, (DiracSlash[-x] + Abs[m12])/(x^2-m12^2), c]};
            FermionOut = FermionOut //. subsOut;
            
            soluzione =  FermionIn FermionOut;
            soluzione =  soluzione Apply[Times,propfotoni] Apply[Times,esterneF];
            (*soluzione = soluzione /. FermionChain[a_, DiracMatrix[l[i_]], b_] g[l[i_],l[j_]] FermionChain[c_, DiracMatrix[l[j_]], d_] :> FermionChain;*)

                            
    soluzione
];

InFermionicPath[d_, ext_, in_] := Module[ {soluzione,vert, from, to, stop, indice},
                                   
        from = ext[[0,1]];
        to = ext[[0,2]];
        vert = Cases[d, vertice[to][e___]][[1]];
                                   
        stop = True;
        soluzione = {VerticeForwardQED[vert]};
        indice = to;
        While[ stop,
              successivo = Join[Cases[d, lineaext[a_,indice][b___,c_] /; lineaext[a,indice][b,c] =!= ext && c=!=0], Cases[d,prop[indice, c_][e__,f_] /; f =!=0]] [[1]];
              If[MatchQ[Head[successivo], lineaext[a_,indice]],
                 If[Head[successivo][[1]] > in,
                    NormalOrdering += 1;
                    soluzione[[-1]] = soluzione[[-1]] /. {FermionChain[a___, SpinorU[x_,m_]] :> FermionChain[a, SpinorV[-x, m]]};
                    ];
                 
                 stop = False,
                 indice= successivo[[0,2]];
                 vert = Cases[d, vertice[indice][e___]][[1]];
                soluzione = Append[soluzione,VerticeBackwardQED[vert]];
                 ];
              ];
        
        soluzione = ((-1)^(NormalOrdering -1)) * Apply[Times, soluzione];
          
        
soluzione
        ];
        
                                  
OutFermionicPath[d_,ext_, in_] := Module[ {soluzione,vert, from, to, stop, indice},
                                       
        from = ext[[0,1]];
        to = ext[[0,2]];
        vert = Cases[d, vertice[to][e___]][[1]];
                                       
        stop = True;
        soluzione = {VerticeBackwardQED[vert]};
        indice = to;
            While[ stop,
               successivo = Join[Cases[d, lineaext[a_,indice][b___,c_] /; lineaext[a,indice][b,c] =!= ext && c=!=0], Cases[d,prop[c_, indice][e__,f_] /; f =!=0]] [[1]];
                  If[MatchQ[Head[successivo], lineaext[a_,indice]],
                     If[Head[successivo][[1]] <= in,
                     NormalOrdering += 1;
                        soluzione[[-1]] = soluzione[[-1]] /. FermionChain[a___, SpinorV[x_,m_]] :> FermionChain[a, SpinorU[-x,m]];
                     ];
                     
                     stop = False,
                     indice= successivo[[0,1]];
                     vert = Cases[d, vertice[indice][e___]][[1]];
                     soluzione = Append[soluzione,VerticeBackwardQED[vert]];
                     ];
                  ];
             soluzione = ((-1)^(NormalOrdering -1)) * Apply[Times, soluzione];
soluzione
            ];
        
    
    
VerticeForwardQED[a_] := Module[ {soluzione, u ,v,f},
       u = Flatten[Cases[a, {b___,-1}],1];
       v =Flatten[ Cases[a, {b___,1}],1];
       f = Flatten[Cases[a, {b___,0}],1];
       v[[2]] = u[[2]]; (*La QED non mischia i sapori*)
       soluzione = I e FermionChain[ Bar[SpinorV[v[[3]], v[[2]]]], DiracMatrix[f[[1]]], SpinorU[u[[3]], u[[2]]]];
                         
soluzione
];

VerticeBackwardQED[a_] := Module[ {soluzione, u ,v,f},
        u = Flatten[Cases[a, {b___,-1}],1];
        v = Flatten[ Cases[a, {b___,1}],1];
        f = Flatten[Cases[a, {b___,0}],1];
        v[[2]] = u[[2]]; (*La QED non mischia i sapori*)
        soluzione = I e FermionChain[ Bar[SpinorU[-v[[3]], v[[2]]]], DiracMatrix[f[[1]]], SpinorV[-u[[3]], u[[2]]]];
                             
soluzione
];


FotoniEsterniQED[a_] := Module[ {soluzione},
            If[a[[-1]] =!= 0,
               soluzione = 1,
               soluzione = eps[a[[3]]][a[[1]]]
               ];
  soluzione
];

                                                      
 propagatoreFotoneQED[a_] := Module[ {soluzione, tens},
        tens = a[[1]] /. List -> g;
       soluzione = - I tens / a[[3]]^2;
soluzione
];
                                                                       
     




(*AMPLITUDE GENERATION SQED***********************************************************************************************************************
**************************************************************************************************************************************************
*)

ManipolaIndiciSQED[a_] := Module[ {soluzione, subs },
                    SetAttributes[g,Orderless];
                    soluzione = a;
                    subs := { g[l[c_],l[d_]] g[l[f_],l[d_]] ->   g[l[c],l[f]],
                               g[l[c_],l[d_]] f_[l[c_]] ->   f[l[d]],
                               c_[l[d_]] f_[l[d_]]  ->   SP[c,f]};
                    SetAttributes[SP, Orderless];
                    soluzione = soluzione //. subs;
soluzione
];
                        
ProdottoComplessoSQED[amp1_,amp2_, quantifotoni_] := Module[ {soluzione1, soluzione2, list1, list2, unit1, unit2, unit, subs1, subs2}, (* il secondo prende la coniugazione *)
        
            unit1 = 1;
            unit2 = 1;
            
            list1 = Apply[List,amp1];
            list2 = Apply[List,amp2];

            If[MemberQ[list1,+I],
               unit1 = Cases[list1, +I][[1]];
               soluzione1 = -1  amp1 unit1,
               soluzione1=amp1;
            ];
            If[MemberQ[list1,-I],
                unit1 = Cases[list1, -I][[1]];
                soluzione1 = -1  amp1 unit1,
                soluzione1=soluzione1;
            ];
            If[MemberQ[list2,+I],
               unit2 = Cases[list2, +I][[1]];
               soluzione2= -1 amp2 unit2,
               soluzione2=amp2;
            ];
            If[MemberQ[list2,-I],
               unit2 = Cases[list2, -I][[1]];
               soluzione2= -1 amp2 unit2,
               soluzione2=soluzione2;
            ];
                                          
            (*Print["if verified"];*)
           unit = unit1 Conjugate[unit2];
           soluzione = (soluzione1 soluzione2 unit);
            
                                                            
            subs1 = {SP[eps[b_], c_] d___ SP[eps[b_], e_] -> (-1  d SP[c,e])};
            subs2 = {SP[eps[b_], c_]^2  -> -1  SP[c,c]};
            subs3 = {SP[eps[b_], eps[b_]] -> -4 };
            subs = Join[subs1, subs2, subs3];

           soluzione = soluzione //. subs;
           
            If[quantifotoni =!=0,
               soluzione = soluzione / (2 * quantifotoni),
               soluzione
             ];
          (* Print["subst done"];*)
soluzione
];
                           
                           
                           
(*AMPLITUDE GENERATION QED******************************STRETTAMENTE PER AMPIEZZAE NON POLARIZZATE************************************************
**************************************************************************************************************************************************
*)
    
Bar[Bar[a_]] := a ;
Bar[DiracMatrix[b_]] := DiracMatrix[b];

ProdottoComplessoQED[amp1_,amp2_, in_] := Module[ {soluzione1, soluzione2, list1, list2,unit1, unit2, unit, subs1, subs2, subs3, subs4,s1,s2,fotoni, newamp1, newamp2, Toreverse, reversed, mass, quantifotoni}, (* il secondo prende la coniugazione *)
        
        unit1 = 1;
        unit2 = 1;

        newamp1 = amp1 /. {FermionChain[a__, DiracMatrix[l[i_]], b__] g[l[i_], l[j_]] FermionChain[c__, DiracMatrix[l[j_]], d__] :> FermionChain[a, DiracMatrix[l[i]], b] FermionChain[c, DiracMatrix[l[i]], d]};
        newamp2 =  amp2 /. {FermionChain[a__, DiracMatrix[l[i_]], b__] g[l[i_], l[j_]] FermionChain[c__, DiracMatrix[l[j_]], d__] :> FermionChain[a, DiracMatrix[l[j]], b] FermionChain[c, DiracMatrix[l[j]], d]};
                                                 
        list1 = Apply[List,newamp1];
        list2 = Apply[List,newamp2];
                                                 
        fotoni= Position[list1, eps[a_][b_]];
        list1 = Delete[list1,fotoni];
        quantifotoni = Length[fotoni];
        quantifotoni = (-1)^quantifotoni;
        fotoni= Position[list2, eps[a_][b_]];
        list2 = Delete[list2,fotoni];
                                                 
        (* La seguente operazione semplifica i passaggi successivi *)
        Toreverse = Position[list2, FermionChain[a___]];
        reversed= Flatten[Map[ list2[[#]]&, Toreverse ]];
        reversed = Map[Apply[List,#]&, reversed];
        reversed = Map[Reverse,reversed];
        reversed = Map[Bar, reversed, {2}];
        reversed = Map[Apply[FermionChain,#]&, reversed];
        list2 = Delete[list2, Toreverse];
        list2 = Flatten[Append[list2, reversed]];
        newamp1 = Apply[Times, list1];
        newamp2 = Apply[Times, list2];
                                                 
        If[MemberQ[list1,+I],
            unit1 = Cases[list1, +I][[1]];
            soluzione1 = -1  newamp1 unit1,
            soluzione1=newamp1;
        ];
        If[MemberQ[list1,-I],
            unit1 = Cases[list1, -I][[1]];
            soluzione1 = -1  newamp1 unit1,
            soluzione1=soluzione1;
        ];
        If[MemberQ[list2,+I],
            unit2 = Cases[list2, +I][[1]];
            soluzione2= -1 newamp2 unit2,
            soluzione2=newamp2;
        ];
        If[MemberQ[list2,-I],
            unit2 = Cases[list2, -I][[1]];
            soluzione2= -1 newamp2 unit2,
            soluzione2=soluzione2;
        ];
                                          
            (*Print["if verified"];*)
           unit = unit1 Conjugate[unit2];
           soluzione = (soluzione1 soluzione2 unit);
         
                                                            
          subs1 := {FermionChain[a__, SpinorU[b_,m_]] FermionChain[Bar[SpinorU[b_,m_]], c__] :> FermionChain[a, (DiracSlash[b] + m)/(2 * m), c]};
          subs2 := {FermionChain[a__, SpinorV[b_,m_]] FermionChain[Bar[SpinorV[b_,m_]], c__] :> FermionChain[a, (DiracSlash[b] - m)/(2 * m), c]};
        
          subs3 := {FermionChain[Bar[SpinorU[b_,m_]], a__, SpinorU[b_,m_]] :> Traccia[(DiracSlash[b] + m)/(2 * m), a]};
          subs4 := {FermionChain[Bar[SpinorV[b_,m_]], a__, SpinorV[b_,m_]] :> Traccia[(DiracSlash[b] - m)/(2 * m), a]};
         
          
          s1 = Join[subs1, subs2];
          s2 = Join[subs3, subs4];

         soluzione = soluzione //. s1;
         soluzione = soluzione //. s2;
         soluzione = quantifotoni soluzione / (2 * in);
soluzione
];
                           
                           
(*UTILE PER IDENTIFICARE LE PARTICELLE *************************************************************************************************************************
****************************************************************************************************************************************************************
 *)
                          
                          
ParticellaIn[a_, b_] := Module[ {soluzione},
                                Which[ b===0,
                                       soluzione = {"fotone ",  a},
                                       b===-1,
                                       soluzione = {"leptone -", a},
                                       b===+1,
                                      soluzione = {"leptone +", a}
                                      ];
                               soluzione
                        ];
                          
ParticellaOut[a_, b_] := Module[ {soluzione},
                                Which[ b===0,
                                       soluzione = {"fotone",  a},
                                       b===-1,
                                       soluzione = {"leptone -", a},
                                       b===+1,
                                      soluzione = {"leptone +", a}
                                      ];
                               soluzione
                        ];
                          
IdentificaParticelle[a_, b_] := Module[ {nIn,nOut, soluzione, In, Out},
                                  nIn = Length[a];
                                  nOut = Length[b];
                                  In = Table[
                                             Apply[ParticellaIn,a[[i]]],
                                             {i,1,nIn}
                                            ];
                                  In = Append[In, "In particles"];
                                        
                                  Out = Table[
                                              Apply[ParticellaOut,b[[i]]],
                                             {i,1,nOut}
                                             ];
                                  Out = Append[Out, "Out particles"];
                                  
                                  soluzione = Join[In, Out];
                             soluzione
                             ];
                          
                   
                          
                          
                          
    


                           
                           
    (*MAIN DI ESEMPIO*)
                           
 eseguiSQED[InState_, OutState_, loop_] := Module[ {processo,in, out, soluzione, quantiprodotti,prodotti, indice, quantifotoni},
    processo = ProcessSQED[InState,OutState,loop];
    in = Length[InState];
    out = Length[OutState];
    quantifotoni = Map[Apply[ParticellaIn,#]&, InState];
    quantifotoni = Count[quantifotoni, {"fotone ", b_}];
    processo= Map[AssegnaRegoleSQED[#,in,out]&,processo];
    processo = Map[ManipolaIndiciSQED, processo];
    soluzione = processo;
    quantiprodotti = Length[processo];
    indice=0;
    soluzione = {};
    While[ indice < quantiprodotti,
         prodotti=Map[ProdottoComplessoSQED[processo[[indice+1]], #, quantifotoni] &, processo];
         soluzione = Append[soluzione, prodotti];
          indice +=1;
          ];
  
    soluzione = Flatten[soluzione];

    soluzione = Apply[ Plus, soluzione];
    soluzione
];
   
              

eseguiQED[InState_, OutState_, M_] := Module[ {processo,in, out, soluzione, array},
            processo = ProcessQED[InState, OutState, M];
            in = Length[InState];
            out = Length[OutState];
            processo = Map[AssegnaRegoleQED[#,in, out]&, processo];
            (*Controllare che non si stanno mischiando i sapori*)
            array = Flatten[Outer[f, processo, processo]];
            array = array  /. f[a_,b_] -> f[a,b,in];
            processo = array /. f -> ProdottoComplessoQED;
                                         
            soluzione = processo ;
                                         
                                         
soluzione
];

                          

                           

