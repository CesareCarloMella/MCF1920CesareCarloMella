
(*S1= {{p[1],0}, {p[2],1}};
S2 = {{  p[3], 0}, {p[1]+p[2] - p[3], 1}};*)

(*S1 = {{p[1],1}, {p[2],-1}};
S2 = {{  p[3], 1}, {p[1]+p[2] - p[3], -1}};*)

(*S1 = {{p[1],1}, {p[2],-1}};
S2 = {{p[3], 1}, {p[4], -1}, {p[1]+p[2]-p[3]-p[4],0}};*)

S1 = {{p[1], 1}, {p[2],-1}};
S2 = {{p[3],0}, {p[4],0}, {p[5],1}, {p[1] +p[2]-p[3]-p[4]-p[5], -1}};


(* GENERAZIONE DEI GRAFI ********************************        *)
(* Si parte dal grafo elementare con n = 3 archi. G = {{1,-1}, {2,-1}, {3,-1}} *)

G3 = {{1,-1}, {2,-1}, {3,-1}}





GeneraGrafi[n_Integer] := Module[ {actualn,listagrafi, listagrafi3, listagrafi4},
    actualn=3;
    listagrafi={G3};
    While[actualn<n,
    listagrafi3=Flatten[Map[Vertici3, listagrafi],1];
        listagrafi4=Flatten[Map[Vertici4, listagrafi],1];
        listagrafi=Join[listagrafi3,listagrafi4];
        actualn+=1;
        ];
        
    listagrafi
    ];


Vertici4[g_] := Module[ {risultato, value,i, min},
            value = Table[{i, Count[g, {i,x_} | {x_,i}]}, {i,min = Min[g],-1}];
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
        risultato = ReplacePart[g, pippo[{c[[1]],a}, {b,a}, {c[[2]],a}], p] /. pippo -> Sequence ;
        
        risultato
    ];




 
 Impulsi[g_] := Module[ {soluzine},
			soluzione = Map[Apply[p,#] &, g];
			soluzione
            ];
 Cariche[g_] := Module[ {soluzione},
            soluzione = Map[Apply[Q,#] &, g];
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


 LeggiConservazione[g_] := Module[ {min, max, impulsi, equazioni, parziale, incognite, soluzioneP, soluzioneQ, soluzione, globale},
                    max = Max[g];
                    min = Min[g];
                    impulsi = Impulsi[g];
                                
                    equazioni = Table[
                                parziale=Join[Cases[impulsi,p[a_,i]], -Cases[impulsi,p[i,a_]]];
                                Apply[Plus, parziale] == 0,
                                {i,min,-1}
                        ];
                    equazioni = equazioni //. { p[i_ /; i>0,j_] -> p[i]};
                    incognite = Cases[ equazioni, p[a_,b_], Infinity];
                    globale = p[max] -> - Sum[p[i], {i,1,max-1}] ;
                    equazioni = equazioni /. globale ;
                    soluzioneP = Solve[equazioni, incognite];
                    soluzioneP = Append[soluzioneP, globale];
                    soluzioneQ = soluzioneP /. {p -> Q};
                    soluzione = Flatten[Join[ soluzioneP, soluzioneQ],1];
                    soluzione
];
 
 
 
 
 Vestigrafo[g_] := Module[ {max, min,v,  vertici, grafo, soluzione, esterne, propr, propagatore, propagatori, leggi, properties, propProperties,pr, function},
                          max = Max[g];
                          min = Min[g]; 
			 
                          leggi = LeggiConservazione[g];
                          
                          vertici = Table[
                                v = Join[ Cases[g, {i,a_}], Cases[g, {b_,i}]];
                                impulsi = Impulsi[v];
                                cariche = Cariche[v];
                                lorentz = Lorentz[v];
                                masse= Masse[v];
                                impulsi = impulsi /. p[a_, b_] :> p[a] /; a > 0  && b < 0;
                                cariche = cariche /. Q[a_, b_] :> Q[a] /; a > 0  && b < 0;
                                lorentz = lorentz /. l[a_, b_] :> l[a] /; a > 0  && b < 0;
                                masse = masse /. m[a_, b_] :> m[a] /; a>0 && b<0;
                                impulsi = impulsi /. p[i, b_]  :>  - p[i,b];
                                cariche = cariche /. Q[i,b_] :>  - Q[i,b];
                                masse = masse /. m[i,b_] :> -m[i,b];
                                lorentz = lorentz /. l[i, b_] :> l[b];
                                lorentz = lorentz /. l[b_, i] :> l[b];
                                properties = (lorentz+impulsi+cariche +masse ) /. Plus -> List;
                                properties = properties //. leggi;
                                vert= Apply[vertice[i], properties],
                                {i,min, -1}
                             ];
                          
                          esterne = Cases[g, {a_,b_} /; a>0];
                          proprQ = esterne /. {{a_,b_} :> {a}};
                          esterne = Map[ Apply[lineaext, #] &, esterne];
                          proprQ= Map[Apply[Q,#] &, proprQ];
                          proprP= proprQ //. Q -> p;
                          proprM= proprQ //. Q -> m;
                          proprL= proprQ //. Q -> l;
                          extProperties=(proprL+proprQ+proprP+ proprM) /. Plus -> List;
                          ext =  Table[
                                    Apply[esterne[[i]], extProperties[[i]]],
                                    {i,1,Length[esterne]}
                                ];
                          
                          
                          propagatori = Cases[g, {a_, b_} /; a <0 && b < 0];
                          propagatori = Map[Apply[prop,#] &, propagatori];
                          proprQ= Map[Apply[Q,#] &, propagatori];
                          proprM = proprQ //. Q -> m;
                          proprP= proprQ //. Q -> p;
                          proprL= proprQ //. Q[a_, b_] :> function[l[a],l[b]];
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
 

 

               
                           
                           
                           
                           
                           
                           
                           
                          
                          
    
                          
(* ASSEGNAZIONE DEI PROCESSI ************************************* *)
                          
                          
                          


                           
AssegnaProcessoSQED[a_,b_, g_] := Module[ {soluzione,numIniziali, numFinali, subs1, subs2, subs,i},
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
                                        soluzione = g /. subs;
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
                          
     
                          
                        
                          

Process[a_, b_] := Module[{g,leggi,diagrammi, NExt, Controllo, lista, listaV,c},
                        NExt= Length[a]+Length[b];
                          c = - b ;
                          
                        Controllo = Apply[Plus,a] + Apply[Plus,c];
                        If[Controllo === {0,0},
                           lista = GeneraGrafi[NExt];
                           listaV = Map[Vestigrafo, lista];
                           proposta = Map[AssegnaProcessoSQED[a,c,#]&, listaV];
                           diagrammi = Map[ControllaDiagrammiSQED[#]&,proposta];
                           eliminare = Position[diagrammi, {}];
                           diagrammi=Delete[diagrammi, eliminare],
                           diagrammi=0;
                        ];
diagrammi
];
               
                           
                           
                           
                           
                           
                           
 
(*ASSEGNAZIONE DELLE REGOLE DI FEYNMAN ************************ *)

AssegnaRegoleSQED[d_, in_, out_] := Module[ {propfotoni, g, mu, soluzione, propscalari, esterneOut, esterneIn,
                                                   verticiTre, veticiQuattro},
             
            esterneIn = Cases[d, lineaext[a_,e_][b__,f_] /; a<=in];
            esterneIn = Map[RegoleEsterneInSQED,esterneIn];
            esterneOut = Cases[d, lineaext[a_,e_][b__,f_] /; a>in];
            esterneOut = Map[RegoleEsterneOutSQED, esterneOut];
            propscalari = Cases[d, prop[a_,b_][c__,f_] /; f =!= 0];
            propscalari = Map[propagatoreScalareSQED, propscalari];
            propfotoni = Cases[d, prop[a_,b_][c__,0]];
            propfotoni = Map[propagatoreFotoneSQED, propfotoni];
                                                  
            verticiTre = Cases[d, vertice[a_][b___] /; Length[vertice[a][b]] === 3 ];
            verticiTre = Map[RegoleTreSQED, verticiTre];
            verticiQuattro =Cases[d, vertice[a_][b___] /; Length[vertice[a][b]] === 4 ];
            verticiQuattro = Map[RegoleQuattroSQED, verticiQuattro];
            soluzione =  Apply[Times,esterneIn] Apply[Times,esterneOut] Apply[Times,propfotoni] Apply[Times, propscalari] Apply[Times, verticiTre] Apply[Times, verticiQuattro];
                                                    
    soluzione
];

                                                      
RegoleEsterneInSQED[a_] := Module[ {soluzione},
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
];
                                                      
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
     

ManipolaIndici[a_] := Module[ {soluzione, subs },
                    SetAttributes[g,Orderless];
                    soluzione = a;
                    subs := { b___  g[l[c_],l[d_]] g[l[f_],l[d_]] -> b  g[l[c],l[f]],
                            b___  g[l[c_],l[d_]] f_[l[c_]] -> b  f[l[d]],
                            b___ c_[l[d_]] f_[l[d_]]  -> b  SP[c,f]};
                    SetAttributes[SP, Orderless];
                    soluzione = soluzione //. subs;
soluzione
];
                        
ProdottoComplessoSQED[amp1_,amp2_, quantifotoni_] := Module[ {soluzione1, soluzione2, list1, list2,unit1, unit2, unit, subs1, subs2}, (* il secondo prende la coniugazione *)
        
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
                                          
                
           unit = unit1 Conjugate[unit2];
           soluzione = (soluzione1 soluzione2 unit) ;
           subs4 := {(a___ SP[eps[b_], c_] d___ SP[eps[b_], e_]) -> (-1  a d SP[c,e])};
           subs5 := {(a___ SP[Conjugate[eps[b_]], c_] d___ SP[Conjugate[eps[b_]], e_]) -> (-1 a d SP[c,e])};
           subs6 := {(a___ SP[Conjugate[eps[b_]], c_]^2  ) -> (-1 a  SP[c,c])};
           subs7 :={(a___ SP[eps[b_], c_]^2 ) -> (-1 a  SP[c,c])};
           subs1 := {a___ SP[Conjugate[eps[b_]], eps[e_]]^2 -> 4 a  };
           subs2 :={a___ SP[Conjugate[eps[b_]], Conjugate[eps[e_]]]^2 -> 4 a  };
           subs3 :={a___ SP[eps[b_], eps[e_]]^2 ->  4 a  };
            
                                                            
           soluzione = soluzione //. subs1;
           soluzione = soluzione //. subs2;
           soluzione = soluzione //. subs3;
           soluzione = soluzione //. subs4;
           soluzione = soluzione //. subs5;
           soluzione = soluzione //. subs6;
           soluzione = soluzione //. subs7;
            If[quantifotoni =!=0,
               soluzione = soluzione / (2 * quantifotoni),
               soluzione
             ];
            
soluzione
];
                           
                           
                           
                           
                           
                           
                           
                           
                           
                           
(*UTILE PER IDENTIFICARE LE PARTICELLE ******************************** *)
                          
                          
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
                           
 esegui[InState_, OutState_] := Module[ {processo,in, out, soluzione, quantiprodotti,prodotti, indice, quantifotoni},
    processo = Process[InState,OutState];
    in = Length[InState];
    out = Length[OutState];
    quantifotoni = Map[Apply[ParticellaIn,#]&, InState];
    quantifotoni = Count[quantifotoni, {"fotone ", b_}];
    processo= Map[AssegnaRegoleSQED[#,in,out]&,processo];
    processo = Map[ManipolaIndici, processo];
    soluzione = processo;
    quantiprodotti = Length[processo];
    indice=0;
    soluzione = {};
    Print["I got here !"];
    While[ indice < quantiprodotti,
         prodotti=Map[ProdottoComplessoSQED[processo[[indice+1]], #, quantifotoni] &, processo];
        Print["first set multiplied"];
         soluzione = Append[soluzione, prodotti];
          Print[indice];
          indice +=1;
          ];
  
    soluzione = Flatten[soluzione];

    soluzione = Apply[ Plus, soluzione];
    soluzione
];
                    


                          

                           

