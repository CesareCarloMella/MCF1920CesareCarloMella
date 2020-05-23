
S1= {{p[1],0}, {p[2],1}};
S2 = {{  p[3], 0}, {p[1]+p[2] - p[3], 1}};

(*S1 = {{p[1],1}, {p[2],-1}};
S2 = {{  p[3], 1}, {p[1]+p[2] - p[3], -1}};*)

(*S1 = {{p[1],1}, {p[2],-1}};
S2 = {{p[3], 1}, {p[4], -1}, {p[1]+p[2]-p[3]-p[4],0}};*)



(*GENERAZIONE DEI GRAFI ********************************        *)
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
                                globale = p[max] -> -Sum[p[i], {i,1,max-1}] ;
                                equazioni = equazioni /. globale ;
                                soluzioneP = Solve[equazioni, incognite];
                                soluzioneP = Append[soluzioneP, globale];
                                soluzioneQ = soluzioneP /. {p -> Q};
                                soluzione = Flatten[Join[ soluzioneP, soluzioneQ],1];
                                soluzione
                                ];
 
 
 
 
 Vestigrafo[g_] := Module[ {max, min,v,  vertici, grafo, soluzione, esterne, propr, propagatore, propagatori, leggi, properties, propProperties,pr},
                          max = Max[g];
                          min = Min[g]; 
			 
                          leggi = LeggiConservazione[g];
                          
                          vertici = Table[
                                v = Join[ Cases[g, {i,a_}], Cases[g, {b_,i}]];
                                impulsi = Impulsi[v];
                                cariche = Cariche[v];
                                lorentz = Lorentz[v];
                                impulsi = impulsi /. p[a_, b_] :> p[a] /; a > 0  && b < 0;
                                cariche = cariche /. Q[a_, b_] :> Q[a] /; a > 0  && b < 0;
                                lorentz = lorentz /. l[a_, b_] :> l[a] /; a > 0  && b < 0;
                                impulsi = impulsi /. p[i, b_] :>  - p[i,b];
                                cariche = cariche /. Q[i,b_] :>  - Q[i,b];
                                lorentz = lorentz /. l[i, b_] :> l[b];
                                lorentz = lorentz /. l[b_, i] :> l[b];
                                properties = (lorentz+impulsi + cariche) /. Plus -> List;
                                properties = properties //. leggi;
                                vert= Apply[vertice[i], properties],
                                {i,min, -1}
                             ];
                          
                          esterne = Cases[g, {a_,b_} /; a>0];
                          proprQ = esterne /. {{a_,b_} :> {a}};
                          esterne = Map[ Apply[lineaext, #] &, esterne];
                          proprQ= Map[Apply[Q,#] &, proprQ];
                          proprP= proprQ //. Q -> p;
                          (*proprM= proprQ //. Q -> m;*)
                          proprL= proprQ //. Q -> l;
                          extProperties=(proprL+proprQ+proprP) /. Plus -> List;
                          ext =  Table[
                                    Apply[esterne[[i]], extProperties[[i]]],
                                    {i,1,Length[esterne]}
                                ];
                          
                          
                          propagatori = Cases[g, {a_, b_} /; a <0 && b < 0];
                          propagatori = Map[Apply[prop,#] &, propagatori];
                          proprQ= Map[Apply[Q,#] &, propagatori];
                          proprP= proprQ //. Q -> p;
                          proprL= proprQ //. Q[a_, b_] :> l[a][b];
                          propProperties=(proprL+proprP+proprQ) /. Plus -> List;
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
                    cond2=FreeQ[g, prop[a_,b_][c_,d_,e___] /; d>1 || d< -1];
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

AssegnaRegole[d_, in_, out_] := Module[ {propfotoni, g, mu, soluzione, propscalari, esterneOut, esterneIn,
                                                   verticiTre, veticiQuattro},
             
            esterneIn = Cases[d, lineaext[a_,e_][b_,c_,f_] /; a<=in];
            esterneIn = Map[RegoleEsterneIn,esterneIn];
            esterneOut = Cases[d, lineaext[a_,e_][b_,c_,f_] /; a>in];
            esterneOut = Map[RegoleEsterneOut, esterneOut];
            propscalari = Cases[d, prop[a_,b_][c_,e_,f_] /; e =!= 0];
            propscalari = Map[propagatoreScalare, propscalari];
            propfotoni = Cases[d, prop[a_,b_][c_,0,f_]];
            propfotoni = Map[propagatoreFotone, propfotoni];
                                                  
            verticiTre = Cases[d, vertice[a_][b___] /; Length[vertice[a][b]] === 3 ];
            verticiTre = Map[RegoleTre, verticiTre];
            verticiQuattro =Cases[d, vertice[a_][b___] /; Length[vertice[a][b]] === 4 ];
            verticiQuattro = Map[RegoleQuattro, verticiQuattro];
            soluzione =  Apply[Times,esterneIn] Apply[Times,esterneOut] Apply[Times,propfotoni] Apply[Times, propscalari] Apply[Times, verticiTre] Apply[Times, verticiQuattro];
                                                    
    soluzione
];

                                                      
RegoleEsterneIn[a_] := Module[ {soluzione},
               If[a[[3]] =!= 0,
                  soluzione = 1,
                  soluzione = eps[a[[1]], a[[2]]]
                ];
 soluzione
 ];
                                                                                
 RegoleEsterneOut[a_] := Module[ {soluzione},
       If[a[[3]] =!= 0,
              soluzione = 1,
              soluzione = Conjugate[eps[a[[1]],a[[2]]]]
        ];
soluzione
];
                                                      
 propagatoreFotone[a_] := Module[ {soluzione, tens},
       tens = a[[3]] /. l -> g;
       soluzione = - I tens / a[[1]]^2;
soluzione
];
                                                                               
propagatoreScalare[a_] := Module[ {soluzione},
       soluzione =  I/(a[[1]]^2 - m^2 );
soluzione
 ];
                                                     

RegoleTre[a_] := Module[ {soluzione, ferm1, ferm2, fot, diff},
      ferm1=Flatten[ Cases[a, {b_,c_, -1}]];
      ferm2 = Flatten[Cases[a, {b_,c_, 1}]];
      fot = Flatten[Cases[a,{b_,c_,0}]];
      diff = ferm1[[2]] - ferm2[[2]];
      soluzione = I e diff[fot[[1]]];
                                                   
soluzione
];
                                                                                 

RegoleQuattro[a_] := Module[ {soluzione, ferm, tens, indici},
    ferm = Cases[a, {b_, c_,d_} /; d =!= 0];
    If[Length[ferm] === 4,
       soluzione = - I Lambda,
       indici = Cases[a, {b_,c_,0}];
       indici= indici /. {b_,c_,0} -> b;
       tens = g[indici[[1]]][indici[[2]]];
       soluzione = I e^2 tens
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
                                       soluzione = {"fotone",  -a},
                                       b===-1,
                                       soluzione = {"leptone -", -a},
                                       b===+1,
                                      soluzione = {"leptone +", -a}
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
                           
 esegui[InState_, OutState_] := Module[ {processo,in, out, soluzione},
    processo = Process[InState,OutState];
    in = Length[InState];
    out = Length[OutState];
    soluzione = Map[AssegnaRegole[#,in,out]&,processo];
    
    soluzione
];
                    


                          

                           

