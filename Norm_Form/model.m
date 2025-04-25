aF1afterF2[U1War_, U1Cap1_, \[Lambda]_, \[Theta]1a_] := 
  Sqrt[E^(\[Lambda] U1War)/(
    E^(\[Lambda] U1War) + E^(\[Lambda] U1Cap1))] E^(I Re[\[Theta]1a]) ;
aNF1afterF2[U1War_, U1Cap1_, \[Lambda]_, \[Theta]1a_] := 
  Sqrt[E^(\[Lambda] U1Cap1)/(
    E^(\[Lambda] U1War) + E^(\[Lambda] U1Cap1))] E^(I Re[\[Theta]1a]) ;
aF1afterNF2[U1Cap2_, U1Nego_, \[Lambda]_, \[Theta]1b_] := 
  Sqrt[E^(\[Lambda] U1Cap2)/(
    E^(\[Lambda] U1Cap2) + E^(\[Lambda] U1Nego))] E^(
   I Re[\[Theta]1b]) ;
aNF1afterNF2[U1Cap2_, U1Nego_, \[Lambda]_, \[Theta]1b_] := 
  Sqrt[E^(\[Lambda] U1Nego)/(
    E^(\[Lambda] U1Cap2) + E^(\[Lambda] U1Nego))] E^(
   I Re[\[Theta]1b]) ;
aF2afterF1[U2War_, U2Cap2_, \[Lambda]_, \[Theta]2a_] := 
  Sqrt[E^(\[Lambda] U2War)/(
    E^(\[Lambda] U2War) + E^(\[Lambda] U2Cap2))] E^(I Re[\[Theta]2a]) ;
aNF2afterF1[U2War_, U2Cap2_, \[Lambda]_, \[Theta]2a_] := 
  Sqrt[E^(\[Lambda] U2Cap2)/(
    E^(\[Lambda] U2War) + E^(\[Lambda] U2Cap2))] E^(I Re[\[Theta]2a]) ;
aF2afterNF1[U2Cap1_, U2Nego_, \[Lambda]_, \[Theta]2b_] := 
  Sqrt[E^(\[Lambda] U2Cap1)/(
    E^(\[Lambda] U2Cap1) + E^(\[Lambda] U2Nego))] E^(
   I Re[\[Theta]2b]) ;
aNF2afterNF1[U2Cap1_, U2Nego_, \[Lambda]_, \[Theta]2b_] := 
  Sqrt[E^(\[Lambda] U2Nego)/(
    E^(\[Lambda] U2Cap1) + E^(\[Lambda] U2Nego))] E^(I Re[\[Theta]2b])
    ;
    
aF1 [pF2norm_, aF1afterF2_, 
   aF1afterNF2_] := (Sqrt[pF2norm]*aF1afterF2) + (Sqrt[1 - pF2norm]*
     aF1afterNF2);
pF1 [aF1_] := aF1 * Conjugate[aF1];

aNF1[pF2norm_, aNF1afterF2_, 
   aNF1afterNF2_] := (Sqrt[pF2norm]*aNF1afterF2) + (Sqrt[1 - pF2norm]*
     aNF1afterNF2);
pNF1[aNF1_] := aNF1 * Conjugate[aNF1];

pF1norm [pF1_, pNF1_] := pF1 / (pF1 + pNF1);
pNF1norm [pF1_, pNF1_] := pNF1 / (pF1 + pNF1);

(* pF1 = pF2 * pF1afterF2 + pNF2 * pF1afterNF2 + 2*Sqrt[pF2*pNF2]*aF1afterF2*aF1afterNF2*Cos[\[Theta]1a-\[Theta]1b]
*)

(* pF1 depends only phase difference \[CapitalDelta]\[Theta]1 = \[Theta]1a-\[Theta]1b, as expected *)

aF2 [pF1norm_, aF2afterF1_, 
   aF2afterNF1_] := (Sqrt[pF1norm]*aF2afterF1) + (Sqrt[1 - pF1norm]*
     aF2afterNF1);
pF2 [aF2_] := aF2 * Conjugate[aF2];

aNF2[pF1norm_, aNF2afterF1_, 
   aNF2afterNF1_] := (Sqrt[pF1norm]*aNF2afterF1) + (Sqrt[1 - pF1norm]*
     aNF2afterNF1);
pNF2[aNF2_] := aNF2 * Conjugate[aNF2];

pF2norm [pF2_, pNF2_] := pF2 / (pF2 + pNF2);
pNF2norm [pF2_, pNF2_] := pNF2 / (pF2 + pNF2);

(* that gives us two equations with two variables (pF1norm, pF2norm) => unique set of pF1norm, pF2norm satisfying the conditions
in our balanced dataset, we have 375 cases (War1, War2, Cap1, Cap2, Nego) of crisis game, we can look at this stage of IIG, without combining it with demands game
we can look for what combination of \[CapitalDelta]\[Theta]1 and \[CapitalDelta]\[Theta]2 does the model has the highest fit with the empirical observations *)

(* adding the demand game *)

(* to add the demand stage of the game (needed for the comparability to Signorino and de Mesquita), we proceed analogically to crisis game
 *)
 
(* 
demands game	D2	nD2
D1	U1Cris, U2Cris	U1Acq2, U2Acq2
nD1	U1Acq1, U2Acq1	U1SQ, U2SQ
*)


aD1afterD2[U1Cris_, U1Acq1_, \[Lambda]_, \[Theta]3a_] := 
  Sqrt[E^(\[Lambda] U1Cris)/(
    E^(\[Lambda] U1Cris) + E^(\[Lambda] U1Acq1))] E^(
   I Re[\[Theta]3a]) ;
aND1afterD2[U1Cris_, U1Acq1_, \[Lambda]_, \[Theta]3a_] := 
  Sqrt[E^(\[Lambda] U1Acq1)/(
    E^(\[Lambda] U1Cris) + E^(\[Lambda] U1Acq1))] E^(
   I Re[\[Theta]3a]) ;
aD1afterND2[U1Acq2_, U1SQ_, \[Lambda]_, \[Theta]3b_] := 
  Sqrt[E^(\[Lambda] U1Acq2)/(
    E^(\[Lambda] U1Acq2) + E^(\[Lambda] U1SQ))] E^(
   I Re[\[Theta]3b]) ;
aND1afterND2[U1Acq2_, U1SQ_, \[Lambda]_, \[Theta]3b_] := 
  Sqrt[E^(\[Lambda] U1SQ)/(
    E^(\[Lambda] U1Acq2) + E^(\[Lambda] U1SQ))] E^(
   I Re[\[Theta]3b]) ;
aD2afterD1[U2Cris_, U2Acq2_ , \[Lambda]_, \[Theta]4a_] := 
  Sqrt[E^(\[Lambda] U2Cris)/(
    E^(\[Lambda] U2Cris) + E^(\[Lambda] U2Acq2))] E^(
   I Re[\[Theta]4a]) ;
aND2afterD1[U2Cris_, U2Acq2_, \[Lambda]_, \[Theta]4a_] := 
  Sqrt[E^(\[Lambda] U2Acq2)/(
    E^(\[Lambda] U2Cris) + E^(\[Lambda] U2Acq2))] E^(I Re[\[Theta]4a]);
aD2afterND1[U2Acq1_, U2SQ_, \[Lambda]_, \[Theta]4b_] := 
  Sqrt[E^(\[Lambda] U2Acq1)/(
    E^(\[Lambda] U2Acq1) + E^(\[Lambda] U2SQ))] E^(
   I Re[\[Theta]4b]) ;
aND2afterND1[U2Acq1_, U2SQ_,  \[Lambda]_, \[Theta]4b_] := 
  Sqrt[E^(\[Lambda] U2SQ)/(
    E^(\[Lambda] U2Acq1) + E^(\[Lambda] U2SQ))] E^(I Re[\[Theta]4b]) ;
    

aD1 [pD2norm_, aD1afterD2_, 
   aD1afterND2_] := (Sqrt[pD2norm]*aD1afterD2) + (Sqrt[1 - pD2norm]*
     aD1afterND2);
pD1 [aD1_] := aD1 * Conjugate[aD1];

aND1[pD2norm_, aND1afterD2_, 
   aND1afterND2_] := (Sqrt[pD2norm]*aND1afterD2) + (Sqrt[1 - pD2norm]*
     aND1afterND2);
pND1[aND1_] := aND1 * Conjugate[aND1];

pD1norm [pD1_, pND1_] := pD1 / (pD1 + pND1);
pND1norm [pD1_, pND1_] := pND1 / (pD1 + pND1);

aD2 [pD1norm_, aD2afterD1_, 
   aD2afterND1_] := (Sqrt[pD1norm]*aD2afterD1) + (Sqrt[1 - pD1norm]*
     aD2afterND1);
pD2 [aD2_] := aD2 * Conjugate[aD2];

aND2[pD1norm_, aND2afterD1_, 
   aND2afterND1_] := (Sqrt[pD1norm]*aND2afterD1) + (Sqrt[1 - pD1norm]*
     aND2afterND1);
pND2[aND2_] := aND2 * Conjugate[aND2];

pD2norm [pD2_, pND2_] := pD2 / (pD2 + pND2);
pND2norm [pD2_, pND2_] := pND2 / (pD2 + pND2);


(* 
now there are 4 phases which we can to solve for to find the best fit with the empirical observations
\[CapitalDelta]\[Theta]1 ... phase connected to the decision to "fight/not fight" of the 1st player
\[CapitalDelta]\[Theta]2 ... phase connected to the decision to "fight/not fight" of the 2nd player
\[CapitalDelta]\[Theta]3 ... phase connected to the decision to "demand/not demand" of the 1st player
\[CapitalDelta]\[Theta]4 ... phase connected to the decision to "demand/not demand" of the 2nd player

we can set \[CapitalDelta]\[Theta]1=\[CapitalDelta]\[Theta]3 and \[CapitalDelta]\[Theta]2=\[CapitalDelta]\[Theta]4 if necessary
*)

Functions for Optimization

FindCrisisEquilibrium[u1War_, u1Cap1_, u1Cap2_, u1Nego_, u2War_,
  					u2Cap2_, u2Cap1_, u2Nego_,
  					lambda_, \[CapitalDelta]\[CapitalTheta]1_, \[CapitalDelta]\
\[CapitalTheta]2_, maxIter_ : 100, tolerance_ : 10^-6, 
  verbose_ : False] :=
 
 	Module[{pF1normCurrent = 0.5, pF2normCurrent = 0.5, pF1normNext, 
   pF2normNext,
   			a1F2, a1NF2, a1F1, a1NF1, p1F1, p1NF1, a2F1,
   			\[Theta]1a, \[Theta]1b, \[Theta]2a, \[Theta]2b, pWar, pCap2, 
   pCap1, pNego, a2NF1, a2F2, a2NF2, p2F2, p2NF2,
   			iter = 0, converged = False, iterationHistory = {}},
  
  	(*Set phase differences*)
  	\[Theta]1a = \[CapitalDelta]\[CapitalTheta]1/2;
  	\[Theta]1b = -\[CapitalDelta]\[CapitalTheta]1/2;
  	\[Theta]2a = \[CapitalDelta]\[CapitalTheta]2/2;
  	\[Theta]2b = -\[CapitalDelta]\[CapitalTheta]2/2;
  
  	pWar = Re[pF1normCurrent*pF2normCurrent];
  	pCap2 = Re[pF1normCurrent*(1 - pF2normCurrent)];
  	pCap1 = Re[(1 - pF1normCurrent)*pF2normCurrent];
  	pNego = Re[(1 - pF1normCurrent)*(1 - pF2normCurrent)];
  
  	(* keep a log for debugging purposes *)
  	If[verbose,
   	AppendTo[iterationHistory,
    			<|"Iteration" -> 0,
     			"pF1" -> Re[pF1normCurrent],
     			"pF2" -> Re[pF2normCurrent],
     			"\[Theta]1a" -> \[Theta]1a, "\[Theta]1b" -> \[Theta]1b,
     			"\[CapitalDelta]\[CapitalTheta]1" -> \[CapitalDelta]\
\[CapitalTheta]1,
     			"\[Theta]2a" -> \[Theta]2a,
     			"\[Theta]2b" -> \[Theta]2b,
     			"\[CapitalDelta]\[CapitalTheta]2" -> \[CapitalDelta]\
\[CapitalTheta]2,
     			"pWar" -> Re[pWar],
     			"pCap2" -> Re[pCap2],
     			"pCap1" -> Re[pCap1],
     			"pNego" -> Re[pNego]|>
    	]
   	];
  	
  	While[iter < maxIter && ! converged,
   	
   	(*Calculate amplitudes for player 1*)
   	a1F2 = aF1afterF2[u1War, u1Cap1, lambda, \[Theta]1a];
   	a1NF2 = aNF1afterF2[u1War, u1Cap1, lambda, \[Theta]1a];
   	a1F1 = 
    aF1[pF2normCurrent, a1F2, 
     aF1afterNF2[u1Cap2, u1Nego, lambda, \[Theta]1b]];
   	a1NF1 = 
    aNF1[pF2normCurrent, a1NF2, 
     aNF1afterNF2[u1Cap2, u1Nego, lambda, \[Theta]1b]];
   	p1F1 = pF1[a1F1];
   	p1NF1 = pNF1[a1NF1];
   	pF1normNext = pF1norm[p1F1, p1NF1];
   
   	(*Calculate amplitudes for player 2*)
   	a2F1 = aF2afterF1[u2War, u2Cap2, lambda, \[Theta]2a];
   	a2NF1 = aNF2afterF1[u2War, u2Cap2, lambda, \[Theta]2a];
   	a2F2 = 
    aF2[pF1normCurrent, a2F1, 
     aF2afterNF1[u2Cap1, u2Nego, lambda, \[Theta]2b]];
   	a2NF2 = 
    aNF2[pF1normCurrent, a2NF1, 
     aNF2afterNF1[u2Cap1, u2Nego, lambda, \[Theta]2b]];
   	p2F2 = pF2[a2F2];
   	p2NF2 = pNF2[a2NF2];
   	pF2normNext = pF2norm[p2F2, p2NF2];
   	pWar = Re[pF1normCurrent*pF2normCurrent];
            pCap2 = Re[pF1normCurrent*(1 - pF2normCurrent)];
            pCap1 = Re[(1 - pF1normCurrent)*pF2normCurrent]; 
            pNego = Re[(1 - pF1normCurrent)*(1 - pF2normCurrent)];
   
   	(*Calculate convergence before updating values*)
   	converged = 
    Abs[pF1normNext - pF1normCurrent] < tolerance && 
     Abs[pF2normNext - pF2normCurrent] < tolerance;
   
   	(*Track iteration if verbose*)
   	If[verbose,
    	AppendTo[iterationHistory,
     	<|"Iteration" -> iter,
      	"pF1" -> Re[pF1normCurrent],
      	"pF2" -> Re[pF2normCurrent],
      	"\[Theta]1a" -> \[Theta]1a,
      	"\[Theta]1b" -> \[Theta]1b,
      	"\[CapitalDelta]\[CapitalTheta]1" -> \[CapitalDelta]\
\[CapitalTheta]1,
      	"\[Theta]2a" -> \[Theta]2a,
      	"\[Theta]2b" -> \[Theta]2b,
      	"\[CapitalDelta]\[CapitalTheta]2" -> \[CapitalDelta]\
\[CapitalTheta]2,
      	"\[CapitalDelta]pF1" -> Abs[pF1normNext - pF1normCurrent],
      	"\[CapitalDelta]pF2" -> Abs[pF2normNext - pF2normCurrent],
      	"pWar" -> pWar,
      	"pCap2" -> pCap2,
      	"pCap1" -> pCap1,
      	"pNego" -> pNego|>
     	]];
   
   	(*Update for next iteration*)
   	pF1normCurrent = Re[pF1normNext];
   	pF2normCurrent = Re[pF2normNext];
   	iter++;
   ];
  
  	(*Return equilibrium probabilities and outcomes*)
  	<|"Converged" -> converged,
   	"Iterations" -> iter,
   	"pF1" -> Re[pF1normCurrent],
   	"pF2" -> Re[pF2normCurrent],
   	"pWar" -> pWar,
   	"pCap2" -> pCap2,
   	"pCap1" -> pCap1,
   	"pNego" -> pNego,
   	"IterationHistory" -> If[verbose, iterationHistory, Null]|>
  ]

(*Full model with both crisis and demands games*)
(*Full model with both crisis and demands games*)
FindFullEquilibrium[u1War_, u1Cap1_, u1Cap2_, u1Nego_, u1SQ_, u1Acq1_,
   u1Acq2_, u2War_, u2Cap2_, u2Cap1_, u2Nego_, u2SQ_, u2Acq1_, 
  u2Acq2_, 
  lambda_, \[CapitalDelta]\[CapitalTheta]1_, \[CapitalDelta]\
\[CapitalTheta]2_, \[CapitalDelta]\[CapitalTheta]3_, \[CapitalDelta]\
\[CapitalTheta]4_, maxIter_ : 100, tolerance_ : 10^-6, 
  verbose_ : False] := 
 Module[{crisisEq, u1Cris, u2Cris, pD1normCurrent = 0.5, 
   pD2normCurrent = 0.5, pD1normNext, pD2normNext, a1D2, a1ND2, a1D1, 
   a1ND1, p1D1, p1ND1, a2D1, a2ND1, a2D2, a2ND2, p2D2, p2ND2, 
   crisisPWar, crisisPCap1, crisisPCap2, 
   crisisPNego, \[Theta]3a, \[Theta]3b, \[Theta]4a, \[Theta]4b, 
   pCrisis, pAcq2, pAcq1, pSQ, iter = 0, converged = False, 
   iterationHistory = {}},(*Set phase differences for demands game*)\
\[Theta]3a = \[CapitalDelta]\[CapitalTheta]3/2;
  \[Theta]3b = -\[CapitalDelta]\[CapitalTheta]3/2;
  \[Theta]4a = \[CapitalDelta]\[CapitalTheta]4/2;
  \[Theta]4b = -\[CapitalDelta]\[CapitalTheta]4/2;
  (*First find crisis game equilibrium*)
  crisisEq = 
   FindCrisisEquilibrium[u1War, u1Cap1, u1Cap2, u1Nego, u2War, u2Cap2,
     u2Cap1, u2Nego, 
    lambda, \[CapitalDelta]\[CapitalTheta]1, \[CapitalDelta]\
\[CapitalTheta]2, maxIter, tolerance, verbose];
  (*Extract the individual probability values*)
  crisisPWar = crisisEq["pWar"];
  crisisPCap1 = crisisEq["pCap1"];
  crisisPCap2 = crisisEq["pCap2"];
  crisisPNego = crisisEq["pNego"];
  (*Calculate expected crisis utilities*)
  u1Cris = 
   crisisPWar*u1War + crisisPCap2*u1Cap2 + crisisPCap1*u1Cap1 + 
    crisisPNego*u1Nego;
  u2Cris = 
   crisisPWar*u2War + crisisPCap2*u2Cap2 + crisisPCap1*u2Cap1 + 
    crisisPNego*u2Nego;
  (*Initialize demand game probabilities*)
  pCrisis = Re[pD1normCurrent*pD2normCurrent];
  pAcq2 = Re[pD1normCurrent*(1 - pD2normCurrent)];
  pAcq1 = Re[(1 - pD1normCurrent)*pD2normCurrent];
  pSQ = Re[(1 - pD1normCurrent)*(1 - pD2normCurrent)];
  (*Record initial state if verbose*)
  If[verbose, 
   AppendTo[
    iterationHistory, <|"Iteration" -> 0, "pD1" -> Re[pD1normCurrent],
      "pD2" -> Re[pD2normCurrent], "\[Theta]3a" -> \[Theta]3a, 
     "\[Theta]3b" -> \[Theta]3b, 
     "\[CapitalDelta]\[CapitalTheta]3" -> \[CapitalDelta]\
\[CapitalTheta]3, "\[Theta]4a" -> \[Theta]4a, 
     "\[Theta]4b" -> \[Theta]4b, 
     "\[CapitalDelta]\[CapitalTheta]4" -> \[CapitalDelta]\
\[CapitalTheta]4, "pCrisis" -> pCrisis, "pAcq2" -> pAcq2, 
     "pAcq1" -> pAcq1, "pSQ" -> pSQ|>]];
  (*Now find demands game equilibrium*)
  While[iter < maxIter && ! 
     converged,(*Calculate amplitudes for player 1*)
   a1D2 = aD1afterD2[u1Cris, u1Acq1, lambda, \[Theta]3a];
   a1ND2 = aND1afterD2[u1Cris, u1Acq1, lambda, \[Theta]3a];
   a1D1 = 
    aD1[pD2normCurrent, a1D2, 
     aD1afterND2[u1Acq2, u1SQ, lambda, \[Theta]3b]];
   a1ND1 = 
    aND1[pD2normCurrent, a1ND2, 
     aND1afterND2[u1Acq2, u1SQ, lambda, \[Theta]3b]];
   p1D1 = pD1[a1D1];
   p1ND1 = pND1[a1ND1];
   pD1normNext = pD1norm[p1D1, p1ND1];
   (*Calculate amplitudes for player 2*)
   a2D1 = aD2afterD1[u2Cris, u2Acq2, lambda, \[Theta]4a];
   a2ND1 = aND2afterD1[u2Cris, u2Acq2, lambda, \[Theta]4a];
   a2D2 = 
    aD2[pD1normCurrent, a2D1, 
     aD2afterND1[u2Acq1, u2SQ, lambda, \[Theta]4b]];
   a2ND2 = 
    aND2[pD1normCurrent, a2ND1, 
     aND2afterND1[u2Acq1, u2SQ, lambda, \[Theta]4b]];
   p2D2 = pD2[a2D2];
   p2ND2 = pND2[a2ND2];
   pD2normNext = pD2norm[p2D2, p2ND2];
   (*Update outcome probabilities*)
   pCrisis = Re[pD1normCurrent*pD2normCurrent];
   pAcq2 = Re[pD1normCurrent*(1 - pD2normCurrent)];
   pAcq1 = Re[(1 - pD1normCurrent)*pD2normCurrent];
   pSQ = Re[(1 - pD1normCurrent)*(1 - pD2normCurrent)];
   (*Check convergence*)
   converged = 
    Abs[pD1normNext - pD1normCurrent] < tolerance && 
     Abs[pD2normNext - pD2normCurrent] < tolerance;
   (*Track iteration if verbose*)
   If[verbose, 
    AppendTo[
     iterationHistory, <|"Iteration" -> iter, 
      "pD1" -> Re[pD1normCurrent], "pD2" -> Re[pD2normCurrent], 
      "\[Theta]3a" -> \[Theta]3a, "\[Theta]3b" -> \[Theta]3b, 
      "\[CapitalDelta]\[CapitalTheta]3" -> \[CapitalDelta]\
\[CapitalTheta]3, "\[Theta]4a" -> \[Theta]4a, 
      "\[Theta]4b" -> \[Theta]4b, 
      "\[CapitalDelta]\[CapitalTheta]4" -> \[CapitalDelta]\
\[CapitalTheta]4, 
      "\[CapitalDelta]pD1" -> Abs[pD1normNext - pD1normCurrent], 
      "\[CapitalDelta]pD2" -> Abs[pD2normNext - pD2normCurrent], 
      "pCrisis" -> pCrisis, "pAcq2" -> pAcq2, "pAcq1" -> pAcq1, 
      "pSQ" -> pSQ|>]];
   (*Update for next iteration*)pD1normCurrent = Re[pD1normNext];
   pD2normCurrent = Re[pD2normNext];
   iter++;];
  (*Return full equilibrium probabilities and outcomes*)<|
   "CrisisEquilibrium" -> <|"pWar" -> crisisPWar, 
     "pCap1" -> crisisPCap1, "pCap2" -> crisisPCap2, 
     "pNego" -> crisisPNego, "pF1" -> crisisEq["pF1"], 
     "pF2" -> crisisEq["pF2"], "Converged" -> crisisEq["Converged"], 
     "Iterations" -> crisisEq["Iterations"]|>, 
   "DemandsConverged" -> converged, "DemandsIterations" -> iter, 
   "pD1" -> Re[pD1normCurrent], "pD2" -> Re[pD2normCurrent], 
   "pCrisis" -> pCrisis, "pAcq2" -> pAcq2, "pAcq1" -> pAcq1, 
   "pSQ" -> pSQ, 
   "ExpectedUtility1" -> 
    pCrisis*u1Cris + pAcq2*u1Acq2 + pAcq1*u1Acq1 + pSQ*u1SQ, 
   "ExpectedUtility2" -> 
    pCrisis*u2Cris + pAcq2*u2Acq2 + pAcq1*u2Acq1 + pSQ*u2SQ, 
   "DemandsIterationHistory" -> If[verbose, iterationHistory, Null]|>]

   (*Grid search to find optimal phase differences*)
(* make the optimization for 4 parmsinstead of 8 *)
FindOptimalPhases[dataSet_, lambda_, phaseGridSize_ : 10] := 
  Module[{phaseValues, bestFit = -Infinity, bestPhases, currentFit, 
    results},
   
   (*Create grid of phase values*)
   phaseValues = 
    Table[N[2 \[Pi]*i/phaseGridSize], {i, 0, phaseGridSize - 1}];
   
   (*Grid search*)
   bestPhases = {};
   
   (*For each combination of phases*)
   Do[
    
    (*Get equilibrium for this case*)
    results = Table[
      With[{row = dataSet[[i]]},
       FindFullEquilibrium[row["U1War"], row["U1Cap1"], 
        row["U1Cap2"],
        				      row["U1Nego"], row["U1SQ"], row["U1Acq1"], 
        row["U1Acq2"],
        				       row["U2War"], row["U2Cap2"], row["U2Cap1"], 
        row["U2Nego"],
        				       row["U2SQ"], row["U2Acq1"], row["U2Acq2"], 
        lambda, \[Theta]1a, \[Theta]1b, \[Theta]2a, \[Theta]2b, \
\[Theta]3a, \[Theta]3b, \[Theta]4a, \[Theta]4b]], {i, 
       Length[dataSet]}];
    
    (*Calculate fit using log-likelihood*)
    currentFit =
     Sum[
      With[{row = dataSet[[i]], result = results[[i]]},
       Which[row["groundtruth"] == "War",
        		Log[result["CrisisEquilibrium"]["pWar"]],
        	row["groundtruth"] == "Cap1",
        		Log[result["CrisisEquilibrium"]["pCap1"]],
        	row["groundtruth"] == "Cap2",
        		Log[result["CrisisEquilibrium"]["pCap2"]],
        	row["groundtruth"] == "Nego",
        		Log[result["CrisisEquilibrium"]["pNego"]],
        	row["groundtruth"] == "SQ",
        		Log[result["pSQ"]],
        	row["groundtruth"] == "Acq1",
        	Log[result["pAcq1"]],
        	row["groundtruth"] == "Acq2",
        		Log[result["pAcq2"]], True, 0]], {i, Length[dataSet]}];
    
    (*Update best fit*)
    If[currentFit > bestFit, bestFit = currentFit;
     	bestPhases = {\[Theta]1a, \[Theta]1b, \[Theta]2a, \[Theta]2b, \
\[Theta]3a, \[Theta]3b, \[Theta]4a, \[Theta]4b};],
    
    (* iteration updates *)
    {\[Theta]1a, phaseValues},
    {\[Theta]1b, phaseValues},
    {\[Theta]2a, phaseValues},
    {\[Theta]2b, phaseValues},
    {\[Theta]3a, phaseValues},
    {\[Theta]3b, phaseValues},
    {\[Theta]4a, phaseValues},
    {\[Theta]4b, phaseValues}
    ];
   
   (*Return best phases and fit*)
   {"BestPhases" -> bestPhases,
    "LogLikelihood" -> bestFit,
    "Lambda" -> lambda}
   ];


Testing

data = Import[
  "/Users/162191/Documents/GitHub/quantum_like_international_\
interaction_game/Norm_Form/first_dataset_with_SQ.csv", "CSV", 
  "HeaderLines" -> 1]

  dataset = 
  Dataset[Association @@@ (Rule @@@ 
        Transpose[{{"Agent1", "Agent2", "wrTu1sq", "wrTu1ac1", 
           "wrTu1ac2", "wrTu1neg", "wrTu1cp1", "wrTu1cp2", "wrTu1wr1",
            "wrTu1wr2", "wrTu2sq", "wrTu2ac2", "wrTu2ac1", "wrTu2neg",
            "wrTu2cp2", "wrTu2cp1", "wrTu2wr2", "wrTu2wr1", 
           "groundtruth"}, #}] & /@ data)];


preparedData =
  Table[<|"Agent1" -> dataset[i, "Agent1"], 
    "Agent2" -> dataset[i, "Agent2"], 
    "U1War" -> (dataset[i, "wrTu1wr1"] + dataset[i, "wrTu1wr2"])/2, 
    "U1Cap1" -> dataset[i, "wrTu1cp1"], 
    "U1Cap2" -> dataset[i, "wrTu1cp2"], 
    "U1Nego" -> dataset[i, "wrTu1neg"], 
    "U1SQ" -> dataset[i, "wrTu1sq"], 
    "U1Acq1" -> dataset[i, "wrTu1ac1"], 
    "U1Acq2" -> dataset[i, "wrTu1ac2"], 
    "U2War" -> (dataset[i, "wrTu2wr1"] + dataset[i, "wrTu2wr2"])/2, 
    "U2Cap1" -> dataset[i, "wrTu2cp1"], 
    "U2Cap2" -> dataset[i, "wrTu2cp2"], 
    "U2Nego" -> dataset[i, "wrTu2neg"], 
    "U2SQ" -> dataset[i, "wrTu2sq"], 
    "U2Acq1" -> dataset[i, "wrTu2ac1"], 
    "U2Acq2" -> dataset[i, "wrTu2ac2"], 
    "groundtruth" -> dataset[i, "groundtruth"]|>, {i, 
    Length[dataset]}];

Single Case Test

sampleCase = preparedData[[1]]

result =
  FindCrisisEquilibrium[sampleCase["U1War"],
   					sampleCase["U1Cap1"],
   					sampleCase["U1Cap2"],
   					sampleCase["U1Nego"],
   					sampleCase["U2War"],
   					sampleCase["U2Cap2"],
   					sampleCase["U2Cap1"],
   					sampleCase["U2Nego"],
   					1.0, \[Pi], 3 \[Pi]/2, 100, 0.000001, True];

Print["Converged: ", result["Converged"]    ];
Print["Total Iterations: ", result["Iterations"]   ]; 
Print["Likelihood Player 1 Fighting: ", Re[result["pF1"]   ]]; 
Print["Likelihood Player 2 Fighting: ", Re[result["pF2"]   ]];  
Print["pWar: ", result["pWar"]   ];  
Print["pCap1: ", result["pCap1"]   ];  
Print["pCap2: ", result["pCap2"]   ];  
Print["pNego: ", result["pNego"]   ];  

Get SubCrisis Game Results

history = result["IterationHistory"];

ListPlot[{
  Table[{i, history[[i + 1, "pF1"]]},
   	  {i, 0, Length[history] - 1}],
  Table[{i, history[[i + 1, "pF2"]]}, {i, 0, Length[history] - 1}]},
 PlotLegends -> {"Player 1", "Player 2"}, 
 AxesLabel -> {"Iteration", "Probability"}, 
 PlotLabel -> "Convergence of Fight Probabilities", Joined -> True, 
 Mesh -> All, PlotMarkers -> Automatic]


ListPlot[{
  	 Table[{i, history[[i + 1, "pWar"]]}, {i, 0, 
    Length[history] - 1}],
  	 Table[{i, history[[i + 1, "pCap2"]]}, {i, 0, Length[history] - 1}],
  	 Table[{i, history[[i + 1, "pCap1"]]}, {i, 0, Length[history] - 1}],
  	 Table[{i, history[[i + 1, "pNego"]]}, {i, 0, 
    Length[history] - 1}]
  	},
 PlotLegends -> {"pWar", "pCap2", "pCap1", "pNego"}, 
 AxesLabel -> {"Iteration", "Probability"}, 
 PlotLabel -> "Convergence of Probabilities in SubCrisis Game", 
 Joined -> True, Mesh -> All, PlotMarkers -> Automatic]


 If[Length[history] > 1,
 ListLogPlot[{
   Table[{i, history[[i + 1, "\[CapitalDelta]pF1"]]},
    {i, 1, Length[history] - 1}],
   Table[{i, history[[i + 1, "\[CapitalDelta]pF2"]]},
    	{i, 1, Length[history] - 1}]},
  PlotLegends -> {"Player 1", "Player 2"}, 
  AxesLabel -> {"Iteration", "Log(\[CapitalDelta]p)"}, 
  PlotLabel -> "Convergence Rate", Joined -> True]]

  If[Length[history] > 1, 
 Grid[{{"Iteration", "pF1", "pF2", "\[CapitalDelta]pF1", 
     "\[CapitalDelta]pF2", "\[Theta]1a", "\[Theta]1b", "\[Theta]2a", 
     "\[Theta]2b", "\[CapitalDelta]\[CapitalTheta]1", 
     "\[CapitalDelta]\[CapitalTheta]2"}}~Join~
   Table[{history[[i, "Iteration"]],
     NumberForm[history[[i, "pF1"]], {4, 4}],
     NumberForm[history[[i, "pF2"]], {4, 4}],
     NumberForm[history[[i, "\[CapitalDelta]pF1"]], {4, 4}],
     NumberForm[history[[i, "\[CapitalDelta]pF2"]], {4, 4}],
     NumberForm[history[[i, "\[Theta]1a"]], {4, 4}],
     NumberForm[history[[i, "\[Theta]1b"]], {4, 4}],
     NumberForm[history[[i, "\[Theta]2a"]], {4, 4}],
     NumberForm[history[[i, "\[Theta]2b"]], {4, 4}],
     NumberForm[
      history[[i, "\[CapitalDelta]\[CapitalTheta]1"]], {4, 4}],
     NumberForm[
      history[[i, "\[CapitalDelta]\[CapitalTheta]2"]], {4, 4}]
     },
    {i, 2, Length[history]}],
  Frame -> All, Alignment -> Center, 
  Background -> {None, {LightGray, None}}]]


Get Full Game Results

fullResult = 
  FindFullEquilibrium[sampleCase["U1War"], sampleCase["U1Cap1"], 
   sampleCase["U1Cap2"], sampleCase["U1Nego"], sampleCase["U1SQ"], 
   sampleCase["U1Acq1"], sampleCase["U1Acq2"], sampleCase["U2War"], 
   sampleCase["U2Cap2"], sampleCase["U2Cap1"], sampleCase["U2Nego"], 
   sampleCase["U2SQ"], sampleCase["U2Acq1"], sampleCase["U2Acq2"], 
   1.0, 0, 0, 0, 0, 100, 0.000001, True];

(*Print Crisis Game Results*)
crisisEq = fullResult["CrisisEquilibrium"];
Print["Crisis Game Results:"];
Print["  Converged: ", crisisEq["Converged"]];
Print["  Total Iterations: ", crisisEq["Iterations"]];
Print["  Likelihood Player 1 Fighting: ", crisisEq["pF1"]];
Print["  Likelihood Player 2 Fighting:} ", crisisEq["pF2"]];
Print["  pWar: ", crisisEq["pWar"]];
Print["  pCap1: ", crisisEq["pCap1"]];
Print["  pCap2: ", crisisEq["pCap2"]];
Print["  pNego: ", crisisEq["pNego"]];

(*Print Demands Game Results*)
Print["\nDemands Game Results:"];
Print["  Converged: ", fullResult["DemandsConverged"]];
Print["  Total Iterations: ", fullResult["DemandsIterations"]];
Print["  Likelihood Player 1 Demanding: ", fullResult["pD1"]];
Print["  Likelihood Player 2 Demanding: ", fullResult["pD2"]];
Print["  pCrisis: ", fullResult["pCrisis"]];
Print["  pAcq1: ", fullResult["pAcq1"]];
Print["  pAcq2: ", fullResult["pAcq2"]];
Print["  pSQ: ", fullResult["pSQ"]];
Print["\nExpected Utilities:"];
Print["  Player 1: ", fullResult["ExpectedUtility1"]];
Print["  Player 2: ", fullResult["ExpectedUtility2"]];

demandsHistory = fullResult["DemandsIterationHistory"]

(*Plot convergence of demand probabilities*)ListPlot[{Table[{i, 
    demandsHistory[[i + 1, "pD1"]]}, {i, 0, 
    Length[demandsHistory] - 1}], 
  Table[{i, demandsHistory[[i + 1, "pD2"]]}, {i, 0, 
    Length[demandsHistory] - 1}]}, 
 PlotLegends -> {"Player 1", "Player 2"}, 
 AxesLabel -> {"Iteration", "Probability"}, 
 PlotLabel -> "Convergence of Demand Probabilities", Joined -> True, 
 Mesh -> All, PlotMarkers -> Automatic]


ListPlot[{Table[{i, demandsHistory[[i + 1, "pCrisis"]]}, {i, 0, 
    Length[demandsHistory] - 1}], 
  Table[{i, demandsHistory[[i + 1, "pAcq2"]]}, {i, 0, 
    Length[demandsHistory] - 1}], 
  Table[{i, demandsHistory[[i + 1, "pAcq1"]]}, {i, 0, 
    Length[demandsHistory] - 1}], 
  Table[{i, demandsHistory[[i + 1, "pSQ"]]}, {i, 0, 
    Length[demandsHistory] - 1}]}, 
 PlotLegends -> {"pCrisis", "pAcq2", "pAcq1", "pSQ"}, 
 AxesLabel -> {"Iteration", "Probability"}, 
 PlotLabel -> "Convergence of Probabilities in Demands Game", 
 Joined -> True, Mesh -> All, PlotMarkers -> Automatic]

 (*Plot convergence rate for demands game*)If[
 Length[demandsHistory] > 1, 
 ListLogPlot[{Table[{i, 
     demandsHistory[[i + 1, "\[CapitalDelta]pD1"]]}, {i, 1, 
     Length[demandsHistory] - 1}], 
   Table[{i, demandsHistory[[i + 1, "\[CapitalDelta]pD2"]]}, {i, 1, 
     Length[demandsHistory] - 1}]}, 
  PlotLegends -> {"Player 1", "Player 2"}, 
  AxesLabel -> {"Iteration", "Log(\[CapitalDelta]p)"}, 
  PlotLabel -> "Demand Game Convergence Rate", Joined -> True]]

  If[Length[demandsHistory] > 1, 
 Grid[{{"Iteration", "pD1", "pD2", "\[CapitalDelta]pD1", 
     "\[CapitalDelta]pD2", "\[Theta]3a", "\[Theta]3b", "\[Theta]4a", 
     "\[Theta]4b", "\[CapitalDelta]\[CapitalTheta]3", 
     "\[CapitalDelta]\[CapitalTheta]4"}}~Join~
   Table[{demandsHistory[[i, "Iteration"]], 
     NumberForm[demandsHistory[[i, "pD1"]], {4, 4}], 
     NumberForm[demandsHistory[[i, "pD2"]], {4, 4}], 
     If[i > 1, 
      NumberForm[demandsHistory[[i, "\[CapitalDelta]pD1"]], {4, 4}], 
      "-"], If[i > 1, 
      NumberForm[demandsHistory[[i, "\[CapitalDelta]pD2"]], {4, 4}], 
      "-"], NumberForm[demandsHistory[[i, "\[Theta]3a"]], {4, 4}], 
     NumberForm[demandsHistory[[i, "\[Theta]3b"]], {4, 4}], 
     NumberForm[demandsHistory[[i, "\[Theta]4a"]], {4, 4}], 
     NumberForm[demandsHistory[[i, "\[Theta]4b"]], {4, 4}], 
     NumberForm[
      demandsHistory[[i, "\[CapitalDelta]\[CapitalTheta]3"]], {4, 4}],
      NumberForm[
      demandsHistory[[i, "\[CapitalDelta]\[CapitalTheta]4"]], {4, 
       4}]}, {i, 1, Length[demandsHistory]}], Frame -> All, 
  Alignment -> Center, Background -> {None, {LightGray, None}}]]

  (*Create a summary table showing outcome probabilities for both \
games*)Grid[{{"Game Stage", "Outcome", "Probability"}, {"Crisis Game",
    "War", crisisEq["pWar"]}, {"Crisis Game", "Capitulation 1", 
   crisisEq["pCap1"]}, {"Crisis Game", "Capitulation 2", 
   crisisEq["pCap2"]}, {"Crisis Game", "Negotiation", 
   crisisEq["pNego"]}, {"Demands Game", "Crisis", 
   fullResult["pCrisis"]}, {"Demands Game", "Acquiescence 1", 
   fullResult["pAcq1"]}, {"Demands Game", "Acquiescence 2", 
   fullResult["pAcq2"]}, {"Demands Game", "Status Quo", 
   fullResult["pSQ"]}}, Frame -> All, 
 Alignment -> {Left, Left, Right}, 
 Background -> {{LightGray, None}, {LightGray, None, LightGray, None, 
    LightGray, None, LightGray, None, LightGray}}]