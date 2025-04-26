(* The International Interaction Backward Induction Model *)

(* A Quantum-Like Approach *)

(* Normal form approach *)

(* We propose we can understand IIG as the sequence of two normal-form game - demands and crisis games
In each of these two game, both actors have a strategic set of two strategies, from which they choose simultaneously *)

(* 

demands game	D2	nD2
D1	crisis game	U1Acq2, U2Acq2
nD1	U1Acq1, U2Acq1	U1SQ, U2SQ

 crisis game 	F2	nF2
F1	U1War, U2War	U1Cap2, U2Cap2
nF1	U1Cap1, U2Cap1	U1Nego, U2Nego

(we need to deal with the fact that we do not distinguish utilities of War1 and War2 in normal form game - we currently use an average of the two)

we take Signorino formula for the conditional probability of choosing between the strategies given their utilities and the expected strategy of the other player
p(F1|F2) = exp(lambda*U1War)/(exp(lambda*U1War)+exp(lambda*U1Cap1))

which results in following probability amplitudes

*)


(* =====CRISIS GAME AMPLITUDE FUNCTIONS=====*)
(*Player 1's amplitude for fighting after Player 2 fights*)

aF1afterF2[U1War_, U1Cap1_, \[Lambda]_, \[Theta]1a_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U1War);
   denominator = E^(\[Lambda] U1War) + E^(\[Lambda] U1Cap1);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]1a]);
   amplitude];

(*Player 1's amplitude for not fighting after Player 2 fights*)
aNF1afterF2[U1War_, U1Cap1_, \[Lambda]_, \[Theta]1a_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U1Cap1);
   denominator = E^(\[Lambda] U1War) + E^(\[Lambda] U1Cap1);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]1a]);
   amplitude];

(*Player 1's amplitude for fighting after Player 2 doesn't fight*)
aF1afterNF2[U1Cap2_, U1Nego_, \[Lambda]_, \[Theta]1b_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U1Cap2);
   denominator = E^(\[Lambda] U1Cap2) + E^(\[Lambda] U1Nego);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]1b]);
   amplitude];

(*Player 1's amplitude for not fighting after Player 2 doesn't fight*)
aNF1afterNF2[U1Cap2_, U1Nego_, \[Lambda]_, \[Theta]1b_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U1Nego);
   denominator = E^(\[Lambda] U1Cap2) + E^(\[Lambda] U1Nego);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]1b]);
   amplitude];

(*Player 2's amplitude for fighting after Player 1 fights*)
aF2afterF1[U2War_, U2Cap2_, \[Lambda]_, \[Theta]2a_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U2War);
   denominator = E^(\[Lambda] U2War) + E^(\[Lambda] U2Cap2);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]2a]);
   amplitude];

(*Player 2's amplitude for not fighting after Player 1 fights*)
aNF2afterF1[U2War_, U2Cap2_, \[Lambda]_, \[Theta]2a_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U2Cap2);
   denominator = E^(\[Lambda] U2War) + E^(\[Lambda] U2Cap2);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]2a]);
   amplitude];

(*Player 2's amplitude for fighting after Player 1 doesn't fight*)
aF2afterNF1[U2Cap1_, U2Nego_, \[Lambda]_, \[Theta]2b_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U2Cap1);
   denominator = E^(\[Lambda] U2Cap1) + E^(\[Lambda] U2Nego);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]2b]);
   amplitude];

(*Player 2's amplitude for not fighting after Player 1 doesn't fight*)
aNF2afterNF1[U2Cap1_, U2Nego_, \[Lambda]_, \[Theta]2b_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U2Nego);
   denominator = E^(\[Lambda] U2Cap1) + E^(\[Lambda] U2Nego);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]2b]);
   amplitude];


Crisis Game Probability Functions

(*Combined amplitude for Player 1 fighting*)
aF1[pF2norm_, aF1afterF2_, 
   aF1afterNF2_] := (Sqrt[pF2norm]*aF1afterF2) + (Sqrt[1 - pF2norm]*
     aF1afterNF2);

(*Probability of Player 1 fighting*)
pF1[aF1_] := aF1*Conjugate[aF1];

(*Combined amplitude for Player 1 not fighting*)
aNF1[pF2norm_, aNF1afterF2_, 
   aNF1afterNF2_] := (Sqrt[pF2norm]*aNF1afterF2) + (Sqrt[1 - pF2norm]*
     aNF1afterNF2);

(*Probability of Player 1 not fighting*)
pNF1[aNF1_] := aNF1*Conjugate[aNF1];

(*Normalized probability of Player 1 fighting*)
pF1norm[pF1_, pNF1_] := 
  Module[{denominator = pF1 + pNF1}, 
   If[denominator == 0., 0.5, 
    pF1/denominator] (*Avoid division by zero*)];

(*Normalized probability of Player 1 not fighting*)
pNF1norm[pF1_, pNF1_] := 
  Module[{denominator = pF1 + pNF1}, 
   If[denominator == 0., 0.5, 
    pNF1/denominator] (*Avoid division by zero*)];

(*Combined amplitude for Player 2 fighting*)
aF2[pF1norm_, aF2afterF1_, 
   aF2afterNF1_] := (Sqrt[pF1norm]*aF2afterF1) + (Sqrt[1 - pF1norm]*
     aF2afterNF1);

(*Probability of Player 2 fighting*)
pF2[aF2_] := aF2*Conjugate[aF2];

(*Combined amplitude for Player 2 not fighting*)
aNF2[pF1norm_, aNF2afterF1_, 
   aNF2afterNF1_] := (Sqrt[pF1norm]*aNF2afterF1) + (Sqrt[1 - pF1norm]*
     aNF2afterNF1);

(*Probability of Player 2 not fighting*)
pNF2[aNF2_] := aNF2*Conjugate[aNF2];

(*Normalized probability of Player 2 fighting*)
pF2norm[pF2_, pNF2_] := 
  Module[{denominator = pF2 + pNF2}, 
   If[denominator == 0., 0.5, 
    pF2/denominator] (*Avoid division by zero*)];

(*Normalized probability of Player 2 not fighting*)
pNF2norm[pF2_, pNF2_] := 
  Module[{denominator = pF2 + pNF2}, 
   If[denominator == 0., 0.5, 
    pNF2/denominator] (*Avoid division by zero*)];
    
pF1 = pF2 * pF1afterF2 + pNF2 * pF1afterNF2 + 2*Sqrt[pF2*pNF2]*aF1afterF2*aF1afterNF2*Cos[\[Theta]1a-\[Theta]1b]
pF1 depends only phase difference \[CapitalDelta]\[Theta]1 = \[Theta]1a-\[Theta]1b, as expected


that gives us two equations with two variables (pF1norm, pF2norm) => unique set of pF1norm, pF2norm satisfying the conditions
in our balanced dataset, we have 375 cases (War1, War2, Cap1, Cap2, Nego) of crisis game, we can look at this stage of IIG, without combining it with demands game
we can look for what combination of \[CapitalDelta]\[Theta]1 and \[CapitalDelta]\[Theta]2 does the model has the highest fit with the empirical observations

Demands Game

to add the demand stage of the game (needed for the comparability to Signorino and de Mesquita), we proceed analogically to crisis game

demands game	D2	nD2
D1	U1Cris, U2Cris	U1Acq2, U2Acq2
nD1	U1Acq1, U2Acq1	U1SQ, U2SQ


(*Player 1's amplitude for demanding after Player 2 demands*)
aD1afterD2[U1Cris_, U1Acq1_, \[Lambda]_, \[Theta]3a_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U1Cris);
   denominator = E^(\[Lambda] U1Cris) + E^(\[Lambda] U1Acq1);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]3a]);
   amplitude];

(*Player 1's amplitude for not demanding after Player 2 demands*)
aND1afterD2[U1Cris_, U1Acq1_, \[Lambda]_, \[Theta]3a_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U1Acq1);
   denominator = E^(\[Lambda] U1Cris) + E^(\[Lambda] U1Acq1);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]3a]);
   amplitude];

(*Player 1's amplitude for demanding after Player 2 doesn't demand*)
aD1afterND2[U1Acq2_, U1SQ_, \[Lambda]_, \[Theta]3b_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U1Acq2);
   denominator = E^(\[Lambda] U1Acq2) + E^(\[Lambda] U1SQ);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]3b]);
   amplitude];

(*Player 1's amplitude for not demanding after Player 2 doesn't \
demand*)
aND1afterND2[U1Acq2_, U1SQ_, \[Lambda]_, \[Theta]3b_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U1SQ);
   denominator = E^(\[Lambda] U1Acq2) + E^(\[Lambda] U1SQ);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]3b]);
   amplitude];

(*Player 2's amplitude for demanding after Player 1 demands*)
aD2afterD1[U2Cris_, U2Acq2_, \[Lambda]_, \[Theta]4a_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U2Cris);
   denominator = E^(\[Lambda] U2Cris) + E^(\[Lambda] U2Acq2);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]4a]);
   amplitude];

(*Player 2's amplitude for not demanding after Player 1 demands*)
aND2afterD1[U2Cris_, U2Acq2_, \[Lambda]_, \[Theta]4a_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U2Acq2);
   denominator = E^(\[Lambda] U2Cris) + E^(\[Lambda] U2Acq2);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]4a]);
   amplitude];

(*Player 2's amplitude for demanding after Player 1 doesn't demand*)
aD2afterND1[U2Acq1_, U2SQ_, \[Lambda]_, \[Theta]4b_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U2Acq1);
   denominator = E^(\[Lambda] U2Acq1) + E^(\[Lambda] U2SQ);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]4b]);
   amplitude];

(*Player 2's amplitude for not demanding after Player 1 doesn't \
demand*)
aND2afterND1[U2Acq1_, U2SQ_, \[Lambda]_, \[Theta]4b_] := 
  Module[{numerator, denominator, amplitude}, 
   numerator = E^(\[Lambda] U2SQ);
   denominator = E^(\[Lambda] U2Acq1) + E^(\[Lambda] U2SQ);
   amplitude = Sqrt[numerator/denominator]*E^(I Re[\[Theta]4b]);
   amplitude];
   
(* =====DEMANDS GAME PROBABILITY FUNCTIONS=====*)

(*Combined amplitude for Player 1 demanding*)
aD1[pD2norm_, aD1afterD2_, 
   aD1afterND2_] := (Sqrt[pD2norm]*aD1afterD2) + (Sqrt[1 - pD2norm]*
     aD1afterND2);

(*Probability of Player 1 demanding*)
pD1[aD1_] := aD1*Conjugate[aD1];

(*Combined amplitude for Player 1 not demanding*)
aND1[pD2norm_, aND1afterD2_, 
   aND1afterND2_] := (Sqrt[pD2norm]*aND1afterD2) + (Sqrt[1 - pD2norm]*
     aND1afterND2);

(*Probability of Player 1 not demanding*)
pND1[aND1_] := aND1*Conjugate[aND1];

(*Normalized probability of Player 1 demanding*)
pD1norm[pD1_, pND1_] := 
  Module[{denominator = pD1 + pND1}, 
   If[denominator == 0., 0.5, 
    pD1/denominator] (*Avoid division by zero*)];

(*Normalized probability of Player 1 not demanding*)
pND1norm[pD1_, pND1_] := 
  Module[{denominator = pD1 + pND1}, 
   If[denominator == 0., 0.5, 
    pND1/denominator] (*Avoid division by zero*)];

(*Combined amplitude for Player 2 demanding*)
aD2[pD1norm_, aD2afterD1_, 
   aD2afterND1_] := (Sqrt[pD1norm]*aD2afterD1) + (Sqrt[1 - pD1norm]*
     aD2afterND1);

(*Probability of Player 2 demanding*)
pD2[aD2_] := aD2*Conjugate[aD2];

(*Combined amplitude for Player 2 not demanding*)
aND2[pD1norm_, aND2afterD1_, 
   aND2afterND1_] := (Sqrt[pD1norm]*aND2afterD1) + (Sqrt[1 - pD1norm]*
     aND2afterND1);

(*Probability of Player 2 not demanding*)
pND2[aND2_] := aND2*Conjugate[aND2];

(*Normalized probability of Player 2 demanding*)
pD2norm[pD2_, pND2_] := 
  Module[{denominator = pD2 + pND2}, 
   If[denominator == 0., 0.5, 
    pD2/denominator] (*Avoid division by zero*)];

(*Normalized probability of Player 2 not demanding*)
pND2norm[pD2_, pND2_] := 
  Module[{denominator = pD2 + pND2}, 
   If[denominator == 0., 0.5, 
    pND2/denominator] (*Avoid division by zero*)];
    

now there are 4 phases which we can to solve for to find the best fit with the empirical observations
\[CapitalDelta]\[Theta]1 ... phase connected to the decision to "fight/not fight" of the 1st player
\[CapitalDelta]\[Theta]2 ... phase connected to the decision to "fight/not fight" of the 2nd player
\[CapitalDelta]\[Theta]3 ... phase connected to the decision to "demand/not demand" of the 1st player
\[CapitalDelta]\[Theta]4 ... phase connected to the decision to "demand/not demand" of the 2nd player

we can set \[CapitalDelta]\[Theta]1=\[CapitalDelta]\[Theta]3 and \[CapitalDelta]\[Theta]2=\[CapitalDelta]\[Theta]4 if necessary

Functions

(*
FindCrisisEquilibrium:
  Finds the equilibrium probabilities for the crisis game given \
utility values and phase differences between quantum states.
Parameters:
	 -u1War,u1Cap1,u1Cap2,u1Nego:Player 1's utilities for different \
outcomes
	-u2War,u2Cap2,u2Cap1,u2Nego:Player 2's utilities for different \
outcomes
	-lambda:Decision parameter controlling rationality (higher=more \
rational)
	-deltaTheta1,deltaTheta2:Phase differences for Players 1 and 2
	-maxIter:Maximum number of iterations for convergence
	-tolerance:Convergence threshold
  -verbose:Whether to record detailed iteration history
*)

FindCrisisEquilibrium[u1War_, u1Cap1_, u1Cap2_, u1Nego_, u2War_,
   				      u2Cap2_, u2Cap1_, u2Nego_,
   				      lambda_, deltaTheta1_, deltaTheta2_,
   				      maxIter_ : 100, tolerance_ : 10^-6,
   				      verbose_ : False] :=
  Module[{
    pF1normCurrent = 0.5,
    pF2normCurrent = 0.5,
    pF1normNext,
    pF2normNext,
    a1F2, a1NF2, a1F1, a1NF1, p1F1, p1NF1,
    a2F1, a2NF1, a2F2, a2NF2, p2F2, p2NF2,
    theta1a, theta1b, theta2a, theta2b,
    pWar, pCap2, pCap1, pNego,
    iter = 0, converged = False,
    iterationHistory = {},
    totalProb},
   
   (*Set phase differences*)
   theta1a = deltaTheta1/2;
   theta1b = -deltaTheta1/2;
   theta2a = deltaTheta2/2;
   theta2b = -deltaTheta2/2;
   
   (*Initial probabilities*)
   pWar = Re[pF1normCurrent*pF2normCurrent];
   pCap2 = Re[pF1normCurrent*(1 - pF2normCurrent)];
   pCap1 = Re[(1 - pF1normCurrent)*pF2normCurrent];
   pNego = Re[(1 - pF1normCurrent)*(1 - pF2normCurrent)];
   
   (*Keep a log for debugging purposes*)
   If[verbose,
    AppendTo[iterationHistory,
     <|
      "Iteration" -> 0,
      "pF1" -> Re[pF1normCurrent],
      "pF2" -> Re[pF2normCurrent],
      "theta1a" -> theta1a,
      "theta1b" -> theta1b,
      "deltaTheta1" -> deltaTheta1,
      "theta2a" -> theta2a,
      "theta2b" -> theta2b,
      "deltaTheta2" -> deltaTheta2,
      "pWar" -> Re[pWar],
      "pCap2" -> Re[pCap2],
      "pCap1" -> Re[pCap1],
      "pNego" -> Re[pNego],
      "TotalProb" -> pWar + pCap2 + pCap1 + pNego
      |>
     ]
    ];
   
   While[iter < maxIter && ! converged,
    
    (*Calculate amplitudes for player 1*)
    a1F2 = aF1afterF2[u1War, u1Cap1, lambda, theta1a];
    a1NF2 = aNF1afterF2[u1War, u1Cap1, lambda, theta1a];
    a1F1 = 
     aF1[pF2normCurrent, a1F2, 
      aF1afterNF2[u1Cap2, u1Nego, lambda, theta1b]];
    a1NF1 = 
     aNF1[pF2normCurrent, a1NF2, 
      aNF1afterNF2[u1Cap2, u1Nego, lambda, theta1b]];
    p1F1 = pF1[a1F1];
    p1NF1 = pNF1[a1NF1];
    pF1normNext = pF1norm[p1F1, p1NF1];
    
    (*Calculate amplitudes for player 2*)
    a2F1 = aF2afterF1[u2War, u2Cap2, lambda, theta2a];
    a2NF1 = aNF2afterF1[u2War, u2Cap2, lambda, theta2a];
    a2F2 = 
     aF2[pF1normCurrent, a2F1, 
      aF2afterNF1[u2Cap1, u2Nego, lambda, theta2b]];
    a2NF2 = 
     aNF2[pF1normCurrent, a2NF1, 
      aNF2afterNF1[u2Cap1, u2Nego, lambda, theta2b]];
    p2F2 = pF2[a2F2];
    p2NF2 = pNF2[a2NF2];
    pF2normNext = pF2norm[p2F2, p2NF2];
    
    (*Calculate outcome probabilities*)
    pWar = Re[pF1normCurrent*pF2normCurrent];
    pCap2 = Re[pF1normCurrent*(1 - pF2normCurrent)];
    pCap1 = Re[(1 - pF1normCurrent)*pF2normCurrent];
    pNego = Re[(1 - pF1normCurrent)*(1 - pF2normCurrent)];
    
    (*Check total probability for consistency*)
    totalProb = pWar + pCap2 + pCap1 + pNego;
    If[Abs[totalProb - 1.0] > 10^-10,
     Print["Warning: Total probability differs from 1.0: ", totalProb];
     ];
    
    (*Calculate convergence before updating values*)
    converged =
     Abs[pF1normNext - pF1normCurrent] < tolerance &&
      Abs[pF2normNext - pF2normCurrent] < tolerance;
    
    (*Track iteration if verbose*)
    If[verbose,
     AppendTo[iterationHistory,
      <|
       "Iteration" -> iter,
       "pF1" -> Re[pF1normCurrent],
       "pF2" -> Re[pF2normCurrent],
       "theta1a" -> theta1a,
       "theta1b" -> theta1b,
       "deltaTheta1" -> deltaTheta1,
       "theta2a" -> theta2a,
       "theta2b" -> theta2b,
       "deltaTheta2" -> deltaTheta2,
       "DeltapF1" -> Abs[pF1normNext - pF1normCurrent],
       "DeltapF2" -> Abs[pF2normNext - pF2normCurrent],
       "pWar" -> pWar,
       "pCap2" -> pCap2,
       "pCap1" -> pCap1,
       "pNego" -> pNego,
       "TotalProb" -> totalProb|>
      ]
     ];
    
    (*Update for next iteration*)
    pF1normCurrent = Re[pF1normNext];
    pF2normCurrent = Re[pF2normNext];
    iter++;];
   
   (*Return equilibrium probabilities and outcomes*)
   <|
    "Converged" -> converged,
    "Iterations" -> iter,
    "pF1" -> Re[pF1normCurrent],
    	"pF2" -> Re[pF2normCurrent],
    	"pWar" -> pWar,
    	"pCap2" -> pCap2,
    	"pCap1" -> pCap1,
    	"pNego" -> pNego,
    	"TotalProb" -> totalProb,
    	"IterationHistory" -> If[verbose, iterationHistory, Null]
    |>
   ];
   
FindFullEquilibrium


(*
   FindFullEquilibrium:Finds the equilibrium for the full game \
(demands+crisis) given utility values and phase differences.
Parameters:
	 -u1War,u1Cap1,u1Cap2,u1Nego,u1SQ,u1Acq1,u1Acq2:Player 1's utilities
	-u2War,u2Cap2,u2Cap1,u2Nego,u2SQ,u2Acq1,u2Acq2:Player 2's utilities
	-lambda:Decision parameter
	-deltaTheta1,deltaTheta2:Phase differences for crisis game
	-deltaTheta3,deltaTheta4:Phase differences for demands game
	-maxIter:Maximum iterations
	-tolerance:Convergence threshold
  -verbose:Detailed logging
*)
FindFullEquilibrium[
   u1War_, u1Cap1_, u1Cap2_, u1Nego_, u1SQ_,
   u1Acq1_, u1Acq2_, u2War_, u2Cap2_, u2Cap1_,
   u2Nego_, u2SQ_, u2Acq1_, u2Acq2_,
   lambda_,
   deltaTheta1_, deltaTheta2_, deltaTheta3_, deltaTheta4_,
   maxIter_ : 100,
   tolerance_ : 10^-6,
   verbose_ : False
   ] :=
  	Module[{
    	crisisEq, u1Cris, u2Cris,
    	pD1normCurrent = 0.5, pD2normCurrent = 0.5,
    	pD1normNext, pD2normNext,
    	a1D2, a1ND2, a1D1, a1ND1, p1D1, p1ND1,
    	a2D1, a2ND1, a2D2, a2ND2, p2D2, p2ND2,
    	crisisPWar, crisisPCap1, crisisPCap2, crisisPNego,
    	theta3a, theta3b, theta4a, theta4b,
    	pCrisis, pAcq2, pAcq1, pSQ, totalProb,
    	iter = 0, converged = False, iterationHistory = {}
    },
   
   (*Set phase differences for demands game*)
   theta3a = deltaTheta3/2;
   theta3b = -deltaTheta3/2;
   theta4a = deltaTheta4/2;
   theta4b = -deltaTheta4/2;
   
   (*First find crisis game equilibrium*)
   crisisEq = FindCrisisEquilibrium[
     			u1War, u1Cap1, u1Cap2, u1Nego, u2War, u2Cap2, u2Cap1, u2Nego,
     			lambda, deltaTheta1, deltaTheta2, maxIter, tolerance, verbose
     ];
   
   (*Check if crisis game converged*)
   If[! crisisEq["Converged"],
    If[verbose, Print["Warning: Crisis game did not converge."]];
    ];
   
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
   
   (*Handle extreme utility values with numerical safeguards*)
   If[Abs[u1Cris] > 100 || Abs[u2Cris] > 100,
    If[verbose, 
      Print["Warning: Very high crisis utilities may cause numerical \
instability"]];
    ];
   
   (*Initialize demand game probabilities*)
   pCrisis = Re[pD1normCurrent*pD2normCurrent];
   pAcq2 = Re[pD1normCurrent*(1 - pD2normCurrent)];
   pAcq1 = Re[(1 - pD1normCurrent)*pD2normCurrent];
   pSQ = Re[(1 - pD1normCurrent)*(1 - pD2normCurrent)];
   totalProb = pCrisis + pAcq2 + pAcq1 + pSQ;
   
   (*Record initial state if verbose*)
   If[verbose,
    AppendTo[iterationHistory,
     <|
      "Iteration" -> 0,
      "pD1" -> Re[pD1normCurrent],
      "pD2" -> Re[pD2normCurrent],
      "theta3a" -> theta3a,
      "theta3b" -> theta3b,
      "deltaTheta3" -> deltaTheta3,
      "theta4a" -> theta4a,
      "theta4b" -> theta4b,
      "deltaTheta4" -> deltaTheta4,
      "pCrisis" -> pCrisis,
      "pAcq2" -> pAcq2,
      "pAcq1" -> pAcq1,
      "pSQ" -> pSQ,
      "TotalProb" -> totalProb
      |>
     ]
    ];
   
   (*Now find demands game equilibrium*)
   While[iter < maxIter && ! converged,
    
    (*Calculate amplitudes for player 1*)
    a1D2 = aD1afterD2[u1Cris, u1Acq1, lambda, theta3a];
    a1ND2 = aND1afterD2[u1Cris, u1Acq1, lambda, theta3a];
    a1D1 = 
     aD1[pD2normCurrent, a1D2, 
      aD1afterND2[u1Acq2, u1SQ, lambda, theta3b]];
    a1ND1 = 
     aND1[pD2normCurrent, a1ND2, 
      aND1afterND2[u1Acq2, u1SQ, lambda, theta3b]];
    p1D1 = pD1[a1D1];
    p1ND1 = pND1[a1ND1];
    pD1normNext = pD1norm[p1D1, p1ND1];
    
    (*Calculate amplitudes for player 2*)
    a2D1 = aD2afterD1[u2Cris, u2Acq2, lambda, theta4a];
    a2ND1 = aND2afterD1[u2Cris, u2Acq2, lambda, theta4a];
    a2D2 = 
     aD2[pD1normCurrent, a2D1, 
      aD2afterND1[u2Acq1, u2SQ, lambda, theta4b]];
    a2ND2 = 
     aND2[pD1normCurrent, a2ND1, 
      aND2afterND1[u2Acq1, u2SQ, lambda, theta4b]];
    p2D2 = pD2[a2D2];
    p2ND2 = pND2[a2ND2];
    pD2normNext = pD2norm[p2D2, p2ND2];
    
    (*Update outcome probabilities*)
    pCrisis = Re[pD1normCurrent*pD2normCurrent];
    pAcq2 = Re[pD1normCurrent*(1 - pD2normCurrent)];
    pAcq1 = Re[(1 - pD1normCurrent)*pD2normCurrent];
    pSQ = Re[(1 - pD1normCurrent)*(1 - pD2normCurrent)];
    totalProb = pCrisis + pAcq2 + pAcq1 + pSQ;
    
    (*Check for numerical issues*)
    If[Abs[totalProb - 1.0] > 10^-10,
     If[verbose, 
       Print["Warning: Total probability in demands game differs from \
1.0: ", totalProb]];
     ];
    
    (*Check convergence*)
    converged =
     	Abs[pD1normNext - pD1normCurrent] < tolerance &&
             Abs[pD2normNext - pD2normCurrent] < tolerance;
    
    (*Track iteration if verbose*)
    If[verbose,
     AppendTo[
      iterationHistory,
      <|"Iteration" -> iter,
       "pD1" -> Re[pD1normCurrent],
       "pD2" -> Re[pD2normCurrent],
       "theta3a" -> theta3a,
       "theta3b" -> theta3b,
       "deltaTheta3" -> deltaTheta3,
       "theta4a" -> theta4a,
       "theta4b" -> theta4b,
       "deltaTheta4" -> deltaTheta4,
       "DeltapD1" -> Abs[pD1normNext - pD1normCurrent],
       "DeltapD2" -> Abs[pD2normNext - pD2normCurrent],
       "pCrisis" -> pCrisis,
       "pAcq2" -> pAcq2,
       "pAcq1" -> pAcq1,
       "pSQ" -> pSQ,
       "TotalProb" -> totalProb
       |>
      ]
     ];
    
    (*Update for next iteration*)
    pD1normCurrent = Re[pD1normNext];
    pD2normCurrent = Re[pD2normNext];
    iter++;
    ];
   
   (*Return full equilibrium probabilities and outcomes*)
   <|
    "CrisisEquilibrium" ->
     <|
      "pWar" -> crisisPWar,
      "pCap1" -> crisisPCap1,
      "pCap2" -> crisisPCap2,
      "pNego" -> crisisPNego,
      "pF1" -> crisisEq["pF1"],
      "pF2" -> crisisEq["pF2"],
      "Converged" -> crisisEq["Converged"],
      "Iterations" -> crisisEq["Iterations"],
      "TotalProb" -> crisisEq["TotalProb"]
      |>,
    "DemandsConverged" -> converged,
    "DemandsIterations" -> iter,
    "pD1" -> Re[pD1normCurrent],
    "pD2" -> Re[pD2normCurrent],
    "pCrisis" -> pCrisis,
    "pAcq2" -> pAcq2,
    "pAcq1" -> pAcq1,
    "pSQ" -> pSQ,
    "TotalProb" -> totalProb,
    "ExpectedUtility1" -> 
     pCrisis*u1Cris + pAcq2*u1Acq2 + pAcq1*u1Acq1 + pSQ*u1SQ,
    "ExpectedUtility2" -> 
     pCrisis*u2Cris + pAcq2*u2Acq2 + pAcq1*u2Acq1 + pSQ*u2SQ,
    "DemandsIterationHistory" -> If[verbose, iterationHistory, Null]
    |>
   ];
   
PrepareDataset

(*PlotCrisisConvergence:Visualizes the convergence of crisis game \
probabilities*)

PlotCrisisConvergence[history_] := 
  Module[{playerPlot, outcomePlot, convergencePlot, 
    historyTable},(*Player fight probabilities*)
   playerPlot = 
    ListPlot[{Table[{i, history[[i + 1, "pF1"]]}, {i, 0, 
        Length[history] - 1}], 
      Table[{i, history[[i + 1, "pF2"]]}, {i, 0, 
        Length[history] - 1}]}, 
     PlotLegends -> {"Player 1", "Player 2"}, 
     AxesLabel -> {"Iteration", "Probability"}, 
     PlotLabel -> "Convergence of Fight Probabilities", 
     Joined -> True, Mesh -> All, PlotMarkers -> Automatic, 
     GridLines -> Automatic, PlotRange -> {0, 1}];
   (*Outcome probabilities*)
   outcomePlot = 
    ListPlot[{Table[{i, history[[i + 1, "pWar"]]}, {i, 0, 
        Length[history] - 1}], 
      Table[{i, history[[i + 1, "pCap2"]]}, {i, 0, 
        Length[history] - 1}], 
      Table[{i, history[[i + 1, "pCap1"]]}, {i, 0, 
        Length[history] - 1}], 
      Table[{i, history[[i + 1, "pNego"]]}, {i, 0, 
        Length[history] - 1}]}, 
     PlotLegends -> {"pWar", "pCap2", "pCap1", "pNego"}, 
     AxesLabel -> {"Iteration", "Probability"}, 
     PlotLabel -> "Convergence of Probabilities in Crisis Game", 
     Joined -> True, Mesh -> All, PlotMarkers -> Automatic, 
     GridLines -> Automatic, PlotRange -> {0, 1}];
   (*Convergence rate*)
   If[Length[history] > 1, 
    convergencePlot = 
     ListLogPlot[{Table[{i, history[[i + 1, "DeltapF1"]]}, {i, 1, 
         Length[history] - 1}], 
       Table[{i, history[[i + 1, "DeltapF2"]]}, {i, 1, 
         Length[history] - 1}]}, 
      PlotLegends -> {"Player 1", "Player 2"}, 
      AxesLabel -> {"Iteration", "Log(\[CapitalDelta]p)"}, 
      PlotLabel -> "Convergence Rate", Joined -> True, 
      GridLines -> Automatic], 
    convergencePlot = 
     Graphics[{}, 
      PlotLabel -> "Insufficient data for convergence plot"]];
   (*Iteration table*)
   If[Length[history] > 1, 
    historyTable = 
     Grid[{{"Iteration", "pF1", "pF2", "DeltapF1", "DeltapF2", 
         "theta1a", "theta1b", "theta2a", "theta2b", "deltaTheta1", 
         "deltaTheta2"}}~Join~
       Table[{history[[i, "Iteration"]], 
         NumberForm[history[[i, "pF1"]], {4, 4}], 
         NumberForm[history[[i, "pF2"]], {4, 4}], 
         If[i > 1, NumberForm[history[[i, "DeltapF1"]], {4, 4}], "-"],
          If[i > 1, NumberForm[history[[i, "DeltapF2"]], {4, 4}], 
          "-"], NumberForm[history[[i, "theta1a"]], {4, 4}], 
         NumberForm[history[[i, "theta1b"]], {4, 4}], 
         NumberForm[history[[i, "theta2a"]], {4, 4}], 
         NumberForm[history[[i, "theta2b"]], {4, 4}], 
         NumberForm[history[[i, "deltaTheta1"]], {4, 4}], 
         NumberForm[history[[i, "deltaTheta2"]], {4, 4}]}, {i, 1, 
         Min[Length[history], 10]}], Frame -> All, 
      Alignment -> Center, Background -> {None, {LightGray, None}}], 
    historyTable = Grid[{{"No iteration data available"}}]];
   (*Return all visualizations*){playerPlot, outcomePlot, 
    convergencePlot, historyTable}];

PlotDemandsConvergence

(*PlotDemandsConvergence:Visualizes the convergence of demands game \
probabilities*)
PlotDemandsConvergence[history_] := 
  Module[{playerPlot, outcomePlot, convergencePlot, 
    historyTable},(*Player demand probabilities*)
   playerPlot = 
    ListPlot[{Table[{i, history[[i + 1, "pD1"]]}, {i, 0, 
        Length[history] - 1}], 
      Table[{i, history[[i + 1, "pD2"]]}, {i, 0, 
        Length[history] - 1}]}, 
     PlotLegends -> {"Player 1", "Player 2"}, 
     AxesLabel -> {"Iteration", "Probability"}, 
     PlotLabel -> "Convergence of Demand Probabilities", 
     Joined -> True, Mesh -> All, PlotMarkers -> Automatic, 
     GridLines -> Automatic, PlotRange -> {0, 1}];
   (*Outcome probabilities*)
   outcomePlot = 
    ListPlot[{Table[{i, history[[i + 1, "pCrisis"]]}, {i, 0, 
        Length[history] - 1}], 
      Table[{i, history[[i + 1, "pAcq2"]]}, {i, 0, 
        Length[history] - 1}], 
      Table[{i, history[[i + 1, "pAcq1"]]}, {i, 0, 
        Length[history] - 1}], 
      Table[{i, history[[i + 1, "pSQ"]]}, {i, 0, 
        Length[history] - 1}]}, 
     PlotLegends -> {"pCrisis", "pAcq2", "pAcq1", "pSQ"}, 
     AxesLabel -> {"Iteration", "Probability"}, 
     PlotLabel -> "Convergence of Probabilities in Demands Game", 
     Joined -> True, Mesh -> All, PlotMarkers -> Automatic, 
     GridLines -> Automatic, PlotRange -> {0, 1}];
   (*Convergence rate*)
   If[Length[history] > 1, 
    convergencePlot = 
     ListLogPlot[{Table[{i, history[[i + 1, "DeltapD1"]]}, {i, 1, 
         Length[history] - 1}], 
       Table[{i, history[[i + 1, "DeltapD2"]]}, {i, 1, 
         Length[history] - 1}]}, 
      PlotLegends -> {"Player 1", "Player 2"}, 
      AxesLabel -> {"Iteration", "Log(\[CapitalDelta]p)"}, 
      PlotLabel -> "Demand Game Convergence Rate", Joined -> True, 
      GridLines -> Automatic], 
    convergencePlot = 
     Graphics[{}, 
      PlotLabel -> "Insufficient data for convergence plot"]];
   (*Iteration table*)
   If[Length[history] > 1, 
    historyTable = 
     Grid[{{"Iteration", "pD1", "pD2", "DeltapD1", "DeltapD2", 
         "theta3a", "theta3b", "theta4a", "theta4b", "deltaTheta3", 
         "deltaTheta4"}}~Join~
       Table[{history[[i, "Iteration"]], 
         NumberForm[history[[i, "pD1"]], {4, 4}], 
         NumberForm[history[[i, "pD2"]], {4, 4}], 
         If[i > 1, NumberForm[history[[i, "DeltapD1"]], {4, 4}], "-"],
          If[i > 1, NumberForm[history[[i, "DeltapD2"]], {4, 4}], 
          "-"], NumberForm[history[[i, "theta3a"]], {4, 4}], 
         NumberForm[history[[i, "theta3b"]], {4, 4}], 
         NumberForm[history[[i, "theta4a"]], {4, 4}], 
         NumberForm[history[[i, "theta4b"]], {4, 4}], 
         NumberForm[history[[i, "deltaTheta3"]], {4, 4}], 
         NumberForm[history[[i, "deltaTheta4"]], {4, 4}]}, {i, 1, 
         Min[Length[history], 10]}], Frame -> All, 
      Alignment -> Center, Background -> {None, {LightGray, None}}], 
    historyTable = Grid[{{"No iteration data available"}}]];
   (*Return all visualizations*){playerPlot, outcomePlot, 
    convergencePlot, historyTable}];


CreateSummaryTable

(*Corrected CreateSummaryTable function*)(*CreateSummaryTable:Creates \
a summary table of outcome probabilities from the full game.This \
corrected version properly calculates the total probabilities by \
multiplying crisis game outcomes by the probability of the crisis \
occurring.*)
CreateSummaryTable[fullResult_] := 
  Module[{crisisEq = fullResult["CrisisEquilibrium"], 
    crisisProb = 
     fullResult["pCrisis"],(*Calculate total probabilities*)
    totalWarProb, totalCap1Prob, totalCap2Prob, 
    totalNegoProb,(*Calculate marginal probabilities*)marginalWarProb,
     marginalCap1Prob, marginalCap2Prob, 
    marginalNegoProb},(*Get marginal probabilities (within crisis \
game)*)marginalWarProb = crisisEq["pWar"];
   marginalCap1Prob = crisisEq["pCap1"];
   marginalCap2Prob = crisisEq["pCap2"];
   marginalNegoProb = crisisEq["pNego"];
   (*Calculate total probabilities (accounting for crisis \
probability)*)totalWarProb = crisisProb*marginalWarProb;
   totalCap1Prob = crisisProb*marginalCap1Prob;
   totalCap2Prob = crisisProb*marginalCap2Prob;
   totalNegoProb = crisisProb*marginalNegoProb;
   (*Create the summary table with both marginal and total \
probabilities*)
   Grid[{{"Game Stage", "Outcome", "Marginal Probability", 
      "Total Probability"}, {"Crisis Game", "War", marginalWarProb, 
      totalWarProb}, {"Crisis Game", "Capitulation 1", 
      marginalCap1Prob, totalCap1Prob}, {"Crisis Game", 
      "Capitulation 2", marginalCap2Prob, 
      totalCap2Prob}, {"Crisis Game", "Negotiation", marginalNegoProb,
       totalNegoProb}, {"Demands Game", "Crisis", 
      fullResult["pCrisis"], fullResult["pCrisis"]}, {"Demands Game", 
      "Acquiescence 1", fullResult["pAcq1"], 
      fullResult["pAcq1"]}, {"Demands Game", "Acquiescence 2", 
      fullResult["pAcq2"], fullResult["pAcq2"]}, {"Demands Game", 
      "Status Quo", fullResult["pSQ"], fullResult["pSQ"]}}, 
    Frame -> All, Alignment -> {Left, Left, Right, Right}, 
    Background -> {{LightGray, None}, {LightGray, None, LightGray, 
       None, LightGray, None, LightGray, None, LightGray}}, 
    Dividers -> {{None, None, {True}, None}, None}]];

VerifyFullGameProbabilities

(*Function to verify that probabilities sum to 1*)
VerifyFullGameProbabilities[fullResult_] := 
  Module[{crisisEq = fullResult["CrisisEquilibrium"], 
    crisisProb = fullResult["pCrisis"], totalWarProb, totalCap1Prob, 
    totalCap2Prob, totalNegoProb, totalSum, crisisSumCheck, 
    demandsSumCheck},(*Calculate total probabilities*)
   totalWarProb = crisisProb*crisisEq["pWar"];
   totalCap1Prob = crisisProb*crisisEq["pCap1"];
   totalCap2Prob = crisisProb*crisisEq["pCap2"];
   totalNegoProb = crisisProb*crisisEq["pNego"];
   (*Verify that probabilities sum to 1*)
   totalSum = 
    totalWarProb + totalCap1Prob + totalCap2Prob + totalNegoProb + 
     fullResult["pAcq1"] + fullResult["pAcq2"] + fullResult["pSQ"];
   crisisSumCheck = 
    crisisEq["pWar"] + crisisEq["pCap1"] + crisisEq["pCap2"] + 
     crisisEq["pNego"];
   demandsSumCheck = 
    fullResult["pCrisis"] + fullResult["pAcq1"] + 
     fullResult["pAcq2"] + fullResult["pSQ"];
   <|"TotalProbabilitySum" -> totalSum, 
    "SumsToOne" -> Abs[totalSum - 1.0] < 10^-10, 
    "CrisisGameSum" -> crisisSumCheck, 
    "DemandsGameSum" -> demandsSumCheck, "Crisis-War" -> totalWarProb,
     "Crisis-Cap1" -> totalCap1Prob, "Crisis-Cap2" -> totalCap2Prob, 
    "Crisis-Nego" -> totalNegoProb, 
    "Acquiescence 1" -> fullResult["pAcq1"], 
    "Acquiescence 2" -> fullResult["pAcq2"], 
    "Status Quo" -> fullResult["pSQ"]|>];


Testing

Load Data

data = Import[
   "/Users/162191/Documents/GitHub/quantum_international_interaction_\
game/Norm_Form/first_dataset_with_SQ.csv", "CSV", 
   "HeaderLines" -> 1];
dataset = 
  Dataset[Association @@@ (Rule @@@ 
        Transpose[{{"Agent1", "Agent2", "wrTu1sq", "wrTu1ac1", 
           "wrTu1ac2", "wrTu1neg", "wrTu1cp1", "wrTu1cp2", "wrTu1wr1",
            "wrTu1wr2", "wrTu2sq", "wrTu2ac2", "wrTu2ac1", "wrTu2neg",
            "wrTu2cp2", "wrTu2cp1", "wrTu2wr2", "wrTu2wr1", 
           "groundtruth"}, #}] & /@ data)];
           
           
Prepare Data

preparedData = PrepareDataset[dataset];

Params

NumSamples = 1;
lambda = 1.0;
dt1 = Pi;
dt2 = 3 Pi /2;
dt3 = Pi / 4;
dt4 = Pi / 6;
maxIter = 100;
threshold = 0.000001;
verboseMode = True;


Testing


sampleCase = preparedData[[1]];

Crisis Game

crisisResult = FindCrisisEquilibrium[sampleCase["U1War"],
   								 sampleCase["U1Cap1"],
   								 sampleCase["U1Cap2"],
   								 sampleCase["U1Nego"],
   								 sampleCase["U2War"],
   								 sampleCase["U2Cap2"],
   							          sampleCase["U2Cap1"],
   								 sampleCase["U2Nego"],
   								 lambda,
   								 dt1,
   								 dt2,
   								 maxIter,
   								 threshold,
   								 verboseMode];


crisisConvergenceViz = 
  PlotCrisisConvergence[crisisResult["IterationHistory"]];
  

crisisConvergenceViz[[1]]

crisisConvergenceViz[[2]]


fullResult = FindFullEquilibrium[sampleCase["U1War"],
   							sampleCase["U1Cap1"],
   							sampleCase["U1Cap2"],
   							sampleCase["U1Nego"],
   							sampleCase["U1SQ"],
   							sampleCase["U1Acq1"],
   							sampleCase["U1Acq2"],
   							sampleCase["U2War"],
   							sampleCase["U2Cap2"],
   							sampleCase["U2Cap1"],
   							sampleCase["U2Nego"],
   							sampleCase["U2SQ"],
   							sampleCase["U2Acq1"],
   							sampleCase["U2Acq2"],
   							lambda,
   							dt1,
   							dt2,
   							dt3,
   							dt4,
   							maxIter,
   							threshold,
   							verboseMode];
   							
fullGameConvergenceViz[[1]]

VerifyFullGameProbabilities[fullResult]








