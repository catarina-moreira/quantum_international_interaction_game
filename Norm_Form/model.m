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

