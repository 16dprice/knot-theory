(* ::Package:: *)

BeginPackage["KnotTheory`PDCode`", {
	"KnotTheory`Common`",
	"KnotTheory`MinimumBraids`"
}];

PD::usage = "
	PD[v1, v2, ...] represents a planar diagram whose vertices are v1, v2,
	.... PD also acts as a \"type caster\", so for example, PD[K] where K
	is a named knot (or link) returns the PD presentation of that knot.
";

PD::about = "
	
";

X::usage = "
	X[i,j,k,l] represents a crossing between the edges labeled i, j, k
	and l starting from the incoming lower strand i and going
	counterclockwise through j, k and l.  The (sometimes ambiguous)
	orientation of the upper strand is determined by the ordering of
	{j,l}.
";

PositiveQ::usage = "
	PositiveQ[X] returns True if X is a positive (right handed)
	crossing and False if it is negative (left handed).
";

NegativeQ::usage = "
	NegativeQ[X] returns True if X is a negative (left handed)
	crossing and False if it is positive (right handed).
";

Loop::usage = "
	Loop[i] represents a crossingsless loop labeled i.
";

AlternatingQ::usage = "
	AlternatingQ[D] returns True iff the knot/link diagram D is alternating.
";

Mirror::usage = "
	Mirror[PDCode] returns the mirror of PDCode.
";

PositiveCrossings::usage = "
	PositiveCrossings[L] returns the number of positive (right handed)
	crossings in a knot/link L (in its given presentation).
";

NegativeCrossings::usage = "
	NegativeCrossings[L] returns the number of negaitve (left handed)
	crossings in a knot/link L (in its given presentation).
";

ConnectedSum::usage = "
	ConnectedSum[K1, K2] represents the connected sum of the knots K1 and
	K2 (ConnectedSum may not work with links).
";

Begin["`Private`"];

PD[pd_PD] := pd;
PD[Mirror[L_Knot|L_Link]] := Mirror[PD[L]];

PD[BR[k_, {}]] := PD @@ (Loop /@ Range[k])
PD[BR[k_Integer, l_List]] := Module[
  {
    a, b, c, d, e = Range[k], m = k, j, j1, Xp, Xm, pd, ar, cycles = 1, 
    closurerule, indexes, len, loops
  },
  
  pd = PD @@ (l  /. j_Integer :> (
    j1 = Abs[j];
    a = e[[j1]]; b = e[[j1 + 1]]; c = e[[j1]] = ++m; d = e[[j1 + 1]] = ++m;
    cycles *= ar[a, d]*ar[b, c];
    If[j > 0, Xp[b, d, c, a], Xm[a, b, d, c]]
  ));
  
  closurerule = MapThread[Rule, {e, Range[k]}];
  cycles = cycles /. closurerule //.
    ar[a_, b___, c_]ar[c_, d___, e_] :> ar[a, b, c, d, e] /.
    a_ar :> Rest[a];
    
  pd = pd /. closurerule;
  len = Length[indexes = Flatten[List @@@ List @@ cycles]];
  loops = Length[Complement[Range[k], Abs[l], Abs[l] + 1]];
  
  Return[Join[
    pd /. MapThread[Rule, {indexes, Range[len]}] /. Xp | Xm -> X,
    Loop /@ PD @@ Range[len + 1, len + loops]
  ]];
];

PD[TorusKnot[m_, n_]] := PD[BR[TorusKnot[m, n]]];

PD[Knot[n_, k_]] := Module[{},
	Unset[PD[Knot[n1_, k1_]]];
	Import[FileNameJoin[{
		ExpandFileName[KnotTheory`KnotTheoryDirectory[]],
		"PDCode",
		"PD4Knots.m"
	}]];
	Return[PD[Knot[n, k]]];
];

PD[Link[n_, t_, k_]] := Module[{},	
	Unset[PD[Link[n1_, t1_, k1_]]];
	Import[FileNameJoin[{
		ExpandFileName[KnotTheory`KnotTheoryDirectory[]],
		"PDCode",
		"PD4Links.m"
	}]];
	Return[PD[Link[n, t, k]]];
];

Mirror[PD[Xs___X]] := PD[Xs] /. {
  X[i_,j_,k_,l_] /; j-l==1 || l-j>1 :> X[l,i,j,k],
  X[i_,j_,k_,l_] /; l-j==1 || j-l>1 :> X[j,k,l,i]
};

AlternatingQ[diag_Knot|diag_Link] := Module[{h},
  0 === Plus @@ (PD[diag] /. {
    X[i_, j_, k_, l_] :> h[i] - h[j] + h[k] - h[l],
    _Loop -> 0
  })
];

PositiveQ[X[i_,j_,k_,l_]] := Return[i == j || k == l || j-l == 1 || l-j > 1];
NegativeQ[X[i_,j_,k_,l_]] := Return[i == l || j == k || l-j == 1 || j-l > 1];

PositiveCrossings[pd_PD] := Count[pd, _?PositiveQ];
PositiveCrossings[L_Knot|L_Link] := PositiveCrossings[PD[L]];

NegativeCrossings[pd_PD] := Count[pd, _?NegativeQ];
NegativeCrossings[L_Knot|L_Link] := NegativeCrossings[PD[L]];

ConnectedSum[pd1_PD, pd2_PD] := Module[{c1, c2, l2, npd1, npd2},
  If[Head[First[pd1]] === Loop, Return[Join[Drop[pd1, 1], pd2]]];
  If[Head[First[pd2]] === Loop, Return[Join[pd1, Drop[pd2, 1]]]];
  
  c1 = pd1[[1, 1]];
  c2 = pd2[[1, 1]];
  l2 = Max @@ Max @@@ pd2;
  
  npd1 = Map[If[# > c1, # + l2, #] &, pd1, {2}];
  npd1[[1, 1]] += l2;
  npd2 = Map[If[# <= c2, # + c1 + l2 - c2, # + c1 - c2] &, pd2, {2}];
  npd2[[1, 1]] -= l2;
  
  Return[Join[npd1, npd2]];
];
PD[ConnectedSum[K1_Knot|K1_Link, K2_Knot|K2_Link]] := ConnectedSum[PD[K1], PD[K2]];

End[];
EndPackage[];
