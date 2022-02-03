(* ::Package:: *)

BeginPackage["KnotTheory`PDCode`", {
	"KnotTheory`Common`",
	"KnotTheory`MinimumBraids`"
}];

PD::usage = "
  PD[v1, v2, ...] represents a planar diagram whose vertices are v1, v2,
  .... PD also acts as a \"type caster\", so for example, PD[K] where K is 
  is a named knot (or link) returns the PD presentation of that knot.
";

X::usage = "
  X[i,j,k,l] represents a crossing between the edges labeled i, j, k
  and l starting from the incoming lower strand i and going
  counterclockwise through j, k and l.  The (sometimes ambiguous)
  orientation of the upper strand is determined by the ordering of
  {j,l}.
";

AlternatingQ::usage = "
	AlternatingQ[D] returns True iff the knot/link diagram D is alternating.
";

Mirror;

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
  
  Join[
    pd /. MapThread[Rule, {indexes, Range[len]}] /. Xp | Xm -> X,
    Loop /@ PD @@ Range[len + 1, len + loops]
  ]
]

Mirror[PD[Xs___X]] := PD[Xs] /. {
  X[i_,j_,k_,l_] /; j-l==1 || l-j>1 :> X[l,i,j,k],
  X[i_,j_,k_,l_] /; l-j==1 || j-l>1 :> X[j,k,l,i]
};

PD[TorusKnot[m_, n_]] := PD[BR[TorusKnot[m, n]]];

PD[Knot[n_, k_]] := (
	Import[FileNameJoin[{
		ExpandFileName[KnotTheory`KnotTheoryDirectory[]],
		"PDCode",
		"PD4Knots.m"
	}]];
	Unset[PD[Knot[n1_, k1_]]];
	Return[PD[Knot[n, k]]]
)

AlternatingQ[diag_Knot|diag_Link] := Module[{h},
  0 === Plus @@ (PD[diag] /. {
    X[i_, j_, k_, l_] :> h[i] - h[j] + h[k] - h[l],
    _Loop -> 0
  })
];

End[];
EndPackage[];
