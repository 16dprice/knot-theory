(* ::Package:: *)

BeginPackage["KnotTheory`Common`"];

Knot::usage = "
  Knot[n, k] denotes the kth knot with n crossings in the Rolfsen table.
  Knot[n, Alternating, k] (for n between 11 and 16) denotes the kth alternating n-crossing knot in
  the Hoste-Thistlethwaite table. Similary, Knot[n, NonAlternating, k] denotes the
  kth non alternating n-crossing knot in the Hoste-Thistlethwaite table.
";

Link::usage = "
  Link[n, Alternating, k] denotes the kth alternating n-crossing link in
  the Thistlethwaite table. Link[n, NonAlternating, k] denotes the kth
  non alternating n-crossing link in the Thistlethwaite table.
";

TorusKnot::usage = "
  TorusKnot[m, n] represents the (m,n) torus knot.
";

TorusKnot::invalidInput = "
	TorusKnot[`1`, `2`] invalid because either `1` or `2` is less than or equal to 0.
";

Loop::usage = "
  Loop[i] represents a crossingsless loop labeled i.
";

Crossings::usage = "
  Crossings[L] returns the number of crossings of a knot/link L (in its
  given presentation).
";

NumberOfKnots::usage = "
	NumberOfKnots[n] returns the number of knots with n crossings.
	NumberOfKnots[n, Alternating|NonAlternating] returns the number of knots
	of the specified type.
";

NumberOfLinks::usage = "
	NumberOfLinks[n] returns the number of links with n crossings.
	NumberOfLinks[n, Alternating|NonAlternating] returns the number of links
	of the specified type.
";

AllKnots::usage = "
  AllKnots[] return a list of all knots with up to 11 crossings. AllKnots[n_] returns
  a list of all knots with n crossings, up to 16. AllKnots[{n_,m_}] returns a list of
  all knots with between n and m crossings, and AllKnots[n_,Alternating|NonAlternating]
  returns all knots with n crossings of the specified type.
";

AllLinks::usage = "
  AllLinks[] return a list of all links with up to 11 crossings. AllLinks[n_] returns
  a list of all links with n! crossings, up to 12.
";

AllKnots::crossingNumberOutOfBounds = AllLinks::crossingNumberOutOfBounds = "
	There is currently no data for `1` crossings.
";

AllKnots::improperlyOrderedIndices = AllLinks::improperlyOrderedIndices = "
	The minimum crossing value `1` is greater than the maximum crossing value `2`.
";

Alternating;
NonAlternating;

Begin["`Private`"];

TorusKnot[m_, n_] /; m <= 0 || n <= 0 := (
	Message[TorusKnot::invalidInput, m, n];
	Return[TorusKnot[1, 1]];
);

Crossings[Knot[n_, __]] := n;
Crossings[Link[n_, __]] := n;

NumberOfKnots[0, Alternating] = 1;
NumberOfKnots[1, Alternating] = 0;
NumberOfKnots[2, Alternating] = 0;
NumberOfKnots[3, Alternating] = 1;
NumberOfKnots[4, Alternating] = 1;
NumberOfKnots[5, Alternating] = 2;
NumberOfKnots[6, Alternating] = 3;
NumberOfKnots[7, Alternating] = 7;
NumberOfKnots[8, Alternating] = 18;
NumberOfKnots[9, Alternating] = 41;
NumberOfKnots[10, Alternating] = 123;
NumberOfKnots[11, Alternating] = 367;
NumberOfKnots[12, Alternating] = 1288;
NumberOfKnots[13, Alternating] = 4878;
NumberOfKnots[14, Alternating] = 19536;
NumberOfKnots[15, Alternating] = 85263;
NumberOfKnots[16, Alternating] = 379799;

NumberOfKnots[0, NonAlternating] = 0;
NumberOfKnots[1, NonAlternating] = 0;
NumberOfKnots[2, NonAlternating] = 0;
NumberOfKnots[3, NonAlternating] = 0;
NumberOfKnots[4, NonAlternating] = 0;
NumberOfKnots[5, NonAlternating] = 0;
NumberOfKnots[6, NonAlternating] = 0;
NumberOfKnots[7, NonAlternating] = 0;
NumberOfKnots[8, NonAlternating] = 3;
NumberOfKnots[9, NonAlternating] = 8;
NumberOfKnots[10, NonAlternating] = 42;
NumberOfKnots[11, NonAlternating] = 185;
NumberOfKnots[12, NonAlternating] = 888;
NumberOfKnots[13, NonAlternating] = 5110;
NumberOfKnots[14, NonAlternating] = 27436;
NumberOfKnots[15, NonAlternating] = 168030;
NumberOfKnots[16, NonAlternating] = 1008906;

NumberOfLinks[2, Alternating] = 1;
NumberOfLinks[3, Alternating] = 0;
NumberOfLinks[4, Alternating] = 1;
NumberOfLinks[5, Alternating] = 1;
NumberOfLinks[6, Alternating] = 5;
NumberOfLinks[7, Alternating] = 7;
NumberOfLinks[8, Alternating] = 21;
NumberOfLinks[9, Alternating] = 55;
NumberOfLinks[10, Alternating] = 174;
NumberOfLinks[11, Alternating] = 548;
NumberOfLinks[12, Alternating] = 2020;
NumberOfLinks[2, NonAlternating] = 0;
NumberOfLinks[3, NonAlternating] = 0;
NumberOfLinks[4, NonAlternating] = 0;
NumberOfLinks[5, NonAlternating] = 0;
NumberOfLinks[6, NonAlternating] = 1;
NumberOfLinks[7, NonAlternating] = 2;
NumberOfLinks[8, NonAlternating] = 8;
NumberOfLinks[9, NonAlternating] = 28;
NumberOfLinks[10, NonAlternating] = 113;
NumberOfLinks[11, NonAlternating] = 459;
NumberOfLinks[12, NonAlternating] = 2256;

NumberOfKnots[n_] := NumberOfKnots[n, Alternating] + NumberOfKnots[n, NonAlternating];
NumberOfKnots[{n_, m_}] := Sum[NumberOfKnots[k], {k, n, m}];
NumberOfKnots[{n_, m_}, t_] := Sum[NumberOfKnots[k, t], {k, n, m}];

NumberOfLinks[n_] := NumberOfLinks[n, Alternating] + NumberOfLinks[n, NonAlternating];
NumberOfLinks[{n_, m_}]:= Sum[NumberOfLinks[k], {k,n,m}];
NumberOfLinks[{n_, m_}, t_]:= Sum[NumberOfLinks[k, t], {k,n,m}];

(* These are ordered lists for the purpose of data loading! Do not mess! *)
AllKnots[] := Flatten[{
  Table[Knot[n,k], {n,0,10}, {k,NumberOfKnots[n]}],
  Table[Knot[11, Alternating, k], {k, NumberOfKnots[11, Alternating]}],
  Table[Knot[11, NonAlternating, k], {k, NumberOfKnots[11, NonAlternating]}]
}];

AllLinks[] := Flatten[Table[{
  Table[Link[n, Alternating, k], {k,NumberOfLinks[n, Alternating]}],
  Table[Link[n, NonAlternating, k], {k,NumberOfLinks[n, NonAlternating]}]
}, {n,2,11}]];

AllKnots[crossingNumber_] := Module[{},
	If[0 <= crossingNumber <= 10, 
		Return[Table[Knot[crossingNumber, k], {k, 1, NumberOfKnots[crossingNumber]}]]
	];
	
	If[11 <= crossingNumber <= 16,
		Return[AllKnots[crossingNumber, Alternating]~Join~AllKnots[crossingNumber, NonAlternating]]
	];

	Message[AllKnots::crossingNumberOutOfBounds, crossingNumber];
	Return[{}];
];

AllKnots[crossingNumber_, t_] := Module[{},
	If[0 <= crossingNumber <= 10,
		If[t === Alternating,
			Return[Table[Knot[crossingNumber, k], {k, NumberOfKnots[crossingNumber, Alternating]}]],
			Return[Table[
				Knot[crossingNumber, NumberOfKnots[crossingNumber, Alternating] + k],
				{k, NumberOfKnots[crossingNumber, NonAlternating]}
			]]
		]
	];
	
	If[11 <= crossingNumber <= 16,
		Return[Table[Knot[crossingNumber, t, k], {k, NumberOfKnots[crossingNumber, t]}]]
	];
	
	Message[AllKnots::crossingNumberOutOfBounds, crossingNumber];
	Return[{}];
];

AllKnots[{minCrossings_, maxCrossings_}] := Module[{},
	If[minCrossings < 0 || minCrossings > 16,
		Message[AllKnots::crossingNumberOutOfBounds, minCrossings];
		Return[{}];
	];
	
	If[maxCrossings < 0 || maxCrossings > 16,
		Message[AllKnots::crossingNumberOutOfBounds, maxCrossings];
		Return[{}];
	];
	
	If[minCrossings > maxCrossings,
		Message[AllKnots::improperlyOrderedIndices, minCrossings, maxCrossings];
		Return[{}];
	];
	
	Return[Join@@Table[AllKnots[i], {i, minCrossings, maxCrossings}]];
];

AllLinks[crossingNumber_] := Module[{},
	If[2 <= crossingNumber <= 12,
		Return[AllLinks[crossingNumber, Alternating]~Join~AllLinks[crossingNumber, NonAlternating]];
	];
	
	Message[AllLinks::crossingNumberOutOfBounds, crossingNumber];
	Return[{}];
];

AllLinks[crossingNumber_, t_] := Module[{},
	If[2 <= crossingNumber <= 12,
		Return[Table[Link[crossingNumber, t, k], {k, NumberOfLinks[crossingNumber, t]}]];
	];
	
	Message[AllKnots::crossingNumberOutOfBounds, crossingNumber];
	Return[{}];
];

AllLinks[{minCrossings_, maxCrossings_}] := Module[{},
	If[minCrossings < 2 || minCrossings > 12,
		Message[AllLinks::crossingNumberOutOfBounds, minCrossings];
		Return[{}];
	];
	
	If[maxCrossings < 2 || maxCrossings > 12,
		Message[AllLinks::crossingNumberOutOfBounds, maxCrossings];
		Return[{}];
	];
	
	If[minCrossings > maxCrossings,
		Message[AllLinks::improperlyOrderedIndices, minCrossings, maxCrossings];
		Return[{}];
	];
	
	Return[Join@@Table[AllLinks[i], {i, minCrossings, maxCrossings}]];
];

End[];
EndPackage[];
