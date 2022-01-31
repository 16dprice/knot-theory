(* ::Package:: *)

BeginPackage["KnotTheory`"];

KnotTheoryVersion::usage = "
KnotTheoryVersion[] returns the current version of the KnotTheory package.
The version is determined based on versioning principles available at https://semver.org/
";

KnotTheoryWelcomeMessage::usage = "
KnotTheoryWelcomeMessage[] returns a string containing the welcome message
printed when KnotTheory` is first loaded.
";

KnotTheoryDirectory::usage = "
KnotTheoryDirectory[] returns the best guess KnotTheory` has for its
location on the host computer. It can be reset by the user.
";

CreditMessage::usage = "CreditMessage[cm] is used to print the string cm as a 'credit message'. Every credit message is printed at most once.";

KnotTheory::loading = "Loading precomputed data in `1`.";

Crossings::usage = "asdvas";

NumberOfKnots::usage = "asdfasdv";

Knot::usage = " asdklfjasdf";

AllKnots::usage = "alsdjkflasjdf";

KnotTheory::credits = "`1`";

Jones::usage = "asdlaksjdv";

Begin["`Private`"];

(* See https://semver.org/ for versioning principles. *)

MajorVersion = 0;
MinorVersion = 0;
PatchVersion = 0;

KnotTheoryVersion[] := StringJoin[
	ToString[MajorVersion],
	".",
	ToString[MinorVersion],
	".",
	ToString[PatchVersion]
];

KnotTheoryWelcomeMessage[] := StringJoin[
  "Loading KnotTheory` version ",
  KnotTheoryVersion[],
  ".\nRead more at http://katlas.org/wiki/KnotTheory."
];

(* Since this function is actually in the knot theory package, this error message should technically never fire... but just in case. *)
KnotTheoryDirectory::packageNotFound = "Can't find KnotTheory package on $Path. Ensure that the KnotTheory folder is in a folder on $Path.";

KnotTheoryDirectory[] := Module[{packageLocation},
	packageLocation = Flatten[FileInformation[ToFileName[#,"KnotTheory"]] & /@ ($Path /. "." -> Directory[])];
	If[Length[packageLocation] == 0, Message[KnotTheoryDirectory::packageNotFound] ];
	Return[File /. packageLocation];
];

CreditMessage[cm_String] := Module[{l = Length[$MessageList]},
  Message[KnotTheory::credits, cm];
  If[Length[$MessageList] > l, CreditMessage[cm] = Null];
];

Print[KnotTheoryWelcomeMessage[]]

Crossings[Knot[n_,__]] := n;
Crossings[Link[n_,__]] := n;

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

NumberOfKnots[n_] := NumberOfKnots[n, Alternating] + NumberOfKnots[n, NonAlternating];

NumberOfKnots[{n_, m_}]:= Sum[NumberOfKnots[k], {k,n,m}];
NumberOfKnots[{n_, m_}, t_]:= Sum[NumberOfKnots[k, t], {k,n,m}];

AllKnots[] = Flatten[{
  Table[Knot[n,k], {n,0,10}, {k,NumberOfKnots[n]}],
  Table[Knot[11, Alternating, k], {k, NumberOfKnots[11, Alternating]}],
  Table[Knot[11, NonAlternating, k], {k, NumberOfKnots[11, NonAlternating]}]
}];

End[];

Needs["KnotTheory`Jones4Knots`", FileNameJoin[{KnotTheoryDirectory[], "Jones4Knots.m"}]];

EndPackage[];
