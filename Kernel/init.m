(* ::Package:: *)

BeginPackage["KnotTheory`"];

KnotTheoryVersion::usage = "
	KnotTheoryVersion[] returns the current version of the KnotTheory package.
	The version is determined based on versioning principles available at https://semver.org/
";

KnotTheoryWelcomeMessage::usage = "
	KnotTheoryWelcomeMessage[] returns a string containing the welcome message printed when KnotTheory` is first loaded.
";

KnotTheoryDirectory::usage = "
	KnotTheoryDirectory[] returns the best guess KnotTheory` has for its location on the host computer.
	It can be reset by the user.
";

CreditMessage::usage = "
	CreditMessage[cm] is used to print the string cm as a 'credit message'. Every credit message
	is printed at most once.
";

KnotTheory::loading = "Loading precomputed data in `1`.";

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
	"\nRead more at http://katlas.org/wiki/KnotTheory."
];

(*
	Since this function is actually in the knot theory package, this error message should technically never fire... but just in case.
	It has fired during development of this package due to not having the development directory in the $Path variable, so it's at least
	useful for development purposes.
*)
KnotTheoryDirectory::packageNotFound = "
	Can't find KnotTheory package on $Path. Ensure that the KnotTheory folder is in a folder in $Path.
";

KnotTheoryDirectory[] := Module[{packageLocation},
	packageLocation = Select[ToFileName[#, "KnotTheory"] & /@ ($Path /. "." -> Directory[]), DirectoryQ[#]&];
	If[Length[packageLocation] == 0, Message[KnotTheoryDirectory::packageNotFound] ];
	Return[File[packageLocation[[1]]]];
];

CreditMessage[cm_String] := Module[{l = Length[$MessageList]},
	Message[KnotTheory::credits, cm];
	If[Length[$MessageList] > l, CreditMessage[cm] = Null];
];

Print[KnotTheoryWelcomeMessage[]]

End[];

EndPackage[];

DeclarePackage["KnotTheory`Common`", {
	"Knot",
	"Link",
	"TorusKnot",
	"Crossings",
	"NumberOfKnots",
	"NumberOfLinks",
	"AllKnots",
	"AllLinks",
	"Alternating",
	"NonAlternating"
}];
DeclarePackage["KnotTheory`Jones4Knots`", {
	"Jones"
}];
DeclarePackage["KnotTheory`MinimumBraids`", {
	"BR"
}];
DeclarePackage["KnotTheory`PDCode`", {
	"PD",
	"X",
	"PositiveQ",
	"NegativeQ",
	"Loop",
	"Mirror",
	"AlternatingQ",
	"PositiveCrossings",
	"NegativeCrossings",
	"ConnectedSum"
}];
DeclarePackage["KnotTheory`KnotDrawing`", {
	"DrawPD"
}];



