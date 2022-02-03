(* ::Package:: *)

BeginPackage["KnotTheory`MinimumBraids`", {
	"KnotTheory`Common`"
}];

BR::usage = "
	BR stands for Braid Representative. BR[k,l] represents a
	braid on k strands with crossings l={i1,i2,...}, where a positive index
	i within the list l indicates a right-handed crossing between strand
	number i and strand number i+1 and a negative i indicates a left handed
	crossing between strands numbers |i| and |i|+1. Each ij can also be a
	list of non-adjacent (i.e., commuting) indices. BR also acts as a
	\"type caster\": BR[K] will return a braid whose closure is K if K is
	given in any format that KnotTheory` understands. BR[K] where K is is a
	named knot with up to 10 crossings returns a minimum braid
	representative for that knot.
";

BR::about = "
	The minimum braids representing the knots with up to 10 crossings were
	provided by Thomas Gittings. See his article on the subject at
	arXiv:math.GT/0401051. Vogel's algorithm was implemented by Dan Carney in
	the summer of 2005 at the University of Toronto.
";

Begin["`Private`"];

BR[br_BR] := br;

BR[TorusKnot[m_, n_]] := BR[n, Flatten[Table[Range[n - 1], {m}]]];

BR[k_Integer, s_String] := BR[
	k,
	ToCharacterCode[s] /. j_Integer :> If[j < 97, 64 - j, j - 96]
];

Mirror[BR[k_Integer, l_List]] := BR[k, -l];
BR[Mirror[L_Knot|L_Link]] := Mirror[BR[L]];

br[bi_Integer, bs_String] := (
	CreditMessage["
		The minimum braids representing the knots with up to 10 crossings
		were provided by Thomas Gittings. See arXiv:math.GT/0401051."
	];
	Return[BR[bi, bs]];
);

BR[Knot[0, 1]] := br[1, ""];
BR[Knot[3, 1]] := br[2, "AAA"];
BR[Knot[4, 1]] := br[3, "AbAb"];
BR[Knot[5, 1]] := br[2, "AAAAA"];
BR[Knot[5, 2]] := br[3, "AAABaB"];
BR[Knot[6, 1]] := br[4, "AABacBc"];
BR[Knot[6, 2]] := br[3, "AAAbAb"];
BR[Knot[6, 3]] := br[3, "AAbAbb"];
BR[Knot[7, 1]] := br[2, "AAAAAAA"];
BR[Knot[7, 2]] := br[4, "AAABaBCbC"];
BR[Knot[7, 3]] := br[3, "aaaaabAb"];
BR[Knot[7, 4]] := br[4, "aabAbbcBc"];
BR[Knot[7, 5]] := br[3, "AAAABaBB"];
BR[Knot[7, 6]] := br[4, "AAbACbC"];
BR[Knot[7, 7]] := br[4, "aBaBcBc"];
BR[Knot[8, 1]] := br[5, "AABaBCbdCd"];
BR[Knot[8, 2]] := br[3, "AAAAAbAb"];
BR[Knot[8, 3]] := br[5, "AABacBcdCd"];
BR[Knot[8, 4]] := br[4, "AAAbAbcBc"];
BR[Knot[8, 5]] := br[3, "aaaBaaaB"];
BR[Knot[8, 6]] := br[4, "AAAABacBc"];
BR[Knot[8, 7]] := br[3, "aaaaBaBB"];
BR[Knot[8, 8]] := br[4, "aaabACbCC"];
BR[Knot[8, 9]] := br[3, "AAAbAbbb"];
BR[Knot[8, 10]] := br[3, "aaaBaaBB"];
BR[Knot[8, 11]] := br[4, "AABaBBcBc"];
BR[Knot[8, 12]] := br[5, "AbACbdCd"];
BR[Knot[8, 13]] := br[4, "AAbAbbcBc"];
BR[Knot[8, 14]] := br[4, "AAABaBcBc"];
BR[Knot[8, 15]] := br[4, "AAbACBBBC"];
BR[Knot[8, 16]] := br[3, "AAbAAbAb"];
BR[Knot[8, 17]] := br[3, "AAbAbAbb"];
BR[Knot[8, 18]] := br[3, "AbAbAbAb"];
BR[Knot[8, 19]] := br[3, "aaabaaab"];
BR[Knot[8, 20]] := br[3, "aaaBAAAB"];
BR[Knot[8, 21]] := br[3, "AAABaaBB"];
BR[Knot[9, 1]] := br[2, "AAAAAAAAA"];
BR[Knot[9, 2]] := br[5, "AAABaBCbCDcD"];
BR[Knot[9, 3]] := br[3, "aaaaaaabAb"];
BR[Knot[9, 4]] := br[4, "AAAAABaBCbC"];
BR[Knot[9, 5]] := br[5, "aabAbbcBcdCd"];
BR[Knot[9, 6]] := br[3, "AAAAAABaBB"];
BR[Knot[9, 7]] := br[4, "AAAABaBCbCC"];
BR[Knot[9, 8]] := br[5, "AAbAbcBDcD"];
BR[Knot[9, 9]] := br[3, "AAAAABaBBB"];
BR[Knot[9, 10]] := br[4, "aabAbbbbcBc"];
BR[Knot[9, 11]] := br[4, "aaaaBacBc"];
BR[Knot[9, 12]] := br[5, "AAbACbCDcD"];
BR[Knot[9, 13]] := br[4, "aaaabAbbcBc"];
BR[Knot[9, 14]] := br[5, "aabACbCdCd"];
BR[Knot[9, 15]] := br[5, "aaabACbdCd"];
BR[Knot[9, 16]] := br[3, "aaaabbAbbb"];
BR[Knot[9, 17]] := br[4, "aBaBBBcBc"];
BR[Knot[9, 18]] := br[4, "AAABaBBBCbC"];
BR[Knot[9, 19]] := br[5, "aBaBBCbdCd"];
BR[Knot[9, 20]] := br[4, "AAAbACbCC"];
BR[Knot[9, 21]] := br[5, "aabAbCbdCd"];
BR[Knot[9, 22]] := br[4, "AbAbCbbbC"];
BR[Knot[9, 23]] := br[4, "AAABaBBCbCC"];
BR[Knot[9, 24]] := br[4, "AAbACbbbC"];
BR[Knot[9, 25]] := br[5, "AAbACBBdCd"];
BR[Knot[9, 26]] := br[4, "aaaBaBcBc"];
BR[Knot[9, 27]] := br[4, "AAbAbbCbC"];
BR[Knot[9, 28]] := br[4, "AAbACbbCC"];
BR[Knot[9, 29]] := br[4, "aBBcBaBcB"];
BR[Knot[9, 30]] := br[4, "AAbbAbCbC"];
BR[Knot[9, 31]] := br[4, "AAbAbCbCC"];
BR[Knot[9, 32]] := br[4, "aaBaBacBc"];
BR[Knot[9, 33]] := br[4, "AbAbbACbC"];
BR[Knot[9, 34]] := br[4, "AbAbCbAbC"];
BR[Knot[9, 35]] := br[5, "AABaBBCbbDcBDC"];
BR[Knot[9, 36]] := br[4, "aaaBaacBc"];
BR[Knot[9, 37]] := br[5, "AAbACbadCbCd"];
BR[Knot[9, 38]] := br[4, "AABBcBaBCCB"];
BR[Knot[9, 39]] := br[5, "aabACBadcBcd"];
BR[Knot[9, 40]] := br[4, "AbACbACbC"];
BR[Knot[9, 41]] := br[5, "AABacbbDCbCD"];
BR[Knot[9, 42]] := br[4, "aaaBAAcBc"];
BR[Knot[9, 43]] := br[4, "aaabaaCbC"];
BR[Knot[9, 44]] := br[4, "AAABaacBc"];
BR[Knot[9, 45]] := br[4, "AABaBACbC"];
BR[Knot[9, 46]] := br[4, "AbAbCBaBC"];
BR[Knot[9, 47]] := br[4, "AbAbcbAbc"];
BR[Knot[9, 48]] := br[4, "aabAbaCbAbC"];
BR[Knot[9, 49]] := br[4, "aabaaCbAbcc"];
BR[Knot[10, 1]] := br[6, "AABaBCbCDceDe"];
BR[Knot[10, 2]] := br[3, "AAAAAAAbAb"];
BR[Knot[10, 3]] := br[6, "AABaBCbdCdeDe"];
BR[Knot[10, 4]] := br[5, "AAAbAbcBcdCd"];
BR[Knot[10, 5]] := br[3, "aaaaaaBaBB"];
BR[Knot[10, 6]] := br[4, "AAAAAABacBc"];
BR[Knot[10, 7]] := br[5, "AABaBCbCCdCd"];
BR[Knot[10, 8]] := br[4, "AAAAAbAbcBc"];
BR[Knot[10, 9]] := br[3, "aaaaaBaBBB"];
BR[Knot[10, 10]] := br[5, "AAbAbbcBcdCd"];
BR[Knot[10, 11]] := br[5, "AAAABacBcdCd"];
BR[Knot[10, 12]] := br[4, "aaaaabACbCC"];
BR[Knot[10, 13]] := br[6, "AABacBDceDe"];
BR[Knot[10, 14]] := br[4, "AAAAABaBcBc"];
BR[Knot[10, 15]] := br[4, "aaaaBaBCbCC"];
BR[Knot[10, 16]] := br[5, "aabAbbCbCDcD"];
BR[Knot[10, 17]] := br[3, "AAAAbAbbbb"];
BR[Knot[10, 18]] := br[5, "AAABaBcBcdCd"];
BR[Knot[10, 19]] := br[4, "AAAAbAbbcBc"];
BR[Knot[10, 20]] := br[5, "AAAABaBCbdCd"];
BR[Knot[10, 21]] := br[4, "AABaBBBBcBc"];
BR[Knot[10, 22]] := br[4, "aaaabACbCCC"];
BR[Knot[10, 23]] := br[4, "AAbAbbbbcBc"];
BR[Knot[10, 24]] := br[5, "AABaBBBCbdCd"];
BR[Knot[10, 25]] := br[4, "AAAABaBBcBc"];
BR[Knot[10, 26]] := br[4, "AAAbAbbbcBc"];
BR[Knot[10, 27]] := br[4, "AAAABaBcBcc"];
BR[Knot[10, 28]] := br[5, "aabAbbcBDcDD"];
BR[Knot[10, 29]] := br[5, "AAAbACbdCd"];
BR[Knot[10, 30]] := br[5, "AABaBBCbCdCd"];
BR[Knot[10, 31]] := br[5, "AAABacBccdCd"];
BR[Knot[10, 32]] := br[4, "aaaBaBBCbCC"];
BR[Knot[10, 33]] := br[5, "AABaBcBccdCd"];
BR[Knot[10, 34]] := br[5, "aaabAbcBDcDD"];
BR[Knot[10, 35]] := br[6, "AbAbcBDceDe"];
BR[Knot[10, 36]] := br[5, "AAABaBCbCdCd"];
BR[Knot[10, 37]] := br[5, "AAABacBcdCdd"];
BR[Knot[10, 38]] := br[5, "AAABaBBCbdCd"];
BR[Knot[10, 39]] := br[4, "AAABaBBBcBc"];
BR[Knot[10, 40]] := br[4, "aaabAbbCbCC"];
BR[Knot[10, 41]] := br[5, "aBaBBcBDcD"];
BR[Knot[10, 42]] := br[5, "AAbAbCbdCd"];
BR[Knot[10, 43]] := br[5, "AAbACbdCdd"];
BR[Knot[10, 44]] := br[5, "AAbACbCdCd"];
BR[Knot[10, 45]] := br[5, "AbAbCbCdCd"];
BR[Knot[10, 46]] := br[3, "aaaaaBaaaB"];
BR[Knot[10, 47]] := br[3, "aaaaaBaaBB"];
BR[Knot[10, 48]] := br[3, "AAAAbbAbbb"];
BR[Knot[10, 49]] := br[4, "AAAAbACBBBC"];
BR[Knot[10, 50]] := br[4, "aabAbbCbbbC"];
BR[Knot[10, 51]] := br[4, "aabAbbCbbCC"];
BR[Knot[10, 52]] := br[4, "aaaBaaBBCbC"];
BR[Knot[10, 53]] := br[5, "AABaBcBDCCCD"];
BR[Knot[10, 54]] := br[4, "aaaBaaBCbCC"];
BR[Knot[10, 55]] := br[5, "AAABacBDCCCD"];
BR[Knot[10, 56]] := br[4, "aaabAbCbbbC"];
BR[Knot[10, 57]] := br[4, "aaabAbCbbCC"];
BR[Knot[10, 58]] := br[6, "aBacBDCCeDe"];
BR[Knot[10, 59]] := br[5, "AbAbCbbdCd"];
BR[Knot[10, 60]] := br[5, "AbAbbCbCBDcD"];
BR[Knot[10, 61]] := br[4, "aaaBaaaBCbC"];
BR[Knot[10, 62]] := br[3, "aaaaBaaaBB"];
BR[Knot[10, 63]] := br[5, "AAbACBBBCDcD"];
BR[Knot[10, 64]] := br[3, "aaaBaaaBBB"];
BR[Knot[10, 65]] := br[4, "aabAbCbbbCC"];
BR[Knot[10, 66]] := br[4, "AAAbACBBBCC"];
BR[Knot[10, 67]] := br[5, "AAABaBCbbdCBdC"];
BR[Knot[10, 68]] := br[5, "aaBaBBCbbDcBDC"];
BR[Knot[10, 69]] := br[5, "aabACbadCbCd"];
BR[Knot[10, 70]] := br[5, "AbACbbbdCd"];
BR[Knot[10, 71]] := br[5, "AAbACbbdCd"];
BR[Knot[10, 72]] := br[4, "aaaabbAbCbC"];
BR[Knot[10, 73]] := br[5, "AABaBAcBcDcD"];
BR[Knot[10, 74]] := br[5, "AABaBBCbbdCBdC"];
BR[Knot[10, 75]] := br[5, "aBaBcBBdCbdc"];
BR[Knot[10, 76]] := br[4, "aaaabACbbbC"];
BR[Knot[10, 77]] := br[4, "aaaabACbbCC"];
BR[Knot[10, 78]] := br[5, "AABaBAcBDcDD"];
BR[Knot[10, 79]] := br[3, "AAAbbAAbbb"];
BR[Knot[10, 80]] := br[4, "AAAbAACBBBC"];
BR[Knot[10, 81]] := br[5, "aaBacbbDCCCD"];
BR[Knot[10, 82]] := br[3, "AAAAbAbAbb"];
BR[Knot[10, 83]] := br[4, "aabAbCbbCbC"];
BR[Knot[10, 84]] := br[4, "aaabACbbCbC"];
BR[Knot[10, 85]] := br[3, "AAAAbAAbAb"];
BR[Knot[10, 86]] := br[4, "AAbAbAbbcBc"];
BR[Knot[10, 87]] := br[4, "aaabACbCbCC"];
BR[Knot[10, 88]] := br[5, "AbACbCbdCd"];
BR[Knot[10, 89]] := br[5, "AbAbcBADCbCD"];
BR[Knot[10, 90]] := br[4, "AAbAbcBAcbb"];
BR[Knot[10, 91]] := br[3, "AAAbAbbAbb"];
BR[Knot[10, 92]] := br[4, "aaabbCbAbCb"];
BR[Knot[10, 93]] := br[4, "AAbAAbAbcBc"];
BR[Knot[10, 94]] := br[3, "aaaBaaBBaB"];
BR[Knot[10, 95]] := br[4, "AAbbCbAbccb"];
BR[Knot[10, 96]] := br[5, "AbaCbaCdCbCd"];
BR[Knot[10, 97]] := br[5, "aabAbaCbAbcDcD"];
BR[Knot[10, 98]] := br[4, "AABBcBaBBcB"];
BR[Knot[10, 99]] := br[3, "AAbAAbbAbb"];
BR[Knot[10, 100]] := br[3, "AAAbAAbAAb"];
BR[Knot[10, 101]] := br[5, "aaabAcBacbbdCd"];
BR[Knot[10, 102]] := br[4, "AAbACbAbbcc"];
BR[Knot[10, 103]] := br[4, "AABacBBcBBc"];
BR[Knot[10, 104]] := br[3, "AAAbbAbAbb"];
BR[Knot[10, 105]] := br[5, "aaBacbbDCbCD"];
BR[Knot[10, 106]] := br[3, "aaaBaBaaBB"];
BR[Knot[10, 107]] := br[5, "AAbAcbbDcBcD"];
BR[Knot[10, 108]] := br[4, "aaBaacBaBCC"];
BR[Knot[10, 109]] := br[3, "AAbAbbAAbb"];
BR[Knot[10, 110]] := br[5, "AbACBBBdcBcd"];
BR[Knot[10, 111]] := br[4, "aabbCbbAbCb"];
BR[Knot[10, 112]] := br[3, "AAAbAbAbAb"];
BR[Knot[10, 113]] := br[4, "aaabCbAbCbC"];
BR[Knot[10, 114]] := br[4, "AABacBcBcBc"];
BR[Knot[10, 115]] := br[5, "aBacbbDCbCCD"];
BR[Knot[10, 116]] := br[3, "AAbAAbAbAb"];
BR[Knot[10, 117]] := br[4, "aabbCbAbCbC"];
BR[Knot[10, 118]] := br[3, "aaBaBaBBaB"];
BR[Knot[10, 119]] := br[4, "AAbACbAbccb"];
BR[Knot[10, 120]] := br[5, "AABacbADCBBCCD"];
BR[Knot[10, 121]] := br[4, "AABcBaBcBcB"];
BR[Knot[10, 122]] := br[4, "aabCbACbCbC"];
BR[Knot[10, 123]] := br[3, "AbAbAbAbAb"];
BR[Knot[10, 124]] := br[3, "aaaaabaaab"];
BR[Knot[10, 125]] := br[3, "aaaaaBAAAB"];
BR[Knot[10, 126]] := br[3, "AAAAABaaaB"];
BR[Knot[10, 127]] := br[3, "AAAAABaaBB"];
BR[Knot[10, 128]] := br[4, "aaabaabbcBc"];
BR[Knot[10, 129]] := br[4, "aaaBAAcBAcB"];
BR[Knot[10, 130]] := br[4, "aaaBAABBCbC"];
BR[Knot[10, 131]] := br[4, "AAABaaBBCbC"];
BR[Knot[10, 132]] := br[4, "aaaBAABCbCC"];
BR[Knot[10, 133]] := br[4, "AAABaaBCbCC"];
BR[Knot[10, 134]] := br[4, "aaabaabcBcc"];
BR[Knot[10, 135]] := br[4, "aaabAbCBBBC"];
BR[Knot[10, 136]] := br[5, "aBaBCbbdCd"];
BR[Knot[10, 137]] := br[5, "AbAbCBBdCd"];
BR[Knot[10, 138]] := br[5, "AbAbcbbDcD"];
BR[Knot[10, 139]] := br[3, "aaaabaaabb"];
BR[Knot[10, 140]] := br[4, "aaaBAAABCbC"];
BR[Knot[10, 141]] := br[3, "aaaaBAAABB"];
BR[Knot[10, 142]] := br[4, "aaabaaabcBc"];
BR[Knot[10, 143]] := br[3, "AAAABaaaBB"];
BR[Knot[10, 144]] := br[4, "AABaBAcBAcb"];
BR[Knot[10, 145]] := br[4, "AABaBACBaBC"];
BR[Knot[10, 146]] := br[4, "AAbAbaCbAbC"];
BR[Knot[10, 147]] := br[4, "aaaBaBCbAbC"];
BR[Knot[10, 148]] := br[3, "AAAABaaBaB"];
BR[Knot[10, 149]] := br[3, "AAAABaBaBB"];
BR[Knot[10, 150]] := br[4, "aaaBaacBAcb"];
BR[Knot[10, 151]] := br[4, "aaabAAcBacB"];
BR[Knot[10, 152]] := br[3, "AAABBAABBB"];
BR[Knot[10, 153]] := br[4, "AAABAAcbbbc"];
BR[Knot[10, 154]] := br[4, "aabAbacbbbc"];
BR[Knot[10, 155]] := br[3, "aaabAAbAAb"];
BR[Knot[10, 156]] := br[4, "AAAbaaCBaBC"];
BR[Knot[10, 157]] := br[3, "aaabbAbAbb"];
BR[Knot[10, 158]] := br[4, "AAABaacbAbc"];
BR[Knot[10, 159]] := br[3, "AAABaBaaBB"];
BR[Knot[10, 160]] := br[4, "aaabaaCbAbC"];
BR[Knot[10, 161]] := br[3, "AAABaBAABB"];
BR[Knot[10, 162]] := br[4, "AABaaBBAcBc"];
BR[Knot[10, 163]] := br[4, "aaBAAcbAbbc"];
BR[Knot[10, 164]] := br[4, "aaBaBBCbAbC"];
BR[Knot[10, 165]] := br[4, "aabACbAbccb"];

End[];
EndPackage[];
