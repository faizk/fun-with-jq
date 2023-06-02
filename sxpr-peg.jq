# a drop-in replacement for sxpr-pc.jq (parser-combinator version)
# somewhat more compact than the parser-combinator version,
# but that's mainly thanks to the use of regex (the pc version stays strictly char-by-char as a demonstration/exercise)
include "peg";
include "sxpr";

def ws0p: consume("[\\n ]*");
def ws1p: consume("[\\n ]+");

def Sxpr:
  def openp: consume("[(]");
  def closep: consume("[)]");
  def inParens(E): openp | ws0p | E | ws0p | closep;

  def Sym: parse("[^()\\s',`0-9][^()\\s',`]*"; {SYM: .});
  def Num: parse("[0-9]+"; {N: tonumber});
  def NIL: pMap(openp | ws0p | closep; null);
  def Bool: parse("#t"; {B: true}) // parse("#f"; {B: false});

  def Atom: NIL // Num // Bool // Sym;

  def Pair:
    def dotp: ws1p|consume("\\.")|ws1p;
    pMap(
      inParens(Sxpr | dotp | Sxpr);
      {car: .[0], cdr: .[1]});
  def List:
    def wsList: pMap(
      Sxpr | ((ws1p|wsList) // (.result += [null]));
      {car: .[0], cdr: .[1]});
    inParens(wsList);

  def Q:  consume("[']") | pMap(Sxpr; {Q: first});
  def QQ: consume("[`]") | pMap(Sxpr; {QQ: first});
  def UQ: consume("[,]") | pMap(Sxpr; {UQ: first});

  Atom // Pair // List // (Q // QQ // UQ)
;

def pcCompact(E; f): {remainder: ., result: []} |
  [E | {a: (.result|f), rest: (.remainder)}];

# Public interface
def sxprP: pcCompact(Sxpr; .[]);
def sxprsP: pcCompact(Sxpr | star(ws1p |Sxpr); .);
