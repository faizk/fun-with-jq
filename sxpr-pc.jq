include "pc";
include "sxpr";

def posNumConstP: fmap(
  oneOrMore(digit);
  reverse|to_entries | map(.key|=exp10 | .key * .value) | add);

def numConstP: orElse(
  foll(chars("-"); posNumConstP; - last);
  posNumConstP);

def openP: chars("(");
def closeP: chars(")");

def boolP: orElse(fmap(chars("#t"); {B: true}); fmap(chars("#f"); {B: false}));
def nilP: foll(openP; follR(zeroOrMore(ws); closeP); null);

def atomCP: exceptP(
    anyOf(chars(","), ws, chars("'"), chars("`"), openP, closeP);
    anyChar);

def atomP: anyOf(nilP,
  fmap(numConstP; {N: .}), boolP, fmap(oneOrMore(atomCP); {SYM: join("")}));

def quoteMP(qp; p; $sym): foll(qp; p; {"\($sym)": .[1]});
def quoteP(p):      quoteMP(chars("'"); p; "Q");
def quasiquoteP(p): quoteMP(chars("`"); p; "QQ"); ## TODO: use these!
def unquoteP(p):    quoteMP(chars(","); p; "UQ");

def maybeQuotedP(p): anyOf(quoteP(p), unquoteP(p), quasiquoteP(p));

def pairP(p): def dotP: surr(chars("."); oneOrMore(ws));
  fmap(
    surr(openP; ws(foll(p; follR(dotP; p))); closeP);
    {car: .[0], cdr: .[1]});

def listP(p):
  fmap(
    surr(openP; ws(delimited(p; oneOrMore(ws))); closeP);
    arr2ConsL);

def sxprP: anyOf(maybeQuotedP(sxprP), atomP, pairP(sxprP), listP(sxprP));
def sxprsP: oneOrMore(ws(sxprP));
