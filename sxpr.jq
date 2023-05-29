include "pc";

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

def atomCP:
  exceptP(
    orElse(
      orElse(chars(","); orElse(ws; orElse(chars("'"); chars("`"))));
      orElse(openP; closeP));
    anyChar);

def atomP:
  orElse(nilP;
    orElse(fmap(numConstP; {N: .});
      orElse(boolP;
        fmap(oneOrMore(atomCP); {SYM: join("")}))));

def quoteMP(qp; p; $sym): foll(qp; p; {"\($sym)": .[1]});
def quoteP(p):      quoteMP(chars("'"); p; "Q");
def quasiquoteP(p): quoteMP(chars("`"); p; "QQ"); ## TODO: use these!
def unquoteP(p):    quoteMP(chars(","); p; "UQ");

def maybeQuotedP(p): orElse(quoteP(p); orElse(unquoteP(p); quasiquoteP(p)));

def pairP(p): def dotP: surr(chars("."); oneOrMore(ws));
  fmap(
    surr(openP; ws(foll(p; follR(dotP; p))); closeP);
    {car: .[0], cdr: .[1]});

def arr2ConsL:
  if (length >= 1) then {car: .[0], cdr: (.[1:]|arr2ConsL)}
  else null end;

def isNIL:    . == null;
def isSym:    has("SYM");
def isLitNum: has("N");
def isBool:   has("B");
def isQ:      has("Q");
def isQQ:     has("QQ");
def isUQ:     has("UQ");
def isAtom:   isLitNum or isBool or isSym or isNIL;
def isCons:   has("car") and has("cdr") and (length == 2);

def consL2Arr:
  if (isCons) then [.car] + (.cdr | consL2Arr)
  elif (isNIL) then []
  else "not a list" | error end;

def isConsL: ((isCons) and (.cdr | isConsL)) or isNIL;

def listP(p):
  fmap(
    surr(openP; ws(delimited(p; oneOrMore(ws))); closeP);
    arr2ConsL);

def sxprP:
  orElse(maybeQuotedP(sxprP);
    orElse(atomP;
      orElse(pairP(sxprP); listP(sxprP))));

def show:
  if   (isNIL) then "()"
  elif (isConsL) then consL2Arr | map(show) | join (" ") | "(\(.))"
  elif (isCons) then "(\(.car|show) . \(.cdr|show))"
  elif (isLitNum) then .N | tostring
  elif (isSym) then .SYM
  elif (isBool) then if (.B) then "#t" else "#f" end
  elif (isQ) then "'\(.Q|show)"
  elif (isQQ) then "`\(.QQ|show)"
  elif (isUQ) then ",\(.UQ|show)"
  else . end;
