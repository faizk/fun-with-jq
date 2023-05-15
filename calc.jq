include "pc";

def posNumConstP: fmap(
  oneOrMore(digit);
  reverse|to_entries | map(.key|=exp10 | .key * .value) | add
);

def numConstP: orElse(
  foll(chars("-"); posNumConstP; - last);
  posNumConstP
);

def op2(pl; op; pr; f): foll(pl; follR(chars(op); pr); f);

def addExpr(pl; pr): op2(pl;"+";pr; .[0] + .[1]);
def mulExpr(pl; pr): op2(pl;"*";pr; .[0] * .[1]);
def divExpr(pl; pr): op2(pl;"/";pr; .[0] / .[1]);
def subExpr(pl; pr): op2(pl;"-";pr; .[0] - .[1]);

def parens(p): surr(chars("("); p; chars(")"));

def expr0(p): ws(orElse(numConstP; parens(p)));

def expr1(p): orElse(divExpr(expr0(p); expr1(p)); expr0(p));
def expr2(p): orElse(mulExpr(expr1(p); expr2(p)); expr1(p));
def expr3(p): orElse(addExpr(expr2(p); expr3(p)); expr2(p));
def expr4(p): orElse(subExpr(expr3(p); expr4(p)); expr3(p));
 
def calcP: ws(expr4(calcP));
