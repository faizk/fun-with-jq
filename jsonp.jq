include "pc";

def word(s; $value): fmap(chars(s); $value);
def nullP: word("null"; null);
def boolP: orElse(word("true"; true); word("false"; false));

def numP:   fmap(
  oneOrMore(digit);
  reverse|to_entries|map(.key|=exp10 | .key*.value) | add
);

def strP:   fmap(
  surr(
    chars("\"");
    zeroOrMore(except(. == "\""; anyChar));
    chars("\"")
  );
  join("")
);

def primP: orElse(nullP; orElse(boolP; orElse(numP; strP)));

def arrayP(p):
  surr(chars("["); delimited(p; chars(",")); chars("]"));

def kvP(p): foll(
  follL(ws(strP); chars(":"));
  p;
  {key: .[0], value: .[1]}
);

def objP(p): fmap(
  surr(
    chars("{");
    delimited(kvP(p); chars(","));
    chars("}")
  );
  from_entries
);

def jsonP: ws(
  orElse(primP; orElse(arrayP(jsonP); objP(jsonP)))
);
