include "pc";

def posNumConstP: fmap(
  oneOrMore(digit);
  reverse|to_entries | map(.key|=exp10 | .key * .value) | add
);

def numConstP: orElse(
  foll(chars("-"); posNumConstP; - last);
  posNumConstP
);

def openP: chars("(");
def closeP: chars(")");

def atomCP:
  exceptP(
    orElse(
      orElse(chars(","); ws);
      orElse(openP; closeP));
    anyChar);

def atomP:
  orElse(
    fmap(numConstP; {num: .});
    fmap(oneOrMore(atomCP); {sym: join("")}));

def quoteMP(qp; p; $sym):
  foll(qp; p; {list: [{sym: $sym}, .[1]]});

def quoteP(p):      quoteMP(chars("'"); p; "quote");
def quasiquoteP(p): quoteMP(chars("`"); p; "quasiquote");
def unquoteP(p):    quoteMP(chars(","); p; "unquote");

def maybeQuotedP(p): orElse(quoteP(p); orElse(unquoteP(p); quasiquoteP(p)));

def sexpP:
  orElse(
    maybeQuotedP(sexpP);
    orElse(
      atomP;
      fmap(
        surr(
          openP;
          delimited(sexpP; ws);
          closeP);
        {list: .})));

def lookup(environ):
  environ[.];


def apply(environ):
  .;

def internToLisp:
  if (has("list")) then
    .list | map(internToLisp) | join(" ") |
    "(\(.))"
  elif (has("num")) then
    .num
  elif (has("sym")) then
    .sym
  else
    "ERROR" | error
  end
;

def eval(environ):
  if (has("num")) then
    .num
  elif (has("sym")) then
    (.sym) as $sym |
    if (environ | has($sym))
    then environ[$sym] | eval(environ)
    else "undefined var: \($sym)" | error end
  elif (has("list")) then
    .list |
    map(eval(environ))
  else
    "ERROR" | error
  end;

def eval:
  eval(
    {
      "+": ""
    }
  );
