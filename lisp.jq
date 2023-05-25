include "pc";

def posNumConstP: fmap(
  oneOrMore(digit);
  reverse|to_entries | map(.key|=exp10 | .key * .value) | add
);

def numConstP: orElse(
  foll(chars("-"); posNumConstP; - last);
  posNumConstP);

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
    fmap(numConstP; {N: .});
    fmap(oneOrMore(atomCP); {SYM: join("")}));

def quoteMP(qp; p; $sym):
  foll(qp; p; {list: [{SYM: $sym}, .[1]]});

def quoteP(p):      quoteMP(chars("'"); p; "quote");
def quasiquoteP(p): quoteMP(chars("`"); p; "quasiquote");
def unquoteP(p):    quoteMP(chars(","); p; "unquote");

def maybeQuotedP(p): orElse(quoteP(p); orElse(unquoteP(p); quasiquoteP(p)));

def ws1(p): surr(p; oneOrMore(ws));

def pairP(p):
  fmap(
    surr(
      openP;
      foll(p; follR(ws1(chars(".")); p));
      closeP);
    {car: .[0], cdr: .[1]});

def arr2ConsL:
  if (length >= 1) then {car: .[0], cdr: (.[1:]|arr2ConsL)}
  else null end;

def isNIL: . == null;
def isSym: has("SYM");
def isLitNum: has("N");
def isAtom: isLitNum or isSym or isNIL;
def isCons: has("car") and has("cdr");

def consL2Arr:
  if (isCons) then [.car] +
    (.cdr | consL2Arr)
  else
    if (isNIL) then [] else "not a list" | error end
  end;

def isConsL:
  ((isCons) and (.cdr | isConsL)) or isNIL;

def listP(p):
  fmap(
    surr(
      openP;
      delimited(p; ws);
      closeP);
    arr2ConsL);

def sexpP:
  orElse(
    maybeQuotedP(sexpP);
    orElse(
      atomP;
      orElse(
        pairP(sexpP);
        listP(sexpP))));

def builtin_add: map(.N) | add | {N: .};
def builtin_sub: map(.N) | (if (length >=2) then . else [0] + . end) |
  reduce .[1:][] as $item (.[0]; . - $item);
def builtin_mul: map(.N) | reduce .[] as $item (1; . * $item) | {N: .};
def builtin_div: map(.N) | reduce .[] as $item (1; . / $item) | {N: .};
def builtin_car: .[0].car;
def builtin_cdr: .[0].cdr;
def builtin_cons: {car: .[0], cdr: .[1]};
def builtin_lte: map(.N) | (.[0] <= .[1]);
def builtin_lt:  map(.N) | (.[0] <  .[1]);
def builtin_gte: map(.N) | (.[0] >= .[1]);
def builtin_gt:  map(.N) | (.[0] >  .[1]);
def builtin_eq:  map(.N) | (.[0] == .[1]);
def builtin_equal:  (.[0] == .[1]);

def eval(environ):
  def apply($thing):
    if ($thing | has("lambda")) then
      ($thing.lambda) as $lambda |
      ($lambda.environ +
       with_entries(.key |= ($lambda.fargs[.]))
      ) as $newEnv |
      $lambda.body | eval($newEnv)
    elif ($thing | has("builtIn")) then
      (.) as $args |
      ($thing.builtIn) |
      if   (. == "+")      then $args | builtin_add
      elif (. == "*")      then $args | builtin_mul
      elif (. == "-")      then $args | builtin_sub
      elif (. == "/")      then $args | builtin_div
      elif (. == "<=")     then $args | builtin_lte
      elif (. == "<")      then $args | builtin_lt
      elif (. == ">=")     then $args | builtin_gte
      elif (. == ">")      then $args | builtin_gt
      elif (. == "=")      then $args | builtin_eq
      elif (. == "equal?") then $args | builtin_equal
      elif (. == "car")    then $args | builtin_car
      elif (. == "cdr")    then $args | builtin_cdr
      elif (. == "cons")   then $args | builtin_cons
      else "not a built-in? `\($thing.builtIn)`.." | error
      end
    else
      "can't apply: \($thing)" | error
    end;

  if (isLitNum) then .
  elif (isSym) then
    (.SYM) as $sym |
    if (environ | has($sym))
    then environ[$sym]
    else "undefined var: \($sym)" | error end
  elif (isNIL) then .
  elif (isConsL) then
    (consL2Arr) as $l |

    if (($l | length == 3) and
        ($l[0] == {SYM: "lambda"}) and
        ($l[1] | isConsL))
    then
      {lambda: {fargs: ($l[1]|consL2Arr|map(.SYM)),
                body: $l[2],
                environ: environ}}

    elif (($l | length >= 3) and
          ($l[0] == {SYM: "let"}) and
          ($l[1] | isConsL))
    then
      ($l[1] | consL2Arr |
       reduce .[] as $kv (
         environ;
         (.) as $envSoFar |
         ($kv.car.SYM) as $k |
         ($kv.cdr.car | eval($envSoFar)) as $v |
         . + {"\($k)": $v})
      ) as $newEnv |
      ($l[2]) as $body |
      $body | eval($newEnv)

    else # apply
      ($l[0] | eval(environ)) as $f |
      ($l[1:]| map(eval(environ))) as $args |
      $args | apply($f)
    end
  else
    "ERROR" | error
  end;

def declareBuiltIns: map({key: ., value: {builtIn: .}}) | from_entries;

def eval: eval(
  (["+", "-", "*", "/",
   ">=", ">", "<=", "<", "=",
   "equal?", "empty?",
   "car", "cons", "cdr", "list"
  ] | declareBuiltIns) +
  {
  }
);

def show:
  if (isConsL) then consL2Arr | map(show) | join (" ") | "(\(.))"
  elif (isCons) then "(\(.car|show) . \(.cdr|show))"
  elif (isLitNum) then .N | tostring
  elif (isNIL) then "()"
  elif (isSym) then .SYM
  else . end;
