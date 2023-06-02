include "sxpr";
include "sxpr-pc";

def assumeArity(cond; $msg): if (length | cond) then . else error("arity mismatch: given \(length) for expected \($msg)") end;
def assumeArityEq($expected): assumeArity(. == $expected; $expected);
def assumeNum:      if (.N | type != "number") then error("type error: not a number: \(.)") else .N end;
def numOp(f; $id):  map(assumeNum) | (if (length >= 2) then . else [$id] + . end) |
                      reduce .[1:][] as $r (.[0]; {l: ., $r} | f) | {N: .};
def numBoolOp(f):   map(assumeNum) | assumeArity(. >= 1; ">= 1") |
                      reduce .[1:][] as $r ({l: .[0], B: true}; (.l) as $l |
                        if ((.B) and ({$l,$r} | f)) then {l: $r, B} else {$l, B: false} end) | {B};
def assumeOne(f):   assumeArityEq(1) | .[0] | f;
def assumePair(f):  assumeOne(if (isCons) then f else error("type error: \(.) is not a cons cell") end);

# The `environ` is a map of `Str->Loc`, where `Loc` is just a number (an index in an array)
def lookup($environ): (.) as $sym | # Note that the "errors" thrown represent a bug in *this* code, not the users'
  if ($environ|has($sym)) then $environ[$sym] else "undefined variable: [\($sym)]" | error end;
# `Loc`s are indexes in the `mem`, which is an array that we just pass around.
def fetch($mem): (.) as $loc | $mem[$loc] |
  if ((type == "object") and (has("V"))) then .V else "NPE @ \($loc)" | error end;
def nextLoc($mem): ($mem | length);
def alloc($mem): (.) as $V | (nextLoc($mem)) as $loc |
  ($mem | (.[$loc] = {$V})) as $mem | {$mem, $loc};
# IN: sxpr V; out: {$environ, $mem}
def allocAssoc($sym; $environ; $mem): alloc($mem) as {$mem, $loc} |
  {$environ, $mem} | .environ |= (. + {"\($sym)": $loc});

# in ENV; out: MEM - this is used to convert a `let` behaviour to `letrec`-like behaviour
def recursiveEnv($mem): (.) as $env |
  to_entries | map(
    (.key) as $k | (.value) as $loc |
    ($loc | fetch($mem)) as $V |
    select($V | has("lambda")) |
    ($V | .lambda.environ |= $env) as $updated |
    {$loc, $updated}
  ) |
  reduce .[] as {$loc, $updated} ($mem; .[$loc] |= {V: $updated})
;

def eval(environ; $mem):
  def apply($thing; $mem):
    if ($thing | has("lambda")) then
      ($thing.lambda) as $lambda | assumeArityEq($lambda.fargs|length) |
      (reduce .[] as $arg (
          {$mem, locs: []};
          (.locs) as $locs | (.mem) as $mem |
          $arg | alloc($mem) | {mem, locs: ($locs + [.loc])}
        )) as {$mem, $locs} |
      ($locs | with_entries(.key |= ($lambda.fargs[.]))) as $argsEnv |
      $lambda.body | eval($lambda.environ + $argsEnv; $mem)
    elif ($thing | has("builtIn")) then
      ($thing.builtIn) as $f |
      if   ($f == "+")      then numOp(.l + .r; 0)
      elif ($f == "*")      then numOp(.l * .r; 1)
      elif ($f == "-")      then numOp(.l - .r; 0)
      elif ($f == "/")      then numOp(.l / .r; 1)
      elif ($f == "<=")     then numBoolOp(.l <= .r)
      elif ($f == "<")      then numBoolOp(.l <  .r)
      elif ($f == ">=")     then numBoolOp(.l >= .r)
      elif ($f == ">")      then numBoolOp(.l >  .r)
      elif ($f == "=")      then numBoolOp(.l == .r)
      elif ($f == "equal?") then (.[0] == .[1])  | {B: .}
      elif ($f == "empty?") then assumeOne(isNIL | {B: .})
      elif ($f == "car")    then assumePair(.car)
      elif ($f == "cdr")    then assumePair(.cdr)
      elif ($f == "cons")   then {car: .[0], cdr: .[1]}
      elif ($f == "list")   then arr2ConsL
      else "not a built-in? `\($f)`.." | error
      end
      | {$mem, V: .} # no new memory allocated by builtins!
    else
      "can't apply: \($thing)" | error
    end
  ; # END of `apply`!

  if (isLitNum)  then {$mem, V:.}
  elif (isBool)  then {$mem, V:.}
  elif (isQ)     then {$mem, V:.Q}
  elif (isNIL)   then {$mem, V:.}
  elif (isSym)   then .SYM | lookup(environ) | fetch($mem) | {$mem, V: .}

  elif (isConsL) then (consL2Arr) as $l | def kw($w): ($l[0] == {SYM: $w});

    if (kw("lambda") and ($l|length == 3) and ($l[1]|isConsL)) then
      {lambda: {fargs: ($l[1]|consL2Arr|map(.SYM)),
                body: $l[2],
                environ: environ}} | {$mem, V: .}

    elif ((kw("let") or kw("letrec")) and ($l|length >= 3) and ($l[1]|isConsL)) then
      (reduce ($l[1]|consL2Arr)[] as {car: {SYM: $k}, cdr: {car: $vexp}} (
         {environ: environ, $mem};
         (.environ) as $environ | (.mem) as $mem |
         ($vexp | eval($environ; $mem)) as {$mem, $V} |
         ($V | allocAssoc($k; $environ; $mem))
      )) as {$environ, $mem} |
      ($l[2]) as $body |
      $body | eval($environ; if (kw("letrec")) then $environ|recursiveEnv($mem) else $mem end)

    elif (kw("if") and ($l|length == 4)) then
      ($l[1:]) as [$condE, $thenE, $elseE] |
      ($condE | eval(environ; $mem)) as {$mem, $V} |
      (if ($V == {B: false}) then $elseE else $thenE end) |
      eval(environ; $mem)

    else # apply
      ($l[0] | eval(environ; $mem)) as {$mem, $V} | $V as $f |
      (reduce $l[1:][] as $argE (
        {$mem, args: []};
        (.args) as $args | (.mem) as $mem |
        $argE | eval(environ; $mem) | {mem, args: ($args + [.V])}
      )) as {$mem, $args} |
      $args | apply($f; $mem)
    end

  elif (isQQ and (.QQ|isCons)) then .QQ | {car,cdr} | reduce to_entries[] as {$key, $value} (
      {$mem, V: {}}; (.mem) as $mem |
      ($value | if (isUQ) then .UQ else {QQ: .} end | eval(environ; $mem)) as {$mem, $V} |
      .V[$key] = $V | .mem = $mem)
  elif (isQQ) then {$mem, V: .QQ }
  elif (isUQ) then error("unqoute: not in quasiquote: [\(show)]")

  else "SYNTAX ERROR: \(.)" | error end
; # END of `eval`!

# out: {$environ, $mem} # where $environ is a Map[Str->Loc], $mem is an array
def initEnv: ["+", "-", "*", "/", ">=", ">", "<=", "<", "=",
	      "equal?", "empty?", "car", "cons", "cdr", "list"] |
  reduce .[] as $name (
    {environ: {}, mem: []}; (.environ) as $environ | (.mem) as $mem |
    {builtIn: $name} | allocAssoc($name; $environ; $mem));

def evalAllGlobal($environ; $mem):
  def evalDefine($environ; $mem; $sym):  # OUT: {$environ, $mem, $V}
    eval($environ; ($environ|recursiveEnv($mem))) as {$mem, $V} | $V |
    allocAssoc($sym; $environ; $mem) + {$V};
  reduce .[] as $sxpr ({$environ, $mem};
    (.) as {$environ, $mem} |
    ($sxpr) as {car: {SYM: $define},
                cdr: {car: {SYM: $sym}, cdr: {car: $vsxpr}}} | # FIXME: assuming single expressions
    ($sxpr) as {car: {SYM: $fdefine},
                cdr: {car: {car: {SYM: $fsym}, cdr: {car: $formal, cdr: $formals}},
                      cdr: {car: $bodySxpr}}} |
    if (($define == "define") and ($sym|type == "string") and ($vsxpr != null)) then
      $vsxpr | evalDefine($environ; $mem; $sym)
    elif (($fdefine == "define") and ($fsym|type=="string") and
          ($formal|isSym) and ($formals|isConsL) and ($bodySxpr != null)) then
      [{SYM: "lambda"}, {car:$formal, cdr:$formals}, $bodySxpr] | arr2ConsL |
      evalDefine($environ; $mem; $fsym)
    elif ([$define, $fdefine] | any(. == "define")) then
      "SYNTAX ERROR: (define x value) or (define (f arg1..) body)\nat \($sxpr|show)" | error
    else
      $sxpr | evalDefine($environ; $mem; "~it")
    end
  );

def readEvalAllGlobal: sxprsP |
  if (length >= 1) then .[].a else "parse error (somewhere, sorry)" | error end |
  (initEnv) as {$environ, $mem} | evalAllGlobal($environ; $mem);

def readEvalAll: readEvalAllGlobal | .V | show;
