include "pc"; include "sxpr";

def builtin_add:    map(.N) | add | {N: .};
def builtin_sub:    map(.N) | (if (length >=2) then . else [0] + . end) |
                      reduce .[1:][] as $item (.[0]; . - $item);
def builtin_mul:    map(.N) | reduce .[] as $item (1; . * $item) | {N: .};
def builtin_div:    map(.N) | reduce .[] as $item (1; . / $item) | {N: .};
def builtin_car:    .[0].car;
def builtin_cdr:    .[0].cdr;
def builtin_cons:   {car: .[0], cdr: .[1]};
def builtin_lte:    map(.N) | (.[0] <= .[1]) | {B: .};
def builtin_lt:     map(.N) | (.[0] <  .[1]) | {B: .};
def builtin_gte:    map(.N) | (.[0] >= .[1]) | {B: .};
def builtin_gt:     map(.N) | (.[0] >  .[1]) | {B: .};
def builtin_eq:     map(.N) | (.[0] == .[1]) | {B: .};
def builtin_equal:  (.[0] == .[1]) | {B: .};
def builtin_empty:  (.[0] == null) | {B: .};
def builtin_list:   arr2ConsL;

# The `environ` is a map of `Str->Loc`, where `Loc` is just a number (an index in an array)
def lookup($environ): (.) as $sym | # Note that the "errors" thrown represent a bug in *this* code, not the users'
  if ($environ|has($sym)) then $environ[$sym] else "undefined var: \($sym)" | error end;
# `Loc`s are indexes in the `mem`, which is an array that we just pass around.
def fetch($mem): (.) as $loc | $mem[$loc] |
  if ((type == "object") and (has("V"))) then .V else "NPE @ \($loc)" | error end;
def nextLoc($mem): ($mem | length);
def alloc($mem): (.) as $V | (nextLoc($mem)) as $loc |
  ($mem | (.[$loc] = {$V})) as $mem | {$mem, $loc};

# in ENV; out: MEM - this is used to convert a `let` behaviour to `letrec`-like behaviour
def recursiveEnv($mem):
  (.) as $env |
  to_entries | map(
    (.key) as $k | (.value) as $loc |
    ($loc | fetch($mem)) as $V |
    select($V | has("lambda")) |
    ($V | .lambda.environ |= ($env)) as $updated |
    {$loc, $updated}
  ) |
  reduce .[] as {$loc, $updated} ($mem; . | (.[$loc] |= {V: $updated}))
;

def eval(environ; $mem):
  def apply($thing; $mem):
    if ($thing | has("lambda")) then
      ($thing.lambda) as $lambda |
      (reduce .[] as $arg (
          {$mem, locs: []};
          (.locs) as $locs | (.mem) as $mem |
          $arg | alloc($mem) | {mem, locs: ($locs + [.loc])}
        )) as {$mem, $locs} |
      ($locs | with_entries(.key |= ($lambda.fargs[.]))) as $argsEnv |
      $lambda.body | eval($lambda.environ + $argsEnv; $mem)
    elif ($thing | has("builtIn")) then
      ($thing.builtIn) as $f |
      if   ($f == "+")      then builtin_add
      elif ($f == "*")      then builtin_mul
      elif ($f == "-")      then builtin_sub
      elif ($f == "/")      then builtin_div
      elif ($f == "<=")     then builtin_lte
      elif ($f == "<")      then builtin_lt
      elif ($f == ">=")     then builtin_gte
      elif ($f == ">")      then builtin_gt
      elif ($f == "=")      then builtin_eq
      elif ($f == "equal?") then builtin_equal
      elif ($f == "empty?") then builtin_empty
      elif ($f == "car")    then builtin_car
      elif ($f == "cdr")    then builtin_cdr
      elif ($f == "cons")   then builtin_cons
      elif ($f == "list")   then builtin_list
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

  elif (isConsL) then (consL2Arr) as $l |

    if (({SYM: "lambda"} == $l[0]) and ($l|length == 3) and ($l[1]|isConsL)) then
      {lambda: {fargs: ($l[1]|consL2Arr|map(.SYM)),
                body: $l[2],
                environ: environ}
      } | {$mem, V: .}

    elif (({SYM: "let"} == $l[0]) and ($l|length >= 3) and ($l[1]|isConsL)) then
      (reduce ($l[1]|consL2Arr)[] as {car: {SYM: $k}, cdr: {car: $vexp}} (
         {environ: environ, $mem};
         (.environ) as $environ | (.mem) as $mem |
         ($vexp | eval($environ; $mem)) as {$mem, $V} |
         ($V | alloc($mem)) as {$mem, $loc} |
         {$environ, $mem} |
         .environ |= (. + {"\($k)": $loc})
      )) as {$environ, $mem} |
      ($l[2]) as $body |
      $body | eval($environ; $environ|recursiveEnv($mem))

    elif (({SYM: "if"} == $l[0]) and ($l|length == 4)) then
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

  else
    "SYNTAX ERROR: \(.)" | error
  end
; # END of `eval`!

# in: [Str]; out: {$environ, $mem} # where $environ is a Map[Str->Loc], $mem is an array
def initBuiltins: reduce .[] as $name (
  {environ: {}, mem: []};
  (.environ) as $environ | (.mem) as $mem |
  {builtIn: $name} | alloc($mem) as {$mem, $loc} |
  {$environ, $mem} | .environ |= (. + {"\($name)": $loc}));

def initEnv:
  (["+", "-", "*", "/", ">=", ">", "<=", "<", "=",
    "equal?", "empty?", "car", "cons", "cdr", "list"
   ] | initBuiltins);

def evalAllGlobal($environ; $mem):
  def evalDefine($environ; $mem; $sym): # OUT: {$environ, $mem, $V}
    eval($environ; $mem) as {$mem, $V} | $V | alloc($mem) as {$mem, $loc} |
    {$environ, $mem, $V} | .environ |= (. + {"\($sym)": $loc});
  reduce .[] as $sxpr ({$environ, $mem};
    (.) as {$environ, $mem} |
    ($sxpr) as {car: {SYM: $define},
                cdr: {car: {SYM: $sym}, cdr: {car: $vsxpr}}} | # FIXME: assuming single expressions
    ($sxpr) as {car: {SYM: $fdefine},
                cdr: {car: {car: {SYM: $fsym}, cdr: { car: $formal, cdr: $formals} },
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
      $sxpr | eval($environ; ($environ | recursiveEnv($mem))) as {$mem, $V} |
      {$environ, $mem, $V}
    end
  );

def readEvalAllGlobal: oneOrMore(ws(sxprP)) |
  if (length >= 1) then .[].a else "parse error (somewhere, sorry)" | error end |
  (initEnv) as {$environ, $mem} | evalAllGlobal($environ; $mem);

def readEvalAll: readEvalAllGlobal | .V | show;
