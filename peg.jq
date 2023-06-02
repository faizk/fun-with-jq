# Trying: https://github.com/jqlang/jq/wiki/Parsing-Expression-Grammars

def star(E): (E | star(E)) // .;
def plus(E): E | (plus(E) // . );
def optional(E): E // .;
def amp(E): . as $in | E | $in;
def neg(E): select( [E] == [] );

# Consume a regular expression rooted at the start of .remainder, or emit empty;
# on success, update .remainder and set .match but do NOT update .result
def consume($re):
  # on failure, match yields empty
  (.remainder | match("^" + $re)) as $match
  | .remainder |= .[$match.length :]
  | .match = $match.string ;

def parse($re; f): consume($re) | .result = (.result + [.match|f]);
def parse($re): parse($re; .);

def parseNumber($re): parse($re; tonumber);

# end of string
def eos: select((.remainder|length == 0));

def literal($s):
  select(.remainder | startswith($s))
  | .result += [$s]
  | .remainder |= .[$s | length :];

def nonempty: select( (.remainder | length) > 0 );

def pMap(E; f): ((.result = null) | E) as $e |
  .remainder = $e.remainder |
  .result |= (. + [($e.result | f)]);

