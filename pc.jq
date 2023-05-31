def happy($a): [{$a, rest: .}];

def chars($cs):
  if (startswith($cs)) then [{a: $cs, rest: .[($cs|length):]}]
  else [] end;

def filter(cond; p): p | map(select(.a | cond));
def except(cond; p): filter(cond | not; p);
def filterP(condP; p): filter((condP | length >= 1); p);
def exceptP(condP; p): except((condP | length >= 1); p);

def anyChar:
  if (length >= 1) then [{a: .[0:1], rest: .[1:]}] else [] end;

def orElse(lp; rp):
  (lp) as $l |
  (if ($l | length >= 1) then $l else rp end);
# pass in multiple parsers with `,` (not `;`): anyOf(p1, p2, p3, ..)
def anyOf(p): [first(p | select(length >= 1))[]];

def oneOrMore(p):
  p |
  map(
    (.a) as $a |
    .rest | orElse(oneOrMore(p); happy([])) |
    map(.a |= ([$a] + .)) |
    .[]
  );
def zeroOrMore(p): orElse(oneOrMore(p); happy([]));

def fmap(p; f): p | map(.a |= f);

def foll(pl; pr):
  pl | map((.a) as $l | .rest | fmap(pr; [$l, .]) | .[]);

def foll(pl; pr; f): fmap(foll(pl; pr); f);
def follR(pl; pr): foll(pl; pr; .[1]);
def follL(pl; pr): foll(pl; pr; .[0]);

def surr(pa; pw):     follR(pw; follL(pa; pw));
def surr(pl; pa; pr): follR(pl; follL(pa; pr));

def delimited(p; sepP): foll(
  p;
  zeroOrMore(follR(sepP; p));
  .[0:1] + .[1]
);

def digit:
  ([range(0;10) | {key: "\(.)", value: .}] | from_entries) as $ds |
  filter(. != null; fmap(anyChar; $ds[.]));

def ws:    orElse(chars(" "); chars("\n"));
def ws(p): surr(p; zeroOrMore(ws));
