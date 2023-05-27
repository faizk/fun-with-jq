include "pc"; include "sxpr"; include "lisp2"; include "console-utils";

def snipP(p):
  def openPAndP: surr(openP; ws(delimited(p; oneOrMore(ws))); zeroOrMore(ws));
  def justOpenP: foll(openP; zeroOrMore(ws); []);
  fmap(orElse(openPAndP; justOpenP); {SNIP: .});

def showV: show |
  if (type == "object" and has("lambda")) then
    (initEnv) as {$environ, $mem} |
    .lambda | (.fargs |= join(" ") |
       .environ |= (with_entries(select($environ[.key]==null))) |
       .body |= show) |
    "(λ (\(.fargs)) \(.body))" as $l |
    "\($l | ansiFmt(.UNDERLINE, .GREEN.FG)) >>[env (sans built-ins): \(.environ|ansiFmt(.BRIGHT_BLACK.FG))]<<"
  else ansiFmt(.GREEN.FG, .UNDERLINE) end;

def trimmedR($s): [. | until(endswith($s) | not; rtrimstr($s))]   | last;
def trimmedL($s): [. | until(startswith($s) | not; ltrimstr($s))] | last;
def trimmed($s): trimmedL($s) | trimmedR($s);

def getNonEmptyInput: input | trimmed(" ") | if (length >= 1) then . else getNonEmptyInput end;

def repl($environ; $mem):
  def evalPrint:
    try
      evalAllGlobal($environ; $mem) as {$environ, $mem, $V} |
      ($V | showV), repl($environ; $mem)
    catch
      if (. != "break") then
        ("ERROR: \(.)\n" | ansiFmt(.RED.BG, .WHITE.FG)), repl($environ; $mem)
      else "BYE!" | ansiFmt(.BOLD, .BLUE.FG) end
  ;
  def p: delimited(orElse(sxprP; snipP(sxprP)); oneOrMore(ws));
  def keepGoing($linesSoFar):
    $linesSoFar | join("\n") | p |
    if (length >= 1) then
      (.[].a | any(has("SNIP"))) as $stillCooking |
      if ($stillCooking) then
        ("-- MULTILINE --"|ansiFmt(.BLUE.BG, .WHITE.FG, .UNDERLINE)),
        keepGoing($linesSoFar + [getNonEmptyInput])
      else .[] | 
        if (.rest | length >= 1) then (.a) as $a |
          "[WARN] ignoring trailing garbage: >>>\(.rest)<<<" | debug | $a
        else .a end |
        evalPrint 
      end
    else
      error("parse error!")
    end
  ;
  getNonEmptyInput | keepGoing([.])
;

def repl: (initEnv) as {$environ, $mem} | repl($environ; $mem);
