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
    "\($l | ansiFmt(.UNDERLINE, .GREEN.FG)) >>[env (sans built-ins): \(.environ|ansiFmt(.GREEN.BG, .WHITE.FG))]<<"
  else ansiFmt(.GREEN.FG, .UNDERLINE) end;

def trimmedR($s): [. | until(endswith($s) | not; rtrimstr($s))]   | last;
def trimmedL($s): [. | until(startswith($s) | not; ltrimstr($s))] | last;
def trimmed($s): trimmedL($s) | trimmedR($s);

def getNonEmptyInput: input | trimmed(" ") | if (length >= 1) then . else getNonEmptyInput end;

def repl($linesSoFar; $environ; $mem): ## FIXME: this is really buggy
  def evalPrint:
    evalAllGlobal($environ; $mem) as {$environ, $mem, $V} |
    ($V | showV), repl([]; $environ; $mem);
  def p: delimited(orElse(sxprP; snipP(sxprP)); oneOrMore(ws));
  def keepGoing:
    ($linesSoFar + [.]) as $linesSoFar |
    $linesSoFar | join("\n") | p |
    if (length >= 1) then
      (.[].a | any(has("SNIP"))) as $stillCooking |
      if ($stillCooking) then
        ("-- MULTILINE --"|ansiFmt(.BLUE.BG, .WHITE.FG, .UNDERLINE)),
        repl($linesSoFar; $environ; $mem)
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
  try (getNonEmptyInput | keepGoing)
  catch if (. == "break") then
          ("BYE!" | ansiFmt(.BOLD, .BLUE.FG))
        else
          ("ERROR: \(.)\n" | ansiFmt(.RED.BG, .WHITE.FG)), repl([]; $environ; $mem)
        end
;

def repl: (initEnv) as {$environ, $mem} | repl([]; $environ; $mem);

# a more rufimentary REPL that requires 2 blank lines for execution
# unlike an actual REPL, won't save state between executions
def multilineReadEvalAll:
  foreach inputs as $line (
    {emptyLines: 0, buffer:[], ready: []}
    ;
    if ($line | length >= 1) then
      .buffer |= (. + [$line]) | .emptyLines = 0
    else
      .emptyLines |= (. + 1) |
      if (.emptyLines >= 2) then .ready = .buffer | .buffer = [] else . end
    end
    ;
    if ((.emptyLines == 2) and (.ready | length >= 1)) then
      .ready | join("\n") |
      try readEvalAll
      catch if (. == "break") then "BYE" else stderr | multilineReadEvalAll end
    else empty end
  );

