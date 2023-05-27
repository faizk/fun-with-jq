include "pc"; include "sxpr"; include "lisp2"; include "console-utils";

def snipP(p):
  def openPAndP: surr(openP; ws(delimited(p; oneOrMore(ws))); zeroOrMore(ws));
  def justOpenP: foll(openP; zeroOrMore(ws); []);
  fmap(orElse(openPAndP; justOpenP); {SNIP: .});

def snipOnP(p):
  fmap(ws(delimited(p; oneOrMore(ws))); {SNIP: .});

def endSnipP(p):
  foll(
    orElse(
      ws(delimited(p; oneOrMore(ws)));
      fmap(zeroOrMore(ws); []));
    ws(closeP);
    {SNIPEND: .[0]});

def maySnipP:
  orElse(ws(sxprP); snipP(sxprP));

def showV: show |
  if (type == "object" and has("lambda")) then
    (initEnv) as {$environ, $mem} |
    .lambda | (.fargs |= join(" ") |
       .environ |= (with_entries(select($environ[.key]==null))) |
       .body |= show) |
    "(λ (\(.fargs)) \(.body)) >>[env (sans built-ins): \(.environ)]<<"
  else ansiFmt(.GREEN.FG, .UNDERLINE) end;

def repl($acc; $environ; $mem; p):
  def evalPrint: [.] |
    try
      evalAllGlobal($environ; $mem) as {$environ, $mem, $V} |
      ($V | showV), repl([]; $environ; $mem; maySnipP)
    catch
      if (. != "break") then
        ("ERROR: \(.)\n" | ansiFmt(.RED.BG, .WHITE.FG)), repl([]; $environ; $mem; maySnipP)
      else "BYE!" | ansiFmt(.BOLD, .BLUE.FG) end;
  input | zeroOrMore(p) |
  if (length >= 1) then .[] |
    if (.rest | length >= 1) then
      ("[WARN] excess input: [\(.rest)]" | ansiFmt(.YELLOW.BG)), repl([]; $environ; $mem; p)
    elif (.a | length == 0) then repl([]; $environ; $mem; p)
    else .a[] |
      if (has("SNIP")) then
        (.SNIP) as $soFar |
        def nextP: orElse(endSnipP(sxprP); snipOnP(sxprP));
        repl($acc + $soFar; $environ; $mem; nextP)
      elif (has("SNIPEND")) then
        ($acc + .SNIPEND) |
        arr2ConsL | evalPrint
      else
        evalPrint
      end
    end
  else
    ("parse error" | ansiFmt(.REG.FG)), repl([]; $environ; $mem; p)
  end
;

def repl: (initEnv) as {$environ, $mem} | repl([]; $environ; $mem; maySnipP);
