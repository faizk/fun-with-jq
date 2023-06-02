include "sxpr";
include "lisp2"; include "console-utils";

def showV: show |
  if (type == "object" and has("lambda")) then
    def showBody: (20) as $max | show |
      if (length >= $max) then .[0:$max] | "\(.)..)" else . end;
    (initEnv) as {$environ, $mem} |
    .lambda | (.fargs   |= join(" ") |
               .environ |= (with_entries(select($environ[.key]==null))) |
               .body    |= showBody) |
    "(λ (\(.fargs)) \(.body))" as $l |
    "\($l | ansiFmt(.UNDERLINE, .GREEN.FG)) >>[env (sans built-ins): \(.environ|ansiFmt(.GREEN.BG, .WHITE.FG))]<<"
  else ansiFmt(.GREEN.FG, .UNDERLINE) end;

def trimmedR($s): [. | until(endswith($s) | not; rtrimstr($s))]   | last;
def trimmedL($s): [. | until(startswith($s) | not; ltrimstr($s))] | last;
def trimmed($s): trimmedL($s) | trimmedR($s);

def REPL(cfgF):
  { interactive: true,
  } as $defaults |
  def interactive: $defaults | cfgF | .interactive;
  def fmt(p): if (interactive) then ansiFmt(p) else . end;
  def msgLn($s): "\($s)\n";
  def prompt($s): if (interactive) then $s|fmt(.BLUE.FG, .BOLD) else empty end;
  def prompt: prompt("?> ");
  if (interactive) then msgLn(
    ";; Welcome! This is a rudimentary implementation of a Scheme interpreter"+
    " - in \( "jq"|fmt(.ITALIC, .GREEN.FG))!" | fmt(.REVERSE, .BOLD))
  else empty end,
  prompt,
  (initEnv) as {$environ, $mem} |
  label $out |
  foreach inputs as $line (
    {buffer: [], ready: [], $environ, $mem, problems: []}
    ;
    (.environ) as $environ | (.mem) as $mem |
    .ready = [] | .problems = [] |
    if ($line | trimmed(" ") | length >= 1) then
      .buffer |= (. + [$line]) | .ready = [] |
      (.buffer|join("\n") | sxprL) as $read |
      if ($read|length >= 1) then
        .buffer = [] | . +
        try
          (($read[].a | evalAllGlobal($environ; $mem)) as {$mem, $environ, $V} |
           {ready: (.ready + [$V]), $environ, $mem})
        catch if (. != "break") then {problems: [.]} else break $out end
      else . end
    else . end
    ;
    (if (.problems | length >= 1) then
       (.problems[] | ("[ERROR] \(.)" | fmt(.RED.BG, .WHITE.FG)), "\n")
     else empty end),
    (.ready[] | (showV | if (interactive) then . else sansCtrl end),
      "\n"),
    (if (.buffer| length == 0) then prompt else prompt(".. ") end)
  ),
  if (interactive) then "BYE!" | ansiFmt(.BLUE.FG, .BOLD) else empty end;

def REPL: REPL(.interactive = true);
