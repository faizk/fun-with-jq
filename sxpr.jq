def isNIL:    . == null;
def isSym:    has("SYM");
def isLitNum: has("N");
def isBool:   has("B");
def isQ:      has("Q");
def isQQ:     has("QQ");
def isUQ:     has("UQ");
def isAtom:   isLitNum or isBool or isSym or isNIL;
def isCons:   has("car") and has("cdr") and (length == 2);

def arr2ConsL:
  if (length >= 1) then {car: .[0], cdr: (.[1:]|arr2ConsL)}
  else null end;

def consL2Arr:
  if (isCons) then [.car] + (.cdr | consL2Arr)
  elif (isNIL) then []
  else "not a list" | error end;

def isConsL: ((isCons) and (.cdr | isConsL)) or isNIL;

def show:
  if   (isNIL) then "()"
  elif (isConsL) then consL2Arr | map(show) | join (" ") | "(\(.))"
  elif (isCons) then "(\(.car|show) . \(.cdr|show))"
  elif (isLitNum) then .N | tostring
  elif (isSym) then .SYM
  elif (isBool) then if (.B) then "#t" else "#f" end
  elif (isQ) then "'\(.Q|show)"
  elif (isQQ) then "`\(.QQ|show)"
  elif (isUQ) then ",\(.UQ|show)"
  else . end;
