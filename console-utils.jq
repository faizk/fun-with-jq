
def escape_char: "\u001b";
def ansi_color_codes: { # https://en.wikipedia.org/wiki/ANSI_escape_code#3-bit_and_4-bit
  "RED": { "FG": 31, "BG": 41 },
  "GREEN": { "FG": 32, "BG": 42 },
  "YELLOW": { "FG": 33, "BG": 43 },
  "BLUE": { "FG": 34, "BG": 44 },
  "MAGENTA": { "FG": 35, "BG": 45 },
  "CYAN": { "FG": 36, "BG": 46 },
  "WHITE": { "FG": 37, "BG": 47 },
  "BRIGHT_BLACK": { "FG": 90, "BG": 100 }, # they meant gray
  "BRIGHT_RED": { "FG": 91, "BG": 101 },
  "BRIGHT_GREEN": { "FG": 92, "BG": 102 },
  "BRIGHT_YELLOW": { "FG": 93, "BG": 103 },
  "BRIGHT_BLUE": { "FG": 94, "BG": 104 },
  "BRIGHT_MAGENTA": { "FG": 95, "BG": 105 },
  "BRIGHT_CYAN": { "FG": 96, "BG": 106 },
  "BRIGHT_WHITE": { "FG": 97, "BG": 107 }
};
def ansi_tty_fmts: {
  REGULAR:       0,
  BOLD:          1,
  LOW_INTENSITY: 2,
  ITALIC:        3,
  UNDERLINE:     4,
  BLINKING:      5,
  REVERSE:       6,
  BACKGROUND:    7,
  INVISIBLE:     8
};
def ansiFmt(p):
  ([(ansi_tty_fmts + ansi_color_codes) | p] |
   map(select(. != null) | tostring) |
   join(";")
  ) as $attrs |
  escape_char + "[" + $attrs + "m" + @text + escape_char + "[0m";

def sansCtrl:
  if (type == "string") then
    gsub(escape_char + "\\[([0-9]{1,3}(;[0-9]{1,2})*)?[mGK]"; "")
  else . end;
