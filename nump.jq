# in: [int]
# out: int
def digitsToInt:
  reverse | to_entries |
  map(.key |= exp10 | (.key * .value)) |
  add
;
