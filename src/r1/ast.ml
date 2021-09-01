
type 'a r1_open = [
  | 'a R0.Ast.r0_open
  | `EMult of 'a * 'a
]

type r1 = r1 r1_open

