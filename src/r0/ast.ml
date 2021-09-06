type 'a r0_open =
  [ `EInt of int
  | `ERead
  | `ENegate of 'a
  | `EAdd of 'a * 'a
  ]

type r0 = r0 r0_open
