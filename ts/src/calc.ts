type Result =
  { "input": string
  , "lexemes": [Lexeme]
  , "parsed": Syn
  , "result": Syn
  }

type Lexeme = string
export type Syn =
  { "type": "bin-op", "value": { "op":string, "lhs": Syn, "rhs": Syn }}
  | { "type": "neg", "value": Syn}
  | { "type": "num", "value": string}

export function runCalc (input: string): [Result] | null {
  const f = (window as any).runCalcHs as (undefined | ((input: string) => [Result]));
  return f === undefined ? null : f(input)
}
