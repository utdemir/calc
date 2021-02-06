import React from 'react';
import { useState, useEffect } from 'react';
import './App.css';
import { Input, Container, Center, Heading } from "@chakra-ui/react"
import { runCalc, Syn } from './calc';

function printSyn(s: Syn): string {
  switch (s.type) {
    case "bin-op":
      return printSyn(s.value.lhs) + " " + s.value.op + printSyn(s.value.rhs)
    case "neg":
      return "-" + printSyn(s.value)
    case "num":
      return s.value
  }
}

function App() {
  const [timer, setTimer] = useState(0);
  const [input, setInput] = useState("");

  const results = runCalc(input);

  let ret;
  if(results == null) {
    ret = "loading calc..."
    setInterval(() => setTimer(timer + 1), 100)
  } else {
    if(results.length > 0) {
      const preferred = results[0];
      ret = printSyn(preferred.result)
    } else {
      ret = "no parse."
    }
  }

  return (
    <Container>
      <Center>
        <Heading>calc</Heading>
      </Center>
      <Input size="lg" onChange={(i) => setInput(i.target.value)} /> <br/>
      <pre>
      { ret }
      </pre>
    </Container>
  );
}

export default App
