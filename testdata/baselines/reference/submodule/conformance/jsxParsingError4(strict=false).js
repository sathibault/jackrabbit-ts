//// [tests/cases/conformance/jsx/jsxParsingError4.tsx] ////

//// [a.tsx]
declare const React: any
declare namespace JSX {
    interface IntrinsicElements {
        [k: string]: any
    }
}

const a = (
  <public-foo></public-foo>
);

const b = (
  <public></public>
);


//// [a.js]
const a = (<public-foo></public-foo>);
const b = (<public></public>);
