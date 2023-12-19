import { readInputFileAsText, sum } from "../helpers";

const input = await readInputFileAsText(import.meta.dir + '/input.txt');

const lines = input.split("\n");
const LOOKUP_RANGE = [1, 0, -1];
const symbolOnly = /([^\dA-Za-z.])/;

const grabNumber = (lineIndex: number, linePosition: number): number => {
  const currentLine = lines[lineIndex];

  let startingNumberIndex = linePosition;

  // Lookup backwards to find first int
  while (!isNaN(Number(currentLine[startingNumberIndex - 1]))) {
    startingNumberIndex--;
  }

  let number = "";

  // Build number starting from first int
  while (!isNaN(Number(currentLine[startingNumberIndex]))) {
    number += currentLine[startingNumberIndex];
    startingNumberIndex++;
  }

  return Number(number);
};

// Searches a 3x3 grid around the symbol
// for numbers adjacent. Excludes duplicates.
const matrixLookup = (
  currentLinePosition: number,
  currentLineIndex: number,
): Array<number> => {
  let numbers: Array<number> = [];
  for (let y = 0; y < LOOKUP_RANGE.length; y++) {
    const lineIndex = currentLineIndex - LOOKUP_RANGE[y];
    const currentLine = lines[lineIndex];
    for (let x = 0; x < LOOKUP_RANGE.length; x++) {
      const linePosition = currentLinePosition - LOOKUP_RANGE[x];
      const char = currentLine[linePosition];
      if (!isNaN(Number(char))) {
        const number = grabNumber(lineIndex, linePosition);
        if (number && !numbers.includes(number)) {
          numbers.push(number);
        }
      }
    }
  }
  return numbers.flat();
};

const getPartNumbers = (): Array<number> => {
  let numbers: Array<Array<number>> = [];
  for (let li = 0; li < lines.length; li++) {
    const currentLine = lines[li];

    for (let lp = 0; lp < currentLine.length; lp++) {
      const char = currentLine[lp];
      if (symbolOnly.test(char)) {
        // do matrix lookup
        numbers.push(matrixLookup(lp, li));
      }
    }
  }
  return numbers.flat();
};

const getP1Answer = (partNumbers: Array<number>): number =>
  partNumbers.reduce(sum);

console.log(getP1Answer(getPartNumbers()));
