import { byLine, readInputFileAsText, sum } from "../helpers";
import { flow, pipe, constant } from "fp-ts/lib/function";
import * as RA from "fp-ts/ReadonlyArray";
import * as RNA from "fp-ts/ReadonlyNonEmptyArray";
import * as S from "fp-ts/string";
import * as O from "fp-ts/Option";
import * as P from "fp-ts/Predicate";

interface CardDetails {
  card: O.Option<number>;
  winningNumbers: ReadonlyArray<number>;
  cardNumbers: ReadonlyArray<number>;
}

const getCards = byLine;
const getCardDetails = (cardLine: string): CardDetails => {
  const [cardIdString, numberList] = pipe(cardLine, S.split(":"));
  const [winningNumbers, cardNumbers] = pipe(numberList, S.split("|"));

  const getNumbers = flow(
    S.split(" "),
    RA.map(Number),
    RA.filter((n) => n > 0),
  );
  const getCardId = flow(S.trim, S.split(" "), RA.head, O.map(Number));

  return {
    card: pipe(cardIdString, getCardId),
    winningNumbers: pipe(winningNumbers, getNumbers),
    cardNumbers: pipe(cardNumbers, getNumbers),
  };
};

const checkCardWinners = (cardDetails: CardDetails): ReadonlyArray<number> => {
  const { winningNumbers, cardNumbers } = cardDetails;
  return pipe(
    winningNumbers,
    RA.filter((n) => cardNumbers.includes(n)),
  );
};

const calculateCardPoints = (winningNumbers: ReadonlyArray<number>): number =>
  pipe(
    winningNumbers,
    RA.reduceWithIndex(winningNumbers.length > 0 ? 1 : 0, (i, acc) =>
      i === 0 ? acc : acc * 2,
    ),
  );

const run = async (): Promise<void> => {
  const input = await readInputFileAsText(import.meta.dir + "/input.txt");
  const detailsToPoints = flow(
    getCardDetails,
    checkCardWinners,
    calculateCardPoints,
  );
  const sumPoints = flow(RNA.map(detailsToPoints), RNA.reduce(0, sum));

  const output = pipe(
    input,
    getCards,
    RNA.filter(P.not(S.isEmpty)),
    O.match(constant(0), sumPoints),
  );

  console.log("Total: ", output);
};

run();
