import { flow } from "fp-ts/lib/function";
import * as S from 'fp-ts/string';

export const readInputFileAsText = async (fileName: string) =>
  await Bun.file(fileName).text();
export const sum = (a: number, b: number): number => a + b;
export const byLine = flow(S.split("\n"));
