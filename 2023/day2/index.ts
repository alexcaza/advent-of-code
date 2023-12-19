const input = Bun.file('input.txt');
const asText = await input.text();
const lines: Array<Line> = asText.split('\n');

type Line = string;
type Game = string;
type GameId = string;
type BlocksByColor = {red: number, blue: number, green: number};
type Pulls = Array<BlocksByColor>;

const sum = (a: number, b: number): number => a + b;
const sumBlocks = (blocks: BlocksByColor) => Object.values(blocks).reduce(sum);
const bustsMax = (blocks: BlocksByColor): Boolean => {
    const max = {
        red: 12,
        green: 13,
        blue: 14
    };

    const totalBlocksPossible = sumBlocks(max);
    const totalBlocksInPull = sumBlocks(blocks);

    if (totalBlocksInPull > totalBlocksPossible) {
        return true;
    }

    for (const [color, num] of Object.entries(blocks) as [keyof BlocksByColor, number][]) {
        if (num > max[color]) {
            return true;
        }
    }

    return false;
}
const getGame = (line: Line): Game => line.split(":")[0];
const getGameId = (game: Game): GameId => game.split(" ")[1];
const parseBlocks = (pull: string): BlocksByColor => {
    return pull.split(',').reduce((acc, group) => {
        const [num, color] = group.trim().split(' ');
        acc[color as 'red' | 'blue' | 'green'] += Number(num);
        return acc;
    }, {red: 0, blue: 0, green: 0});
}
const getAllPulls = (line: Line): Pulls => line.split(":")[1]?.split(";")?.map(parseBlocks);

let possibleGames = [];
for (const line of lines) {

    if (!line) {
        continue;
    }

    const gameId = getGameId(getGame(line));
    const pulls = getAllPulls(line);

    if (pulls.map(bustsMax).some(input => !!input)) {
        continue;
    }

    possibleGames.push(Number(gameId));
}

console.log("Game ids: ", possibleGames);
console.log("Sum: ", possibleGames.reduce(sum));
