exports.makePuzzlesJSONImpl = (puzzles) => {
  return JSON.stringify(puzzles, null, 2);
};