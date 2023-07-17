const lerp = (value1, value2, amount) => {
  amount = amount < 0 ? 0 : amount;
  amount = amount > 1 ? 1 : amount;
  return value1 + (value2 - value1) * amount;
};

// @param dataPoints: An array of numbers, or an array where each element is an array of numbers of the same length
// @return: If dataPoints is an array of numbers: a number. Else: an array of numbers.
const lerpInArray = (dataPoints, ratio) => {
  const referenceMaxIndex = dataPoints.length - 1;
  if (referenceMaxIndex < 1) {
    return undefined;
  }
  if (ratio <= 0) {
    return dataPoints[0];
  }
  if (ratio >= 1) {
    return dataPoints[referenceMaxIndex];
  }
  const nearestLowIndex = Math.floor(ratio * referenceMaxIndex);
  const nearestHighIndex = nearestLowIndex + 1;
  const insideRatio = ratio * referenceMaxIndex - nearestLowIndex; // insideRatio should be inside [0.0;1.0[
  if (Array.isArray(dataPoints[nearestLowIndex])) {
    return dataPoints[nearestLowIndex].map((value, index) => {
      return lerp(value, dataPoints[nearestHighIndex][index], insideRatio);
    });
  } else {
    return lerp(
      dataPoints[nearestLowIndex],
      dataPoints[nearestHighIndex],
      insideRatio,
    );
  }
};

const hslToCssColor = (hue, saturation, lightness) => {
  return `hsl(${hue}, ${saturation}%, ${lightness}%)`;
};

export { lerp, lerpInArray, hslToCssColor };
export default hslToCssColor;
