import { lerpInArray, hslToCssColor } from "./color_utils.mjs";

const gradeIndexToCssColor = (gradesHslColorScale, availableGradesLength, gradeIndex) => {
  if (availableGradesLength < 2){
    return undefined;
  }

  const ratio = gradeIndex / (availableGradesLength-1);
  const hsl = lerpInArray(gradesHslColorScale, ratio);
  return hslToCssColor(hsl[0], hsl[1], hsl[2]);
};

/*
#b02800: Reject / À rejeter / hsl(14, 100%, 35%)
#ff6503: Poor / Insuffisant / hsl(23, 100%, 51%)
#ff9f00: Acceptable / Passable / hsl(37, 100%, 50%)
#f5c823: Fair / Assez bien / hsl(47, 91%, 55%)
#7dd162: Good / Bien / hsl(105, 55%, 60%)
#2eb430: Very good / Très bien / hsl(121, 59%, 44%)
#0f7c10: Excellent / Excellent / hsl(121, 78%, 27%)
*/
const majorityJudgmentGradesHslColorScale = [[121, 78, 27], [121, 59, 44], [105, 55, 60], [47, 91, 55], [37, 100, 50], [23, 100, 51], [14, 100, 35]]; // from green to yellow to red

const majorityJudgmentGradeIndexToCssColor = gradeIndexToCssColor.bind(null, majorityJudgmentGradesHslColorScale);

export { gradeIndexToCssColor, majorityJudgmentGradesHslColorScale, majorityJudgmentGradeIndexToCssColor };
export default majorityJudgmentGradeIndexToCssColor;
