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
--majority-judgement-color-0: #b02800; / * Reject / À rejeter / hsl(14, 100%, 35%) * /
--majority-judgement-color-1: #ff6503; / * Poor / insuffisant / hsl(23, 100%, 51%) * /
--majority-judgement-color-2: #ff9f00; / * Acceptable / Passable / hsl(37, 100%, 50%) * /
--majority-judgement-color-3: #f5c823; / * Fair / Assez bien / hsl(47, 91%, 55%) * /
--majority-judgement-color-4: #7dd162; / * Good / Bien / hsl(105, 55%, 60%) * /
--majority-judgement-color-5: #2eb430; / * Very good / Très bien / hsl(121, 59%, 44%) * /
--majority-judgement-color-6: #0f7c10; / * Excellent / Excellent / hsl(121, 78%, 27%) * /
*/
const majorityJudgementGradesHslColorScale = [[14, 100, 35], [23, 100, 51], [37, 100, 50], [47, 91, 55], [105, 55, 60], [121, 59, 44], [121, 78, 27]]; // from red to yellow to green

const majorityJudgementGradeIndexToCssColor = gradeIndexToCssColor.bind(null, majorityJudgementGradesHslColorScale);

export { gradeIndexToCssColor, majorityJudgementGradesHslColorScale, majorityJudgementGradeIndexToCssColor };
export default majorityJudgementGradeIndexToCssColor;
