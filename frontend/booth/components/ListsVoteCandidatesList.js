import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

import CandidateList from "./CandidateList.js";
import { NiceButton } from "./NiceButton.js";

function TranslatableListsVoteCandidatesList({
  lists,
  identifierPrefix,
  currentUserVoteForQuestion,
  dispatchUpdateUserVoteForQuestion,
  t,
}) {
  const isListSelected =
    currentUserVoteForQuestion.filter((listAnswer) => listAnswer[0] === 1)
      .length > 0;

  const renderedLists = lists.map((list, listIndex) => {
    const identifier = `${identifierPrefix}_choice_${listIndex}`;
    const dispatchUpdateUserVoteForCandidateInQuestion = (listValue) => {
      let updatedVote = currentUserVoteForQuestion;
      updatedVote[listIndex] = listValue;
      if (listValue[0] === 1) {
        // unselect other lists when one is selected
        updatedVote = updatedVote.map((otherList, otherListIndex) => {
          if (otherListIndex == listIndex) {
            return otherList;
          } else {
            return otherList.map((_) => 0);
          }
        });
      }
      dispatchUpdateUserVoteForQuestion({
        type: "saveVoteForAllCandidatesInQuestion",
        user_vote_for_all_candidates_in_question: updatedVote,
      });
    };
    const listElement = e(CandidateList, {
      listName: list[0],
      listCandidates: list.slice(1),
      value: currentUserVoteForQuestion[listIndex],
      id: identifier,
      identifierPrefix: identifier,
      key: listIndex,
      dispatchUpdateUserVoteForCandidateInQuestion,
      name: identifier,
    });

    if (!isListSelected || currentUserVoteForQuestion[listIndex][0] === 1) {
      return listElement;
    } else {
      return null;
    }
  });

  const clearButton = e(NiceButton, {
    label: "Select another list",
    onClick: () => {
      const updatedVote = currentUserVoteForQuestion.map((list, _) =>
        list.map((_) => 0),
      );
      dispatchUpdateUserVoteForQuestion({
        type: "saveVoteForAllCandidatesInQuestion",
        user_vote_for_all_candidates_in_question: updatedVote,
      });
    },
  });

  return e(
    "div",
    {
      className: "lists-vote-candidates-list noselect",
    },
    ...renderedLists,
    isListSelected ? clearButton : null,
  );
}

TranslatableListsVoteCandidatesList.defaultProps = {
  identifierPrefix: "question_1",
  lists: [
    ["List 1", "Candidate 1-1"],
    ["List 2", "Candidate 2-1", "Candidate 2-2"],
  ],
  currentUserVoteForQuestion: [
    [0, 0],
    [0, 0, 0],
  ],
  dispatchUpdateUserVoteForQuestion: () => {},
  t: (s) => {
    return s;
  },
};

const ListsVoteCandidatesList = withTranslation()(
  TranslatableListsVoteCandidatesList,
);

export { ListsVoteCandidatesList, TranslatableListsVoteCandidatesList };
export default ListsVoteCandidatesList;
