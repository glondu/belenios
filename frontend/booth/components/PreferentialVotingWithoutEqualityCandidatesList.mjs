import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";
import { DragDropContext } from "react-beautiful-dnd";

import CandidateWithCheckbox from "./CandidateWithCheckbox.mjs";
import PreferentialVotingWithoutEqualityColumn from "./PreferentialVotingWithoutEqualityColumn.mjs";

const buildColumnLabel = (column, columnOrderIndex, t) => {
  return column && column.title
    ? column.title
    : t("preferential_voting_preference_level", {
        level: columnOrderIndex + 1,
      });
};

class PreferentialVotingWithoutEqualityApp extends React.Component {
  constructor(props) {
    super(props);
    const { initialData } = props;
    this.state = {
      ...initialData,
      createdColumnsCounter: initialData.columnOrder.length,
    };
    this.onDragEnd = this.onDragEnd.bind(this);
    this.moveCandidate = this.moveCandidate.bind(this);
    this.render = this.render.bind(this);
    this.buildUserVoteForAllCandidatesInQuestion =
      this.buildUserVoteForAllCandidatesInQuestion.bind(this);
    this.saveUserVoteForAllCandidatesInQuestion =
      this.saveUserVoteForAllCandidatesInQuestion.bind(this);
  }
  buildUserVoteForAllCandidatesInQuestion() {
    let updatedVoteForCandidates = [];
    this.state.columnOrder.forEach((currentColumnId, currentColumnIndex) => {
      this.state.columns[currentColumnId].candidatesIds.forEach(
        (currentCandidateId, currentCandidateIndex) => {
          let candidate = this.state.candidates[currentCandidateId];
          if (!candidate) {
            alert("could not find candidate");
          } else {
            updatedVoteForCandidates[candidate.initialIndex] =
              currentColumnId === "not-ranked"
                ? undefined
                : currentCandidateIndex;
          }
        },
      );
    });
    return updatedVoteForCandidates;
  }
  saveUserVoteForAllCandidatesInQuestion() {
    const userVote = this.buildUserVoteForAllCandidatesInQuestion();
    this.props.dispatchUpdateUserVoteForQuestion({
      type: "saveVoteForAllCandidatesInQuestion",
      user_vote_for_all_candidates_in_question: userVote,
    });
  }
  moveCandidate(
    candidateId,
    sourceColumnId,
    destinationColumnId,
    sourceColumnCandidateIndex,
    destinationColumnCandidateIndex,
  ) {
    const sourceColumn = this.state.columns[sourceColumnId];
    const newSourceColumnCandidateIds = Array.from(sourceColumn.candidatesIds);
    newSourceColumnCandidateIds.splice(sourceColumnCandidateIndex, 1);
    if (sourceColumnId === destinationColumnId) {
      newSourceColumnCandidateIds.splice(
        destinationColumnCandidateIndex,
        0,
        candidateId,
      );
    }
    const newSourceColumn = {
      ...sourceColumn,
      candidatesIds: newSourceColumnCandidateIds,
    };
    let changedColumns = {};
    changedColumns[newSourceColumn.id] = newSourceColumn;
    if (sourceColumnId != destinationColumnId) {
      const destinationColumn = this.state.columns[destinationColumnId];
      const newDestinationColumnCandidateIds = Array.from(
        destinationColumn.candidatesIds,
      );
      newDestinationColumnCandidateIds.splice(
        destinationColumnCandidateIndex,
        0,
        candidateId,
      );
      const newDestinationColumn = {
        ...destinationColumn,
        candidatesIds: newDestinationColumnCandidateIds,
      };
      changedColumns[newDestinationColumn.id] = newDestinationColumn;
    }

    const newState = {
      ...this.state,
      columns: {
        ...this.state.columns,
        ...changedColumns,
      },
    };
    this.setState(newState, this.saveUserVoteForAllCandidatesInQuestion);

    // replicate the local state change in the more global state
    const destinationColumnIndex = this.state.columnOrder.findIndex(
      (element) => element == destinationColumnId,
    );
    if (candidateId in this.state.candidates && destinationColumnIndex > -1) {
      const candidateIndex = this.state.candidates[candidateId].initialIndex;
      this.props.dispatchUpdateUserVoteForQuestion({
        type: "saveVoteForCandidateInQuestion",
        candidate_index: candidateIndex,
        user_vote_for_candidate: destinationColumnIndex,
      });
    } else {
      alert("candidate index not found or destination column index not found");
    }
  }
  onDragEnd(result) {
    const { destination, source, draggableId } = result;
    if (!destination) {
      return;
    }
    if (
      destination.droppableId === source.droppableId &&
      destination.index === source.index
    ) {
      return;
    }
    this.moveCandidate(
      draggableId,
      source.droppableId,
      destination.droppableId,
      source.index,
      destination.index,
    );
  }
  render() {
    const allColumns = this.state.columnOrder.map((columnId, index) => {
      return {
        id: columnId,
        label: buildColumnLabel(
          this.state.columns[columnId],
          index,
          this.props.t,
        ),
      };
    });
    const children = this.state.columnOrder.map((columnId, index) => {
      if (!this.state.columns.hasOwnProperty(columnId)) {
        console.log(
          `/!\\ Column ${columnId} is present at index ${index} in this.state.columnOrder, but is absent from this.state.columns`,
        );
        return e("div");
      }
      const column = this.state.columns[columnId];
      const candidates = column.candidatesIds.map(
        (candidateId) => this.state.candidates[candidateId],
      );
      const otherColumns = Array.from(allColumns);
      otherColumns.splice(index, 1);
      return e(PreferentialVotingWithoutEqualityColumn, {
        key: column.id,
        column: column,
        candidates: candidates,
        label: buildColumnLabel(column, index, this.props.t),
        otherColumns: otherColumns,
        columns: { ...this.state.columns },
        allCandidates: { ...this.state.candidates },
        onSelectCandidateDestinationColumn: (
          candidateId,
          sourceColumnCandidateIndex,
          destinationColumnId,
          destinationColumnCandidateIndex,
        ) => {
          if (
            destinationColumnCandidateIndex === undefined ||
            destinationColumnCandidateIndex === null
          ) {
            destinationColumnCandidateIndex =
              this.state.columns[destinationColumnId].candidatesIds.length;
          }
          this.moveCandidate(
            candidateId,
            column.id,
            destinationColumnId,
            sourceColumnCandidateIndex,
            destinationColumnCandidateIndex,
          );
        },
        disabled: this.props.disabled,
      });
    });
    return e(
      "div",
      {
        className: "preferential-voting-ui",
      },
      e(
        DragDropContext,
        {
          onDragEnd: this.onDragEnd,
        },
        ...children,
      ),
    );
  }
}

PreferentialVotingWithoutEqualityApp.defaultProps = {
  initialData: {
    candidates: {
      "candidate-0": {
        id: "candidate-0",
        initialIndex: 0,
        content: "Charge my phone",
      },
      "candidate-1": {
        id: "candidate-1",
        initialIndex: 1,
        content: "Cook dinner",
      },
      "candidate-2": {
        id: "candidate-2",
        initialIndex: 2,
        content: "Go to the pub",
      },
    },
    columns: {
      ranked: {
        id: "ranked",
        candidatesIds: [],
      },
      "not-ranked": {
        id: "not-ranked",
        title: "Non classé",
        candidatesIds: ["candidate-0", "candidate-1", "candidate-2"],
      },
    },
    columnOrder: ["ranked", "not-ranked"],
  },
  disabled: false,
  t: function (s) {
    return s;
  },
  currentCandidatesHavingAlertsForQuestion: [],
  dispatchUpdateUserVoteForQuestion: () => {},
};

function TranslatablePreferentialVotingWithoutEqualityBigCandidatesList({
  identifierPrefix,
  candidates,
  blankVoteIsAllowed,
  renderedBlankVoteComponent,
  currentUserVoteForQuestion,
  currentCandidatesHavingAlertsForQuestion,
  dispatchUpdateUserVoteForQuestion,
  t,
}) {
  /*
  TODO:
  - show alerts using currentCandidatesHavingAlertsForQuestion
  - optional improvement to implement a smart back button: use currentUserVoteForQuestion to build initialData
  */
  const userHasSelectedBlankVote =
    blankVoteIsAllowed &&
    currentUserVoteForQuestion.length > candidates.length &&
    currentUserVoteForQuestion[candidates.length] === 1
      ? true
      : false;
  let initialData = {};
  const candidatesForInitialData = candidates.map(
    (candidateLabel, candidateIndex) => {
      return {
        id: `${identifierPrefix}_candidate_${candidateIndex}`,
        content: candidateLabel,
        initialIndex: candidateIndex,
      };
    },
  );
  initialData.candidates = candidatesForInitialData.reduce(
    (accumulator, currentValue) => {
      accumulator[currentValue.id] = currentValue;
      return accumulator;
    },
    {},
  );
  initialData.columns = {
    ranked: {
      id: "ranked",
      title: t("preferential_voting_ranked"),
      candidatesIds: [],
    },
    "not-ranked": {
      id: "not-ranked",
      title: t("preferential_voting_not_ranked"),
      candidatesIds: candidatesForInitialData.map((candidate) => candidate.id),
    },
  };
  // Facilitate reordering of the columns
  initialData.columnOrder = ["ranked", "not-ranked"];

  let additionalComponents = [];
  if (blankVoteIsAllowed && renderedBlankVoteComponent) {
    additionalComponents.push(renderedBlankVoteComponent);
  }

  return e(
    "div",
    {
      className: "preferential-voting-ui-container",
    },
    e(PreferentialVotingWithoutEqualityApp, {
      initialData,
      dispatchUpdateUserVoteForQuestion,
      disabled: userHasSelectedBlankVote,
      t,
    }),
    ...additionalComponents,
  );
}

function TranslatablePreferentialVotingWithoutEqualityCandidatesList({
  identifierPrefix,
  candidates,
  blankVoteIsAllowed,
  currentUserVoteForQuestion,
  currentCandidatesHavingAlertsForQuestion,
  dispatchUpdateUserVoteForQuestion,
  t,
}) {
  let renderedBlankVoteComponent = null;
  const candidateIndex = candidates.length;
  const userHasSelectedBlankVote =
    blankVoteIsAllowed &&
    currentUserVoteForQuestion.length > candidates.length &&
    currentUserVoteForQuestion[candidateIndex] === 1
      ? true
      : false;
  if (blankVoteIsAllowed) {
    const blankVoteLabel = t("blank_vote");
    const identifier = `${identifierPrefix}_blank-vote`;
    const currentAlerts =
      currentCandidatesHavingAlertsForQuestion &&
      currentCandidatesHavingAlertsForQuestion.includes(candidateIndex);
    const dispatchBlankVoteInQuestion = (blankVoteIsChecked) => {
      dispatchUpdateUserVoteForQuestion({
        type: "saveBlankVoteInQuestion",
        blankVoteIsChecked,
      });
    };
    const commonProps = {
      candidateInfo: blankVoteLabel,
      checked: userHasSelectedBlankVote,
      id: identifier,
      key: candidateIndex,
      dispatchUpdateUserVoteForCandidateInQuestion: dispatchBlankVoteInQuestion,
      currentAlertsForCandidateInQuestion: currentAlerts,
      name: identifier,
    };
    const blankVoteProps = {
      style: {
        margin: "50px auto 30px",
        maxWidth: "400px",
      },
    };
    renderedBlankVoteComponent = e(CandidateWithCheckbox, {
      ...commonProps,
      ...blankVoteProps,
    });
  }
  let cssClasses = "preferential-voting-candidates-list noselect"; // TODO: ? without-equality-
  if (userHasSelectedBlankVote) {
    cssClasses +=
      " preferential-voting-candidates-list--blank-vote-is-selected";
  }
  return e(
    "div",
    {
      className: cssClasses,
    },
    e(TranslatablePreferentialVotingWithoutEqualityBigCandidatesList, {
      identifierPrefix,
      candidates,
      blankVoteIsAllowed,
      renderedBlankVoteComponent,
      currentUserVoteForQuestion,
      currentCandidatesHavingAlertsForQuestion,
      dispatchUpdateUserVoteForQuestion,
      t,
    }),
  );
}

TranslatablePreferentialVotingWithoutEqualityCandidatesList.defaultProps = {
  identifierPrefix: "question_1",
  candidates: ["Candidate 1", "Candidate 2", "Candidate 3"],
  blankVoteIsAllowed: false,
  t: function (s) {
    return s;
  },
  currentCandidatesHavingAlertsForQuestion: [],
  dispatchUpdateUserVoteForQuestion: () => {},
};

const PreferentialVotingWithoutEqualityCandidatesList = withTranslation()(
  TranslatablePreferentialVotingWithoutEqualityCandidatesList,
);

export {
  PreferentialVotingWithoutEqualityCandidatesList,
  TranslatablePreferentialVotingWithoutEqualityCandidatesList,
  buildColumnLabel,
};
export default PreferentialVotingWithoutEqualityCandidatesList;
