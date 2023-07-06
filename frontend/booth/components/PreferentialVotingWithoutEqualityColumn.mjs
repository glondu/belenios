import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";
import { Draggable, Droppable } from "react-beautiful-dnd";

const TranslatableMoveCandidateHandle = ({ t, tReady, ...props }) => {
  // the icon visual is made using CSS
  return e(
    "div",
    {
      className: "preferential-voting__candidate-handle draggable",
      title: t("preferential_voting_drag_candidate"),
      ...props,
    },
    e("div", {
      className: "preferential-voting__candidate-handle__icon",
    }),
  );
};

const MoveCandidateHandle = withTranslation()(TranslatableMoveCandidateHandle);

const SelectOption = ({ value, label }) => {
  return e(
    "option",
    {
      value: value,
    },
    label,
  );
};

const TranslatableCandidateWithoutEquality = ({
  candidate,
  index,
  column,
  otherColumns,
  columns,
  candidates,
  allCandidates,
  onSelectDestinationColumn,
  disabled,
  showRank,
  t,
}) => {
  let otherPreferencesSelectOptions = [];

  /*
  Build the list of possible places where the user can move this candidate to.
  This list will be displayed on the right of current candidate's name, in a "select".
  A candidate which is currently in the "Not ranked" section can be moved to:
  - The "Ranked" section (generic naming) if no candidate currently is in the "Ranked" section
  - Above any ranked candidate
  - Below last ranked candidate
  A candidate which is currently displayed in the "Ranked" section can be moved to:
  - The "Not ranked" section
  - Above any ranked candidate (except himself)
  - Below last ranked candidate (except if the last ranked candidate is himself)
  */

  // Detect whether there are already candidates in the "Ranked" section (as opposed to the "Not ranked" section)
  const rankedSectionIsNotEmpty =
    columns &&
    columns.hasOwnProperty("ranked") &&
    columns["ranked"].hasOwnProperty("candidatesIds") &&
    columns["ranked"]["candidatesIds"] &&
    columns["ranked"]["candidatesIds"].length > 0;

  if (column && column.id === "not-ranked") {
    // Current candidate currently appears in the "not-ranked" section
    if (!rankedSectionIsNotEmpty) {
      otherPreferencesSelectOptions.push(
        SelectOption({
          value: "ranked",
          label: t("preferential_voting_ranked"),
        }),
      );
    }
  } else {
    // Current candidate currently appears in the "ranked" section
    otherPreferencesSelectOptions.push(
      SelectOption({
        value: "not-ranked",
        label: t("preferential_voting_not_ranked"),
      }),
    );
  }

  if (rankedSectionIsNotEmpty) {
    const otherRankedCandidates = columns["ranked"]["candidatesIds"]
      .map((candidateId, idx) => {
        return { candidateId: candidateId, index: idx };
      })
      .filter((cand) => {
        return cand.candidateId != candidate.id && index + 1 != cand.index;
      });
    let selectOptionsToAdd = otherRankedCandidates.map((cand) => {
      const candidate = allCandidates[cand.candidateId];
      const candidateLabel = t(
        "preferential_voting_without_equality_move_candidate_above_position_x",
        { count: cand.index + 1, candidate: candidate.content },
      );
      const destinationColumnAndIndex = `ranked~${cand.index}`;
      return SelectOption({
        value: destinationColumnAndIndex,
        label: candidateLabel,
      });
    });
    otherPreferencesSelectOptions = [
      ...otherPreferencesSelectOptions,
      ...selectOptionsToAdd,
    ];

    // Add below bottom ranked candidate
    const bottomRankedCandidateId =
      columns["ranked"]["candidatesIds"][
        columns["ranked"]["candidatesIds"].length - 1
      ];
    const bottomRankedCandidate = allCandidates[bottomRankedCandidateId];
    if (bottomRankedCandidate.id != candidate.id) {
      const belowLastLabel = t(
        "preferential_voting_without_equality_move_candidate_below_position_x",
        {
          count: columns["ranked"]["candidatesIds"].length,
          candidate: bottomRankedCandidate.content,
        },
      );
      const BelowLastOption = SelectOption({
        value: "ranked",
        label: belowLastLabel,
      });
      otherPreferencesSelectOptions = [
        ...otherPreferencesSelectOptions,
        BelowLastOption,
      ];
    }
  }

  // Prepend initial option used as dropdown label
  const firstOption = SelectOption({
    value: "default",
    label: t("preferential_voting_move_candidate_to"),
  });
  otherPreferencesSelectOptions.splice(0, 0, firstOption);
  const renderedRank = e(
    "div",
    {
      className: "preferential-voting-without-equality__candidate-rank",
    },
    showRank ? `${index + 1}.` : null,
  );
  const children = (provided) => {
    return e(
      "div",
      {
        className: "preferential-voting-without-equality__candidate-container",
        ...provided.draggableProps,
        ...provided.dragHandleProps,
        ref: provided.innerRef,
      },
      renderedRank,
      e(
        "div",
        {
          className: "preferential-voting-without-equality__candidate",
        },
        e(MoveCandidateHandle, {
          //...provided.dragHandleProps
        }),
        e(
          "div",
          {
            className: "preferential-voting__candidate-label",
          },
          candidate.content,
        ),
        e(
          "select",
          {
            className: "preferential-voting__candidate-select-destination",
            onChange: onSelectDestinationColumn,
            defaultValue: "default",
            disabled: disabled,
          },
          ...otherPreferencesSelectOptions,
        ),
      ),
    );
  };

  return e(Draggable, {
    draggableId: candidate.id,
    index: index,
    children: children,
    isDragDisabled: disabled,
  });
};

const CandidateWithoutEquality = withTranslation()(
  TranslatableCandidateWithoutEquality,
);

const CandidateList = ({ innerRef, placeholder, children, ...otherProps }) => {
  return e(
    "div",
    {
      className: "preferential-voting__column-candidate-list",
      ref: innerRef,
      ...otherProps,
    },
    children,
    placeholder,
  );
};

// A Column is a list which has a title (prop label) and which can contain candidates.
// These candidates can be moved to other columns by drag & drop or using the select box next to each candidate.
// The user can delete a Column if it contains no candidates.
// A special kind of Column is when `column.id` is "not-ranked": this Column cannot be deleted.
const PreferentialVotingWithoutEqualityColumn = ({
  column,
  label,
  otherColumns,
  columns,
  candidates,
  allCandidates,
  onSelectCandidateDestinationColumn,
  disabled,
}) => {
  const rendered_candidates = candidates.map((candidate, index) => {
    return e(CandidateWithoutEquality, {
      key: candidate.id,
      candidate,
      index,
      column,
      otherColumns,
      columns,
      candidates,
      allCandidates,
      onSelectDestinationColumn: (event) => {
        const selectedValue = event.currentTarget.value;
        if (selectedValue === "default") {
          return;
        }
        const destination = selectedValue.split("~");
        const destinationColumn = destination[0];
        const destinationColumnCandidateIndex =
          destination.length > 1 ? destination[1] : null;
        onSelectCandidateDestinationColumn(
          candidate.id,
          index,
          destinationColumn,
          destinationColumnCandidateIndex,
        );
        event.currentTarget.value = "default"; // restore select display
      },
      disabled,
      showRank: column.id === "not-ranked" ? false : true,
    });
  });
  return e(
    "div",
    {
      className: "preferential-voting__column-container noselect",
    },
    e(
      "div",
      {
        className: "preferential-voting__column-header",
      },
      e(
        "h3",
        {
          className: "preferential-voting__column-title",
        },
        label,
      ),
    ),
    e(Droppable, {
      droppableId: column.id,
      children: (provided) => {
        return e(
          CandidateList,
          {
            innerRef: provided.innerRef,
            ...provided.droppableProps,
            placeholder: provided.placeholder,
          },
          ...rendered_candidates,
        );
      },
    }),
  );
};

export default PreferentialVotingWithoutEqualityColumn;
