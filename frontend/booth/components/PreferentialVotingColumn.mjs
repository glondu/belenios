import { WhiteNiceButton } from "./NiceButton.mjs";

const { Draggable, Droppable } = window.ReactBeautifulDnd;
const React = window.React;
const ReactDOM = window.ReactDOM;
const e = React.createElement;

const TranslatableMoveCandidateHandle = ({t, tReady, ...props}) => {
  // the icon visual is made using CSS
  return e(
    "div",
    {
      className: "preferential-voting__candidate-handle draggable",
      title: t("preferential_voting_drag_candidate"),
      ...props
    },
    e(
      "div",
      {
        className: "preferential-voting__candidate-handle__icon"
      }
    )
  );
}

const MoveCandidateHandle = ReactI18next.withTranslation()(TranslatableMoveCandidateHandle);

const TranslatableCandidate = ({ candidate, index, otherColumns, onSelectDestinationColumn, disabled, t }) => {
  const otherPreferencesSelectOptions = otherColumns.map(column => {
    return e(
      "option",
      {
        value: column.id
      },
      column.label
    );
  });
  otherPreferencesSelectOptions.splice(0, 0, e(
    "option",
    {
      value: "default"
    },
    t("preferential_voting_move_candidate_to")
  ));
  const children = (provided) => {
    return e(
      "div",
      {
        className: "preferential-voting__candidate",
        ...provided.draggableProps,
        ...provided.dragHandleProps,
        ref: provided.innerRef
      },
      e(
        MoveCandidateHandle,
        {
          //...provided.dragHandleProps
        }
      ),
      e(
        "div",
        {
          className: "preferential-voting__candidate-label"
        },
        candidate.content
      ),
      e(
        "select",
        {
          className: "preferential-voting__candidate-select-destination",
          onChange: onSelectDestinationColumn,
          defaultValue: "default",
          disabled: disabled
        },
        ...otherPreferencesSelectOptions
      )
    );
  };
  return e(
    Draggable,
    {
      draggableId: candidate.id,
      index: index,
      children: children,
      isDragDisabled: disabled
    }
  );
}

const Candidate = ReactI18next.withTranslation()(TranslatableCandidate);

const CandidateList = ({innerRef, placeholder, children, ...otherProps}) => {
  return e(
    "div",
    {
      className: "preferential-voting__column-candidate-list",
      ref: innerRef,
      ...otherProps
    },
    children,
    placeholder
  );
};

const TranslatableDeletePreferenceLevelButton = ({onClick, disabled, t}) => {
  return e(
    WhiteNiceButton,
    {
      tagName: "a",
      label: "×", // or 🗑✖
      title: t("preferential_voting_delete_preference_level"),
      onClick: disabled ? null : onClick,
      className: "preferential-voting__column-actions__action preferential-voting__column-actions__action__delete-preference-level",
      disabled: disabled
    }
  );
};

const DeletePreferenceLevelButton = ReactI18next.withTranslation()(TranslatableDeletePreferenceLevelButton);

// A Column is a list which has a title (prop label) and which can contain candidates.
// These candidates can be moved to other columns by drag & drop or using the select box next to each candidate.
// The user can delete a Column if it contains no candidates.
// A special kind of Column is when `column.id` is "not-ranked": this Column cannot be deleted.
const PreferentialVotingColumn = ({ column, label, otherColumns, candidates, onClickDeleteButton, onSelectCandidateDestinationColumn, disabled
 }) => {
  const rendered_candidates = candidates.map((candidate, index) => {
    return e(
      Candidate,
      {
        key: candidate.id,
        candidate,
        index,
        otherColumns,
        onSelectDestinationColumn: (event) => {
          onSelectCandidateDestinationColumn(candidate.id, index, event.currentTarget.value);
        },
        disabled: disabled
      }
    );
  });
  const columnActions = column.id === "not-ranked" ? null : e(
    "div",
    {
      className: "preferential-voting__column-actions"
    },
    e(
      DeletePreferenceLevelButton,
      {
        onClick: onClickDeleteButton,
        disabled: disabled
      }
    )
  );
  return e(
    "div",
    {
      className: "preferential-voting__column-container noselect"
    },
    e(
      "div",
      {
        className: "preferential-voting__column-header"
      },
      e(
        "h3",
        {
          className: "preferential-voting__column-title"
        },
        label
      ),
      columnActions,
    ),
    e(
      Droppable,
      {
        droppableId: column.id,
        children: (provided) => {
          return e(
            CandidateList,
            {
              innerRef: provided.innerRef,
              ...provided.droppableProps,
              placeholder: provided.placeholder
            },
            ...rendered_candidates,
          );
        }
      }
    )
  );
}

export default PreferentialVotingColumn;
