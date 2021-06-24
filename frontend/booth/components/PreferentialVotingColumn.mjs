const { Draggable, Droppable } = window.ReactBeautifulDnd;
const React = window.React;
const ReactDOM = window.ReactDOM;
const e = React.createElement;

const MoveCandidateHandle = (props) => {
  // the icon visual is made using CSS
  return e(
    "div",
    {
      className: "preferential-voting__candidate-handle draggable",
      title: "Déplacer ce candidat", // TODO: i18n
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

const Candidate = ({ candidate, index, otherColumns, onSelectDestinationColumn, disabled }) => {
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
    "Déplacer vers" // TODO: i18n
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

const DeletePreferenceLevelButton = ({onClick, disabled}) => {
  const cssClass = "preferential-voting__column-actions__action";
  return e(
    "div",
    {
      className: disabled ? cssClass : cssClass + " clickable",
      onClick: disabled ? null : onClick,
      title: "Supprimer ce niveau de préférence" // TODO: i18n
    },
    e(
      "div",
      {
        className: "preferential-voting__column-actions__action__delete-preference-level"
      },
      "×" // or 🗑✖
    )
    
  );
};


// A Column is a list which has a title (prop label) and which can contain candidates.
// These candidates can be moved to other columns by drag & drop or using the select box next to each candidate.
// The user can delete a Column if it contains no candidates.
// A special kind of Column is when `column.id` is "not-ranked": this Column cannot be deleted.
const Column = ({ column, label, otherColumns, candidates, onClickDeleteButton, onSelectCandidateDestinationColumn, disabled
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

export default Column;

