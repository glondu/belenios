import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

function Progress({ currentStep = 1, ...props }) {
  const renderedSteps = props.steps.map((step, index) => {
    const isCurrentStep = currentStep === index + 1;
    const isStepDone = currentStep > index + 1;

    let className =
      "progress__step" +
      (isCurrentStep
        ? " progress__step--current"
        : " progress__step--not-current") +
      (isStepDone ? " progress__step--done" : "");

    return e(
      React.Fragment,
      null,
      e(
        "div",
        {
          className: className,
        },
        e(
          "div",
          {
            className:
              "progress__step__dot-container" +
              (index !== 0 ? " line-left" : "") +
              (index !== props.steps.length - 1 ? " line-right" : ""),
          },
          e(
            "div",
            {
              className: "progress__step__dot",
            },
            "",
          ),
        ),
        e(
          "span",
          {
            className: "progress__step__title",
          },
          step.title || "",
        ),
        e(
          "span",
          {
            className: "progress__step__short-title",
            title: step.title || "",
          },
          step.shortTitle || "",
        ),
      ),
    );
  });
  return e(
    "div",
    {
      className: "progress",
    },
    ...renderedSteps,
  );
}

function TranslatableVoteProgress({ t, ...props }) {
  let voteProgressSteps = [
    {
      title: t("progress_input_credential"),
    },
    {
      title: t("progress_answer_to_questions"),
    },
    {
      title: t("progress_review_and_encrypt"),
    },
    {
      title: t("progress_authenticate"),
    },
    {
      title: t("progress_confirm"),
    },
  ];
  voteProgressSteps = voteProgressSteps.map(function (el, index) {
    return {
      ...el,
      shortTitle: t("progress_step_x", { step: index + 1 }),
    };
  });
  return e(Progress, {
    steps: voteProgressSteps,
    ...props,
  });
}

const VoteProgress = withTranslation()(TranslatableVoteProgress);

export { Progress, TranslatableVoteProgress, VoteProgress };
export default Progress;
