import React, { createElement as e } from "react";
import { withTranslation } from "react-i18next";

function Breadcrumb(props) {
  const renderedSteps = props.steps.map((step, index) => {
    let className = "breadcrumb__step";
    if (step.isCurrentStep){
      className += " breadcrumb__step--current";
    }
    return e(
      React.Fragment,
      null,
      e(
        'div',
        {
          className: className
        },
        e(
          'span',
          {
            className: "breadcrumb__step__title"
          },
          step.title || ""
        ),
        e(
          'span',
          {
            className: "breadcrumb__step__short-title",
            title: step.title || ""
          },
          step.shortTitle || ""
        )
      ),
      e(
        'div',
        {
          className: "breadcrumb__step-separator"
        }
      )
    );
  });
  return e(
    "div",
    {
      className: "breadcrumb"
    },
    e(
      'div',
      {
        className: "breadcrumb__step-separator"
      }
    ),
    ...renderedSteps
  );
}

Breadcrumb.defaultProps = {
  steps: [
    {
      title: "Title of step 1",
      shortTitle: "Step 1",
      isCurrentStep: true
    },
    {
      title: "Title of step 2",
      shortTitle: "Step 2"
    }
  ]
};

function TranslatableVoteBreadcrumb({ t, currentStep=1, ...props }){
  let voteBreadcrumbSteps = [
    {
      title: t("breadcrumb_input_credential")
    },
    {
      title: t("breadcrumb_answer_to_questions")
    },
    {
      title: t("breadcrumb_review_and_encrypt")
    },
    {
      title: t("breadcrumb_authenticate")
    },
    {
      title: t("breadcrumb_confirm")
    }
  ];
  voteBreadcrumbSteps = voteBreadcrumbSteps.map(function(el, index){
    return {
      ...el,
      shortTitle: t("breadcrumb_step_x", {count: index+1}),
      isCurrentStep: currentStep === index+1 ? true : false
    };
  });
  return e(
    Breadcrumb,
    {
      steps: voteBreadcrumbSteps,
      ...props
    }
  );
}

const VoteBreadcrumb = withTranslation()(TranslatableVoteBreadcrumb);

export { Breadcrumb, TranslatableVoteBreadcrumb, VoteBreadcrumb };
export default Breadcrumb;
