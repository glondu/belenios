const e = React.createElement;

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
      title: t("Input credential")
    },
    {
      title: t("Answer to questions")
    },
    {
      title: t("Review and encrypt")
    },
    {
      title: t("Authenticate")
    },
    {
      title: t("Confirm")
    }
  ];
  voteBreadcrumbSteps = voteBreadcrumbSteps.map(function(el, index){
    return {
      ...el,
      shortTitle: t("stepX", {count: index+1}),
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

const VoteBreadcrumb = ReactI18next.withTranslation()(TranslatableVoteBreadcrumb);

export { Breadcrumb, TranslatableVoteBreadcrumb, VoteBreadcrumb };
export default Breadcrumb;
