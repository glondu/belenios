'use strict';

class InnerTranslatedLikeButton extends React.Component {
  constructor(props) {
    super(props);
    this.state = { liked: false };
  }

  render() {
    const { t } = this.props;
    if (this.state.liked) {
      return t('You liked this');
    }

    return e(
      'button',
      { onClick: () => this.setState({ liked: true }) },
      t('Like')
    );
  }
}

const TranslatedLikeButton = ReactI18next.withTranslation()(InnerTranslatedLikeButton);
