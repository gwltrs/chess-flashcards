exports.eventCode = (e) => {
  return e.code;
};

exports.click = (idString) => {
  return function () {
    try {
      $('#' + idString).click();
    } catch (err) {}
  };
};