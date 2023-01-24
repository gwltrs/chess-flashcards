export function eventCode(e) {
  return e.code;
};

export function click(idString) {
  return function () {
    try {
      $('#' + idString).click();
    } catch (err) {}
  };
};