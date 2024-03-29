export function saveFile (name) {
  return function (text) {
    return function () {
      const element = document.createElement('a');
      element.setAttribute('href', 'data:text/plain;charset=utf-8,' + encodeURIComponent(text));
      element.setAttribute('download', name);
      if (document.createEvent) {
        const event = document.createEvent('MouseEvents');
        event.initEvent('click', true, true);
        element.dispatchEvent(event);
      } else {
        element.click();
      }
    };
  };
};

export function openFileDialogImpl() {
  return new Promise((res, rej) => {
    const fileInput = document.getElementById("fileInput");
    fileInput.addEventListener("change", handleFiles);
    function handleFiles() {
      const file = this.files[0];
      const fileReader = new FileReader();
      fileReader.onload = function(e) {
        const text = fileReader.result;
        res(text);
      }
      if (file instanceof Blob) {
        fileReader.readAsText(file);
      }
    }
    fileInput.click();
  }); 
};