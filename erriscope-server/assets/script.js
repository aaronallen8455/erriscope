// element of the currently selected error message
let selectedElement = null;

window.onload = main;

function main() {
  initSocket();
  window.addEventListener('keydown', keydownHandler);
}

function initSocket() {
  const socket = new WebSocket('ws://localhost:' + socketPort);
  socket.addEventListener('open', socketOpenHandler);
  socket.addEventListener(
    'message',
    socketMessageHandler
  );
  socket.addEventListener('closed', handleSocketClose);
  socket.addEventListener('error', handleSocketClose);
}

function handleSocketClose(ev) {
  console.log('socket closed! ' + ev);
}

function keydownHandler(ev) {
  if (ev.shiftKey) {
    switch (ev.key) {
      case "ArrowDown":
        selectNext();
        ev.preventDefault();
        break;
      case "ArrowUp":
        selectPrev();
        ev.preventDefault();
        break;
    }
  }
}

// Handler for socket open event
function socketOpenHandler(ev) {
  const socket = ev.target;
  socket.send('frontend');
}

// Select the first error if there is no currently selected error
function selectFirstError() {
  const errors = document.getElementsByClassName('error');
  let firstError = null;
  if (firstError = errors[0]) {
    selectError(firstError);
  } else {
    // no errors
    selectedElement = null;
  }
}

// Cycle forward through errors
function selectNext() {
  const errorEls = Array.from(document.getElementsByClassName('error'));
  const curIdx = errorEls.findIndex(x => x === selectedElement);
  const newIdx = (curIdx >= errorEls.length - 1) ? 0 : (curIdx + 1);
  selectError(errorEls[newIdx]);
}

// Cycle back through errors
function selectPrev() {
  const errorEls = Array.from(document.getElementsByClassName('error'));
  const curIdx = errorEls.findIndex(x => x === selectedElement);
  const newIdx = curIdx <= 0 ? (errorEls.length - 1) : (curIdx - 1);
  selectError(errorEls[newIdx]);
}

// Select the error represented by the given DOM element
function selectError(element) {
  if (element !== selectedElement) {
    selectedElement && selectedElement.classList.remove('selected');
    selectedElement = element;
    element.classList.add('selected');
    populateViewport();
  }
}

// Reselect the currently selected error after the sidebar has been refreshed
function reselectError() {
  if (selectedElement) {
    let errorId = selectedElement.id;
    let newError = null;
    if (newError = document.getElementById(errorId)) {
      selectError(newError);
    } else {
      selectFirstError();
    }
  } else {
    selectFirstError();
  }
}

// Handler for socket message event
function socketMessageHandler(ev) {
  const sidebarEl = document.getElementById('sidebar');
  const sidebarHtml = ev.data;
  sidebarEl.innerHTML = sidebarHtml;
  // attach handlers
  const previewEls = document.getElementsByClassName('error');
  Array.from(previewEls, el =>
    el.addEventListener(
      'click',
      previewClickHandler
    )
  );
  reselectError();
  if (selectedElement === null) {
    clearViewport();
  }
}

// Handle click on an error preview. Triggers viewport population.
function previewClickHandler(ev) {
  selectError(ev.currentTarget);
}

// Make a request for the viewport html of currently selected error and add it
// to the DOM.
function populateViewport() {
  // make request for html for current selected error
  const selectedId = selectedElement ? selectedElement.id : "";
  const viewportEl = document.getElementById('viewport');
  const url = '/error/' + selectedId;
  const req = new Request(url);
  const renderViewport = html => {
    viewportEl.innerHTML = html
    const upArrow = document.getElementById('nav-up-arrow');
    if (upArrow !== null) {
      upArrow.addEventListener(
        'click',
        selectPrev
      );
    }
    const downArrow = document.getElementById('nav-down-arrow');
    if (downArrow !== null) {
      downArrow.addEventListener(
        'click',
        selectNext
      );
    }
//    const locationEl = viewportEl.getElementsByClassName('location')[0];
//    if (locationEl) {
//      locationEl.addEventListener(
//        'click',
//        (ev => navigator.clipboard.writeText(locationEl.dataset.vimCmd))
//      );

  };

  const errorHandler = err => {
    const errMessage = document.createElement('div');
    errMessage.classList.add('server-error');
    errMessage.innerText =
      "Error! Unable to connect to the server. Ensure that 'erriscope-server' is running then reload the page."
    viewportEl.innerHTML = "";
    viewportEl.appendChild(errMessage);
  };


  fetch(req)
    .then(resp => resp.text())
    .then(renderViewport)
    .catch(errorHandler);
}

// Clears the viewport if there are no errors
function clearViewport() {
  const viewportEl = document.getElementById('viewport');
  viewportEl.innerHTML = "";
}
