// TODO have this line inserted by the server.
const socketPort = '8083';

// element of the currently selected error message
let selectedElement = null;

window.onload = main;

function main() {
  const socket = new WebSocket('ws://localhost:' + socketPort);
  const sidebarEl = document.getElementById('sidebar');
  const viewportEl = document.getElementById('viewport');
  const popViewport = populateViewport.bind(null, viewportEl);
  socket.addEventListener('open', socketOpenHandler);
  socket.addEventListener(
    'message',
    socketMessageHandler.bind(null, sidebarEl, popViewport)
  );
  window.addEventListener('keydown', keydownHandler.bind(null, popViewport));
}

function keydownHandler(popViewport, ev) {
  if (ev.shiftKey) {
    switch (ev.key) {
      case "ArrowDown":
        selectNext(popViewport);
        ev.preventDefault();
        break;
      case "ArrowUp":
        selectPrev(popViewport);
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
function selectFirstError(popViewport) {
  const errors = document.getElementsByClassName('error');
  if (firstError = errors[0]) {
    selectError(popViewport, firstError);
  }
}

// Cycle forward through errors
function selectNext(popViewport) {
  const errorEls = Array.from(document.getElementsByClassName('error'));
  const curIdx = errorEls.findIndex(x => x === selectedElement);
  const newIdx = (curIdx >= errorEls.length - 1) ? 0 : (curIdx + 1);
  selectError(popViewport, errorEls[newIdx]);
}

// Cycle back through errors
function selectPrev(popViewport) {
  const errorEls = Array.from(document.getElementsByClassName('error'));
  const curIdx = errorEls.findIndex(x => x === selectedElement);
  const newIdx = curIdx <= 0 ? (errorEls.length - 1) : (curIdx - 1);
  selectError(popViewport, errorEls[newIdx]);
}

// Select the error represented by the given DOM element
function selectError(popViewport, element) {
  if (element !== selectedElement) {
    selectedElement && selectedElement.classList.remove('selected');
    selectedElement = element;
    element.classList.add('selected');
    popViewport();
  }
}

// Reselect the currently selected error after the sidebar has been refreshed
function reselectError(popViewport) {
  if (selectedElement) {
    let errorId = selectedElement.id;
    let newError = null;
    if (newError = document.getElementById(errorId)) {
      selectError(popViewport, newError);
    }
  } else {
    selectFirstError(popViewport);
  }
}

// Handler for socket message event
function socketMessageHandler(sidebarEl, popViewport, ev) {
  const sidebarHtml = ev.data;
  sidebarEl.innerHTML = sidebarHtml;
  // attach handlers
  const previewEls = document.getElementsByClassName('error');
  Array.from(previewEls, el =>
    el.addEventListener(
      'click',
      previewClickHandler.bind(null, popViewport)
    )
  );
  reselectError(popViewport);
}

// Handle click on an error preview. Triggers viewport population.
function previewClickHandler(popViewport, ev) {
  selectError(popViewport, ev.currentTarget);
}

// Make a request for the viewport html of currently selected error and add it
// to the DOM.
function populateViewport(viewportEl) {
  // make request for html for current selected error
  const selectedId = selectedElement ? selectedElement.id : "";
  const url = '/error/' + selectedId;
  const req = new Request(url);
  const renderViewport = html => {
    viewportEl.innerHTML = html
    const upArrow = document.getElementById('nav-up-arrow');
    if (upArrow !== null) {
      upArrow.addEventListener(
        'click',
        selectNext.bind(null, populateViewport.bind(null, viewportEl))
      );
    }
    const downArrow = document.getElementById('nav-down-arrow');
    if (downArrow !== null) {
      downArrow.addEventListener(
        'click',
        selectPrev.bind(null, populateViewport.bind(null, viewportEl))
      );
    }

//    const locationEl = viewportEl.getElementsByClassName('location')[0];
//    if (locationEl) {
//      locationEl.addEventListener(
//        'click',
//        (ev => navigator.clipboard.writeText(locationEl.dataset.vimCmd))
//      );

  }

  fetch(req)
    .then(resp => resp.text())
    .then(renderViewport);
}
