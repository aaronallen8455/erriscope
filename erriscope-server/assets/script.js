// TODO have this line inserted by the server.
const socketPort = '9160';

// index of the currently selected error message
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
}

// Handler for socket open event
function socketOpenHandler(ev) {
  const socket = ev.target;
  socket.send('frontend');
}

// Select the first error if there is no currently selected error
function selectFirstErrorIfNoneSelected(popViewport) {
  if (selectedElement === null) {
    const errors = document.getElementsByClassName('error');
    if (firstError = errors[0]) {
      selectError(popViewport, firstError);
    }
  }
}

// Cycle forward through errors
function selectNext(popViewport) {
  if (selectedElement) {
    const el = selectedElement.nextElementSibling
            || selectedElement.parentElement.firstElementChild;
    selectError(popViewport, el);
  } else {
    selectFirstErrorIfNoneSelected(popViewport);
  }
}

// Cycle back through errors
function selectPrev(popViewport) {
  if (selectedElement) {
    const el = selectedElement.prevElementSibling
            || selectedElement.parentElement.lastElementChild;
    selectError(popViewport, el);
  } else {
    selectFirstErrorIfNoneSelected(popViewport);
  }
}

// Select the error represented by the given DOM element
function selectError(popViewport, element) {
  const idx = element.dataset.index;
  selectedElement && selectedElement.classList.remove('selected');
  selectedElement = element
  element.classList.add('selected');
  popViewport();
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
  selectFirstErrorIfNoneSelected(popViewport);
}

// Handle click on an error preview. Triggers viewport population.
function previewClickHandler(popViewport, ev) {
  selectError(popViewport, ev.currentTarget);
}

function populateViewport(viewportEl) {
  // make request for html for current selected error
  const selectedId = selectedElement ? selectedElement.dataset.index : "";
  const url = '/error/' + selectedId;
  const req = new Request(url);

  fetch(req)
    .then(resp => resp.text())
    .then(html => viewportEl.innerHTML = html);
}
