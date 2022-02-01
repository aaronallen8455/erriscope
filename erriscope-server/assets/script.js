// TODO have this line inserted by the server.
const socketPort = '9160';

// index of the currently selected error message
let currentlySelectedIdx = 0;
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

// Handler for socket message event
function socketMessageHandler(sidebarEl, popViewport, ev) {
  const sidebarHtml = ev.data;
  sidebarEl.innerHTML = sidebarHtml;
  currentlySelectedIdx = 0;
  // attach handlers
  const previewEls = document.getElementsByClassName('error');
  selectedElement = previewEls[0];
  Array.from(previewEls, el =>
    el.addEventListener(
      'click',
      previewClickHandler.bind(null, popViewport)
    )
  );
  // re-render viewport
  popViewport();
}

// Handle click on an error preview. Triggers viewport population.
function previewClickHandler(popViewport, ev) {
  const el = ev.currentTarget;
  const idx = el.dataset.index;
  el.classList.add('selected');
  selectedElement && selectedElement.classList.remove('selected');
  selectedElement = el
  currentlySelectedIdx = idx;
  popViewport();
}

function populateViewport(viewportEl) {
  // make request for html for current selected error
  const url = '/error/' + currentlySelectedIdx;
  const req = new Request(url);

  fetch(req)
    .then(resp => resp.text())
    .then(html => viewportEl.innerHTML = html);
}
