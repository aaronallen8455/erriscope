# Erriscope

### What is it?

This is a lightweight GHC plugin that provides an ergonomic web UI for viewing
the errors and warnings emitted during compilation. It seeks to improve the
developer experience for a REPL based workflow.

Advantages over viewing errors in the terminal include:
- A summary view of all errors organized by module and showing only the first 4
  lines of each error message.
- A view containing the complete error message complete with syntax highlighting.
- Warnings persist across reloads which is an issue with GHCi.

![Screenshot](/screenshot.png)

### Quickstart

- Install the server component:
```bash
> cabal update && cabal install erriscope-server
```
or
```bash
> stack update && stack install erriscope-server
```

- Start the server:
```bash
> erriscope-server
```

- Start up a REPL with the following arguments to enable the plugin:
```bash
cabal repl -b erriscope --repl-options="-fplugin Erriscope"
```
or
```bash
stack repl --package erriscope --ghci-options "-fplugin Erriscope"
```

- Point your browser to `localhost:8888`

### User's Guide

You can use the `Shift+UpArrow` and `Shift+DownArrow` hotkeys to quickly cycle
through errors.

You can change the port on which the server and plugin run using command line
arguments. For example, if you want to run the server on port 8000, start the
server as follows:
```bash
erriscope-server 8000
```
and add the additon GHC plugin option:
```bash
cabal repl -b erriscope --repl-options="-fplugin Erriscope -fplugin-opts Erriscope:8000"
```

### Issues
- Erriscope currently only supports GHC 8.10.* and 9.0.*.
- Currently doesn't report errors when building a project outside of the REPL
