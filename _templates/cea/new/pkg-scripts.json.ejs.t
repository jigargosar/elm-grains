---
inject: true
to: package.json
after: scripts
skip_if: 'start": "'
---
    "start": "nodemon -w yarn.lock -w package-lock.json -w elm.json -x env ELM_DEBUGGER=true elm-app start",
    "now-build": "npm add create-elm-app && elm-app build && mv build dist",
    "build": "elm-app build",
    "bs": "npm run build && serve build",
    "deploy": "now -n elm-done && now alias",
