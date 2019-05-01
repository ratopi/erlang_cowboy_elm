#!/bin/bash -e

elm make src/Main.elm --optimize --output=elm.js

cp -v elm.js ../priv/static/elm.min.js
