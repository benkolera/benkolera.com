---
title: Live Reloading with Hakyll
type: post
tags: haskell,hakyll
---

With the help of some existing nodejs code, having your hakyll blog reload in
the browser as you save is a piece of cake! 

You'll need nodejs and npm already installed in the recommended way for your OS.

Create a ```package.json``` in your project root:

``` {.json filename="package.json"}
{
  "name": "blog",
  "version": "1.0.0",
  "dependencies": {
    "livereload": "^0.3.7"
  }
}
```

Install livereload by running ```npm install```.

Create ```livereload.js``` next to the ```package.json```:

``` {.javascript filename="livereload.js"}
livereload = require('livereload');
server = livereload.createServer();
server.watch(__dirname + "/_site");
```

Start the server side by running ```cabal run watch``` and
```node livereload.js``` in separate windows.

Install the 
[Chrome LiveReload Plugin](https://chrome.google.com/webstore/detail/livereload/jnihajbhpnppcggbcgedagnkighmdlei?hl=en)
or [Firefox LiveReload Plugin](https://addons.mozilla.org/en-Us/firefox/addon/livereload/)
, navigate to <http://localhost:8000/> and click the live reload icon to turn on
live reloading for the page. This will inject some javascript into your page to
make the reloading happen.

Each save in your editor will be picked up by the Hakyll watch server and
recompiled into your _site directory. The nodejs server watching _site will
notify the javascript running on your blog (added by the chrome plugin) through
a socket.io connection and reload the page accordingly. Very helpful!
