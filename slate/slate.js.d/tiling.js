tiling = (function() {
  retile = function (win) {
    var windows = [];
    slate.eachApp(function (app) {
      app.eachWindow(function (win) {
        if (win.isMinimizedOrHidden()) return;
        if (null == win.title() || win.title() === "") return;
        windows.push(win);
      });
    });

    //fullscreen it
    //windowObject.doOperation(slate.operation("move", {
    //      "x" : "screenOriginX",
    //      "y" : "screenOriginY",
    //      "width" : "screenSizeX",
    //      "height" : "screenSizeY"
    //    })
    //);
    var ss = win.screen().rect();
    var windowSizeX = ss.width * 0.4;
    var windowSizeY = ss.height / (windows.length - 1);
    var winPosY = 0;

    for (i = 0; i < windows.length; i++) {
      w = windows[i];

      if (w.title() == win.title()) {
        mainWidth = (windows.length > 1) ? "screenSizeX*0.6" : "screenSizeX";

        w.doOperation("move", {
          "x": "screenOriginX",
          "y": "screenOriginY",
          "width": mainWidth,
          "height": "screenSizeY"
        });
      }
      else {
        w.doOperation("move", {
          "x": "screenSizeX*0.6",
          "y": winPosY,
          "width": windowSizeX,
          "height": windowSizeY
        });

        winPosY += windowSizeY;
      }
    }
  }

// Basic keybinds
  slate.bindAll({
    "f:cmd,alt": slate.op("move", {
      "x": "screenOriginX", "y": "screenOriginY",
      "width": "screenSizeX", "height": "screenSizeY"
    }),
    "left:cmd,alt": slate.op("push", {
      "direction": "left", "style": "bar-resize:screenSizeX/2"
    }),
    "right:cmd,alt": slate.op("push", {
      "direction": "right", "style": "bar-resize:screenSizeX/2"
    }),
    "r:cmd,alt": retile,
    "w:alt": slate.op("hint"),
  })
})();
