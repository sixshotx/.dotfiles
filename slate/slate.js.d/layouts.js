var layouts = (function() {
    var self = {},
        LEFT_SCREEN = "0",
        RIGHT_SCREEN = "1";

    var leftThird = slate.op("move", {
        "screen": LEFT_SCREEN,
        "x": "screenOriginX",
        "y": "screenOriginY",
        "width": "screenSizeX*1/3",
        "height": "screenSizeY"
    })

    var rightTwoThirds = slate.op("move", {
        "screen": LEFT_SCREEN,
        "x": "screenOriginX+(screenSizeX/3)",
        "y": "screenOriginY",
        "width": "screenSizeX*2/3",
        "height": "screenSizeY"
    });

    var leftHalf = slate.op("move", {
        "screen": LEFT_SCREEN,
        "x": "screenOriginX",
        "y": "screenOriginY",
        "width": "screenSizeX*1/2",
        "height": "screenSizeY"
    });

    var rightHalf = slate.op("move", {
        "screen": LEFT_SCREEN,
        "x": "screenOriginX + (screenSizeX/2)",
        "y": "screenOriginY",
        "width": "screenSizeX/2",
        "height": "screenSizeY"
    });

    // Taking notes with nvALT.
    self.takingNotes = slate.layout("takingNotes", {
        "nvALT": {
            "operations": [
                function(win) {
                    win.doOperation(leftHalf);
                }
            ]
        },

        "Marked 2": {
            "operations": [
                function(win) {
                    win.doOperation(rightHalf);
                }
            ]
        }
    });

    self.dualMonitors = slate.layout("dualMonitors", {
        "_after_": {
            "operations": [
                function(win) {
                    helpers.setWindowFocusDirection(LEFT_SCREEN, -1);
                    helpers.setWindowFocusDirection(RIGHT_SCREEN, 1);
                }
            ]
        },

        "Firefox": {
            "operations": [
                function (windowObject) {
                    var title = windowObject.title();
                    // if (title !== undefined && title.match(/[Inspector|Console|Debugger|Style Editor|Canvas|Performance|Timeline|Network].+/)) {
                    if (title !== undefined && title.match(/(Inspector|Console).+/)) {
                      windowObject.doOperation(rightHalf);
                    } else {
                      windowObject.doOperation(leftHalf);
                    }
                }
            ],
            "ignore-fail": true,
            "repeat": true
        },

        "FirefoxDeveloperEdition": {
            "operations": [
                function (windowObject) {
                    var title = windowObject.title();
                    if (title !== undefined && title.match(/^Inspector.+/)) {
                      windowObject.doOperation(rightHalf);
                    } else {
                      windowObject.doOperation(leftHalf);
                    }
                }
            ],
            "ignore-fail": true,
            "repeat": true
        },

        "Google Chrome": {
            "operations": [
                function (windowObject) {
                    var title = windowObject.title();
                    if (title !== undefined && title.match(/^Developer\sTools\s-\s.+$/)) {
                      windowObject.doOperation(rightHalf);
                    } else {
                      windowObject.doOperation(leftHalf);
                    }
                }
            ],
            "ignore-fail": true,
            "repeat": true
        },

        // "Emacs": {
        //     "sort-title": true,
        //     "operations": [
        //         slate.op("move", {
        //             "screen": RIGHT_SCREEN,
        //             "x": "screenOriginX",
        //             "y": "screenOriginY",
        //             "width": "screenSizeX/3",
        //             "height": "screenSizeY"
        //         }),
        //         slate.op("move", {
        //             "screen": RIGHT_SCREEN,
        //             "x": "screenOriginX+(screenSizeX/3)",
        //             "y": "screenOriginY",
        //             "width": "screenSizeX/3",
        //             "height": "screenSizeY"
        //         })
        //     ]
        // },

        "iTerm": {
            "sort-title": true,
            "operations": [
                slate.op("move", {
                    "screen": RIGHT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY",
                    "width": "screenSizeX",
                    "height": "screenSizeY"
                })
            ]
        },

        "MacVim": {
            "sort-title": true,
            "operations": [
                slate.op("move", {
                    "screen": RIGHT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY",
                    "width": "screenSizeX",
                    "height": "screenSizeY"
                })
            ]
         },


        "IntelliJ IDEA": {
            "sort-title": true,
            "operations": [
                slate.op("move", {
                    "screen": RIGHT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY",
                    "width": "screenSizeX",
                    "height": "screenSizeY"
                })
            ]
         },

        "Sublime Text": {
            "operations": [
                slate.op("move", {
                    "screen": RIGHT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY",
                    "width": "screenSizeX",
                    "height": "screenSizeY"
                })
            ]
        },


        "nvALT": {
            "operations": [
                slate.op("move", {
                    "screen": LEFT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY",
                    "width": "screenSizeX/3",
                    "height": "screenSizeY/2"
                })
            ]
        },

        "Marked": {
            "operations": [
                slate.op("move", {
                    "screen": LEFT_SCREEN,
                    "x": "screenOriginX",
                    "y": "screenOriginY + screenSizeY/2",
                    "width": "screenSizeX/3",
                    "height": "screenSizeY/2"
                })
            ]
        }
    });

    return self;
}());
