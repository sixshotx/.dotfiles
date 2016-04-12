-- Docs: http://bit.ly/1VJY6ku
-- local hyper = {"ctrl", "cmd", "alt"}
-- local tiling = require "hs.tiling"
-- hs.hotkey.bind(
--   hyper, "3", function()
--     -- Hs notification
--     -- hs.alert.show("Hello World")

--     -- osx notification
--     hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
-- end)

-- hs.hotkey.bind(hyper, "c", function() tiling.cycleLayout() end)
-- hs.hotkey.bind(hyper, "j", function() tiling.cycle(1) end)
-- hs.hotkey.bind(hyper, "k", function() tiling.cycle(-1) end)
-- hs.hotkey.bind(hyper, "space", function() tiling.promote() end)

-- -- Push the window into the exact center of the screen
-- local function center(window)
--   frame = window:screen():frame()
--   frame.x = (frame.w / 2) - (frame.w / 4)
--   frame.y = (frame.h / 2) - (frame.h / 4)
--   frame.w = frame.w / 2
--   frame.h = frame.h / 2
--   window:setFrame(frame)
-- end

-- hs.hotkey.bind(hyper, "f", function() tiling.toggleFloat(center) end)
-- hs.hotkey.bind(hyper, "h", function() tiling.goToLayout("big-main-vertical") end)

-- -- If you want to set the layouts that are enabled
-- tiling.set(
--   'layouts', {
--     'big-main-vertical'
--     -- , 'fullscreen'
--     -- , 'main-vertical'
-- })

-- hs.hotkey.bind(
--   hyper, "l",
--   function()
--     local allWindows = hs.window.allWindows()
--     hs.window.focusWindowEast()
-- end)

-- -- Reload config
-- hs.hotkey.bind(
--   hyper, "r",
--   function()
--     hs.reload()
--     hs.alert.show("Config reloaded")
-- end)
