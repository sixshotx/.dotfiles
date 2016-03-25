-- Docs: http://bit.ly/1VJY6ku
local super = {"ctrl", "cmd", "alt"}
local tiling = require "hs.tiling"
hs.hotkey.bind(
  super, "3", function()
    -- Hs notification
    -- hs.alert.show("Hello World")

    -- osx notification
    hs.notify.new({title="Hammerspoon", informativeText="Hello World"}):send()
end)

hs.hotkey.bind(super, "c", function() tiling.cycleLayout() end)
hs.hotkey.bind(super, "j", function() tiling.cycle(1) end)
hs.hotkey.bind(super, "k", function() tiling.cycle(-1) end)
hs.hotkey.bind(super, "space", function() tiling.promote() end)
-- hotkey.bind(super, "f", function() tiling.goToLayout("fullscreen") end)

-- If you want to set the layouts that are enabled
tiling.set(
  'layouts', {
    'fullscreen', 'main-vertical'
})


hs.hotkey.bind(
  super, "l",
  function()
    local allWindows = hs.window.allWindows()
    hs.window.focusWindowEast()
end)

-- Reload config
hs.hotkey.bind(
  super, "r",
  function()
    hs.reload()
    hs.alert.show("Config reloaded")
end)
