local awful = require("awful")
local beautiful = require("beautiful")
local spawn = require("awful.spawn")
local watch = require("awful.widget.watch")
local wibox = require("wibox")


-- local GET_VOLUME_CMD = 'amixer -D pulse sget Master'
-- local INC_VOLUME_CMD = 'amixer -D pulse sset Master 5%+'
-- local DEC_VOLUME_CMD = 'amixer -D pulse sset Master 5%-'
-- local TOG_VOLUME_CMD = 'amixer -D pulse sset Master toggle'
-- local GET_VOLUME_CMD = 'amixer sget Master'
-- local INC_VOLUME_CMD = 'amixer sset Master 5%+'
-- local DEC_VOLUME_CMD = 'amixer sset Master 5%-'
-- local TOG_VOLUME_CMD = 'amixer sset Master toggle'

-- using pulseaudio and directly access alsa at the same time does not
-- work well, the pulseaudio output is very low. pactl does not allow
-- for directly returning the volume, so for this task also amixer is
-- used. This is not a really satisfactory solution, but it works.
local GET_VOLUME_CMD = 'amixer -D pulse get Master'
local INC_VOLUME_CMD = 'pactl set-sink-volume @DEFAULT_SINK@ +5%'
local DEC_VOLUME_CMD = 'pactl set-sink-volume @DEFAULT_SINK@ -5%'
local TOG_VOLUME_CMD = 'pactl set-sink-mute @DEFAULT_SINK@ toggle'

local image = wibox.widget {
   id = "img",
   -- add this icon to theme, use it
   -- maybe use a smaller icon (without the bars on the right?)
   image = "/usr/share/icons/Arc/status/symbolic/audio-volume-medium-symbolic.svg",
   widget = wibox.widget.imagebox
}


-- mirror the text, because the whole widget will be mirrored after
local mirrored_image = wibox.container.mirror(image, { horizontal = true })

-- mirrored text with background
local mirrored_image_with_background = wibox.container.background(mirrored_image)



local volumearc = wibox.widget {
    mirrored_image_with_background,
    max_value = 1,
    thickness = 2,
    start_angle = 4.71238898, -- 2pi*3/4
    forced_height = 17,
    forced_width = 17,
    bg = "#ffffff11",
    paddings = 2,
    value = nil,
    widget = wibox.container.arcchart
}

-- local volumearc_widget = wibox.container.mirror(volumearc, { horizontal = true })

local update_graphic = function(widget, stdout, _, _, _)
    local mute = string.match(stdout, "%[(o%D%D?)%]")
    local volume = string.match(stdout, "(%d?%d?%d)%%")
    volume = tonumber(string.format("% 3d", volume))

    widget.value = volume / 100;
    if mute == "off" then
        widget.colors = { beautiful.widget_red }
    else
        widget.colors = { beautiful.widget_main_color }
    end
end

-- volumearc:connect_signal("button::press", function(_, _, _, button)
--     if (button == 4) then awful.spawn(INC_VOLUME_CMD, false)
--     elseif (button == 5) then awful.spawn(DEC_VOLUME_CMD, false)
--     elseif (button == 1) then awful.spawn(TOG_VOLUME_CMD, false)
--     end

--     spawn.easy_async(GET_VOLUME_CMD, function(stdout, stderr, exitreason, exitcode)
--         update_graphic(volumearc, stdout, stderr, exitreason, exitcode)
--     end)
-- end)

local volumearc_widget, timer = watch(GET_VOLUME_CMD, 10, update_graphic, volumearc)

volumearc_widget:connect_signal("button::press", function(_, _, _, button)
		      if (button == 4) then awful.spawn(INC_VOLUME_CMD, false)
		      elseif (button == 5) then awful.spawn(DEC_VOLUME_CMD, false)
		      elseif (button == 1) then awful.spawn(TOG_VOLUME_CMD, false)
		      end
		      
		      timer:emit_signal("timeout") -- this prevents (?) memory leak
		      -- at least it drastically improves the situation


		      
		      -- spawn.easy_async(GET_VOLUME_CMD, function(stdout, stderr, exitreason, exitcode)
		      -- update_graphic(volumearc, stdout, stderr, exitreason, exitcode)
		      -- end)
end)

return wibox.container.mirror(volumearc_widget, {horizontal = true})
