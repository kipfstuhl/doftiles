


local watch = require("awful.widget.watch")
local wibox = require("wibox")
local awful = require("awful")
local naughty = require("naughty")

local cpugraph_widget = wibox.widget {
    max_value = 100,
    color = '#74aeab',
    background_color = "#00000000",
    forced_width = 50,
    step_width = 2,
    step_spacing = 1,
    widget = wibox.widget.graph
}

-- mirros and pushs up a bit
-- local cpu_mirror = wibox.container.margin(wibox.container.mirror(
-- 					     cpugraph_widget, { horizontal = true }), 0, 0, 0, 2)

local total_prev = 0
local idle_prev = 0

local cpu_widget, timer = watch("cat /proc/stat | grep '^cpu '", 2,
    function(widget, stdout, stderr, exitreason, exitcode)
        local user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice =
        stdout:match('(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s')

        local total = user + nice + system + idle + iowait + irq + softirq + steal

        local diff_idle = idle - idle_prev
        local diff_total = total - total_prev
        local diff_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10

        if diff_usage > 80 then
            widget:set_color('#ff4136')
        else
            widget:set_color('#74aeab')
        end

        widget:add_value(diff_usage)

        total_prev = total
        idle_prev = idle
    end,
    cpugraph_widget
)


local notification
function show_cpu_status()
   awful.spawn.easy_async([[cat /proc/stat | grep '^cpu ']],
      function(stdout, _, _, _)
	 local user, nice, system, idle, iowait, irq, softirq, steal, guest, guest_nice =
	    stdout:match('(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s(%d+)%s')
	 local total = user + nice + system + idle + iowait + irq + softirq + steal

	 local diff_idle = idle - idle_prev
	 local diff_total = total - total_prev
	 local diff_usage = (1000 * (diff_total - diff_idle) / diff_total + 5) / 10
	 notification = naughty.notify {
	    title = "CPU Usage",
	    -- text = "Total: " .. total .. "\n" ..
	    -- text = "Usage:\t" .. string.format("%.1f", diff_usage),
	    text = string.format("%.1f %%", diff_usage),
	    timeout = 5,
	    hover_timeout = 0.5,
	 }
   end)
end

cpu_widget:connect_signal("mouse::enter", function() show_cpu_status() end)
cpu_widget:connect_signal("mouse::leave", function() naughty.destroy(notification) end)

return wibox.container.margin(wibox.container.mirror(cpugraph_widget, { horizontal = true }), 0, 0, 0, 2)
