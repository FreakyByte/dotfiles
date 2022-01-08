local awful = require('awful')
local wibox = require('wibox')
local gears = require('gears')
local dpi = require('beautiful').xresources.apply_dpi
local clickable_container = require('widget.clickable-container')
local config_dir = gears.filesystem.get_configuration_dir()
local widget_icon_dir = config_dir .. 'widget/sidebar-toggle/icons/'

return function(s)
	s.toggle_sidebar_widget = wibox.widget {
		{
			id = 'icon',
			image = widget_icon_dir .. 'left-arrow' .. '.svg',
			widget = wibox.widget.imagebox,
			resize = true
		},
		layout = wibox.layout.align.horizontal
	}

	-- icon on start-up
	if s.show_left_panel then
		s.toggle_sidebar_widget.icon:set_image(widget_icon_dir .. 'right-arrow' .. '.svg')
	else
		s.toggle_sidebar_widget.icon:set_image(widget_icon_dir .. 'left-arrow' .. '.svg')
	end

	s.toggle_sidebar_widget_button = wibox.widget {
		{
			s.toggle_sidebar_widget,
			margins = dpi(7),
			widget = wibox.container.margin
		},
		widget = clickable_container
	}

	s.toggle_sidebar_widget_button:buttons(
		gears.table.join(
			awful.button(
				{},
				1,
				nil,
				function()
					local focused = awful.screen.focused()
					focused:emit_signal('sidebar::toggle')
				end
			)
		)
	)

	-- Listen to signals
	screen.connect_signal(
		'sidebar::show',
		function(s)
			if s.left_panel then
				s.toggle_sidebar_widget.icon:set_image(widget_icon_dir .. 'right-arrow' .. '.svg')
			end
		end
	)
	screen.connect_signal(
		'sidebar::hide',
		function(s)
			if s.left_panel then
				s.toggle_sidebar_widget.icon:set_image(widget_icon_dir .. 'left-arrow' .. '.svg')
			end
		end
	)

	return s.toggle_sidebar_widget_button
end
