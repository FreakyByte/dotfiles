local awful = require('awful')
local beautiful = require('beautiful')
local wibox = require('wibox')
local gears = require('gears')
local icons = require('theme.icons')
local system = require('configuration.config').system or "One"
local dpi = beautiful.xresources.apply_dpi
local clickable_container = require('widget.clickable-container')
local tag_list = require('widget.tag-list')
local task_list = require('widget.task-list')

local top_panel = function(s, offset)

	local panel = wibox
	{
		ontop = true,
		screen = s,
		type = 'dock',
		height = dpi(28),
		width = s.geometry.width,
		x = s.geometry.x,
		y = s.geometry.y,
		stretch = false,
		bg = beautiful.background,
		fg = beautiful.fg_normal
	}

	-- functions so that offset can be changed even after the panel is created
	panel.update_offset = function()
		panel.width = s.geometry.width - offsetx
		panel.x = s.geometry.x + offsetx
	end
	panel.enable_offset = function ()
		panel.offset = true
		offsetx = dpi(45)
		panel.update_offset()
	end
	panel.disable_offset = function ()
		panel.offset = false
		offsetx = 0
		panel.update_offset()
	end
	panel.toggle_offset = function()
		if panel.offset then
			panel.disable_offset()
		else
			panel.enable_offset()
		end
	end

	-- set starting offset
	if offset then
		panel.enable_offset()
	else
		panel.disable_offset()
	end


	panel:struts
	{
		top = dpi(28)
	}

	panel:connect_signal(
		'mouse::enter',
		function()
			local w = mouse.current_wibox
			if w then
				w.cursor = 'left_ptr'
			end
		end
	)

	s.systray = wibox.widget {
		visible = false,
		base_size = dpi(20),
		horizontal = true,
		screen = 'primary',
		widget = wibox.widget.systray
	}

	local clock				= require('widget.clock')(s)
	local layout_box		= require('widget.layoutbox')(s)
	local add_button		= require('widget.open-default-app')(s)
	s.sidebar_toggler		= require('widget.sidebar-toggle')(s)
	s.tray_toggler			= require('widget.tray-toggle')
	s.updater				= require('widget.package-updater')()
	s.screen_rec			= require('widget.screen-recorder')()
	s.mpd					= require('widget.mpd')()
	s.bluetooth				= require('widget.bluetooth')()
	if system == "Ideapad" then
		s.battery = require('widget.battery')()
	else
		s.battery = nil
	end
	s.network				= require('widget.network')()
	s.info_center_toggle	= require('widget.info-center-toggle')()

	panel : setup {
		layout = wibox.layout.align.horizontal,
		expand = 'none',
		{
			layout = wibox.layout.fixed.horizontal,
			--task_list(s),
			s.sidebar_toggler,
			add_button,
			tag_list(s),

		},
		clock,
		{
			layout = wibox.layout.fixed.horizontal,
			spacing = dpi(5),
			{
				s.systray,
				margins = dpi(5),
				widget = wibox.container.margin
			},
			s.tray_toggler,
			s.updater,
			s.screen_rec,
			s.mpd,
			s.network,
			s.bluetooth,
			s.battery,
			layout_box,
			s.info_center_toggle
		}
	}

	return panel
end


return top_panel
