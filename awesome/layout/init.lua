local awful = require('awful')
local left_panel = require('layout.left-panel')
local top_panel = require('layout.top-panel')
local right_panel = require('layout.right-panel')

-- Create a wibox panel for each screen and add it
screen.connect_signal(
	'request::desktop_decoration',
	function(s)
		--if s.index == 1 then
		--	s.left_panel = left_panel(s)
		--	s.top_panel = top_panel(s, true)
		--	s.show_left_panel = false
		--else
		--	s.top_panel = top_panel(s, false)
		--end

		s.left_panel = left_panel(s)
		s.show_left_panel = false
		s.top_panel = top_panel(s, false)
		s.right_panel = right_panel(s)
		s.right_panel_show_again = false
	end
)

screen.connect_signal(
	'sidebar::show',
	function(s)
		if s.left_panel then
			s.left_panel.visible = true
			s.show_left_panel = true
			s.top_panel.enable_offset()
		end
	end
)
screen.connect_signal(
	'sidebar::hide',
	function(s)
		if s.left_panel then
			s.left_panel.visible = false
			s.show_left_panel = false
			s.top_panel.disable_offset()
		end
	end
)
screen.connect_signal(
	'sidebar::toggle',
	function(s)
		if s.left_panel then
			if s.show_left_panel then
				s:emit_signal('sidebar::hide')
			else 
				s:emit_signal('sidebar::show')
			end
		end
	end
)


-- Hide bars when app go fullscreen
function update_bars_visibility()
	for s in screen do
		if s.selected_tag then
			local fullscreen = s.selected_tag.fullscreen_mode
			-- Order matter here for shadow
			s.top_panel.visible = not fullscreen
			if s.left_panel then
				s.left_panel.visible = not fullscreen and s.show_left_panel
				-- without this "and" the left panel would become visible again when update_bars_visibility is triggered
			end
			if s.right_panel then
				if fullscreen and s.right_panel.visible then
					s.right_panel:toggle()
					s.right_panel_show_again = true
				elseif not fullscreen and not s.right_panel.visible and s.right_panel_show_again then
					s.right_panel:toggle()
					s.right_panel_show_again = false
				end
			end
		end
	end
end

tag.connect_signal(
	'property::selected',
	function(t)
		update_bars_visibility()
	end
)

client.connect_signal(
	'property::fullscreen',
	function(c)
		if c.first_tag then
			c.first_tag.fullscreen_mode = c.fullscreen
		end
		update_bars_visibility()
	end
)

client.connect_signal(
	'unmanage',
	function(c)
		if c.fullscreen then
			c.screen.selected_tag.fullscreen_mode = false
			update_bars_visibility()
		end
	end
)
