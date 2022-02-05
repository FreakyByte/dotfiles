local awful = require('awful')
local gears = require('gears')
local beautiful = require('beautiful')
-- xmonad-style tag management (workspaces) using the Charitabe library
local charitable = require("charitable")

-- Set tags layout
tag.connect_signal(
	'request::default_layouts',
	function()
		awful.layout.append_default_layouts({
			awful.layout.suit.tile,
			--awful.layout.suit.tile.left,
			awful.layout.suit.tile.bottom,
			--awful.layout.suit.tile.top,
			awful.layout.suit.fair,
			awful.layout.suit.fair.horizontal,
			--awful.layout.suit.spiral,
			awful.layout.suit.spiral.dwindle,
			awful.layout.suit.max,
			--awful.layout.suit.max.fullscreen,
			--awful.layout.suit.magnifier,
			awful.layout.suit.corner.nw,
			awful.layout.suit.floating,
		})
	end
)

local tags = charitable.create_tags(
   { "1", "2", "3", "4", "5", "6", "7", "8", "9", "10" },
   {
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
      awful.layout.layouts[1],
   }
)

-- giving the tags a bunch of extra information
tags = require('tagconfig')(tags)


awful.screen.connect_for_each_screen(function(s)
    -- Show an unselected tag when a screen is connected
    for i = 1, #tags do
         if not tags[i].selected then
             tags[i].screen = s
             tags[i]:view_only()
             break
         end
    end

    -- create a special scratch tag for double buffering
    s.scratch = awful.tag.add('scratch-' .. s.index, {})
end)



local update_gap_and_shape = function(t)
	-- Get current tag layout
	local current_layout = awful.tag.getproperty(t, 'layout')
	-- If the current layout is awful.layout.suit.max
	if (current_layout == awful.layout.suit.max) then
		-- Set clients gap to 0 and shape to rectangle if maximized
		t.gap = 0
		for _, c in ipairs(t:clients()) do
			if not c.floating or not c.round_corners or c.maximized or c.fullscreen then
				c.shape = beautiful.client_shape_rectangle
			else
				c.shape = beautiful.client_shape_rounded
			end
		end
	else
		t.gap = beautiful.useless_gap
		for _, c in ipairs(t:clients()) do
			if not c.round_corners or c.maximized or c.fullscreen then
				c.shape = beautiful.client_shape_rectangle
			else
				c.shape = beautiful.client_shape_rounded
			end
		end
	end
end

-- Change tag's client's shape and gap on change
tag.connect_signal(
	'property::layout',
	function(t)
		update_gap_and_shape(t)
	end
)

-- Change tag's client's shape and gap on move to tag
tag.connect_signal(
	'tagged',
	function(t)
		update_gap_and_shape(t)
	end
)

-- Focus on urgent clients
awful.tag.attached_connect_signal(
	s,
	'property::selected',
	function()
		local urgent_clients = function (c)
			return awful.rules.match(c, {urgent = true})
		end
		for c in awful.client.iterate(urgent_clients) do
			if c.first_tag == mouse.screen.selected_tag then
				c:emit_signal('request::activate')
				c:raise()
			end
		end
	end
)
-- work around bugs in awesome 4.0 through 4.3+
-- see https://github.com/awesomeWM/awesome/issues/2780
awful.tag.history.restore = function() end
