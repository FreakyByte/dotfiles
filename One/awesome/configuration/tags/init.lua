local awful = require('awful')
local gears = require('gears')
local beautiful = require('beautiful')
local icons = require('theme.icons')
local apps = require('configuration.apps')
-- xmonad-style tag management (workspaces) using the Charitabe library
local charitable = require("charitable") 	

-- Set tags layout
tag.connect_signal(
	'request::default_layouts',
	function()
	    awful.layout.append_default_layouts({
		    awful.layout.suit.tile,
		    awful.layout.suit.tile.left,
		    awful.layout.suit.tile.bottom,
		    awful.layout.suit.tile.top,
		    awful.layout.suit.fair,
		    awful.layout.suit.fair.horizontal,
		    awful.layout.suit.spiral,
		    awful.layout.suit.spiral.dwindle,
		    awful.layout.suit.max,
		    --awful.layout.suit.max.fullscreen,
		    awful.layout.suit.magnifier,
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
tags[1].type = 'internet'
tags[1].icon = icons.web_browser
tags[1].default_app = apps.default.web_browser
tags[1].gap = beautiful.useless_gap
tags[1].screen = 1

tags[2].type = 'terminal'
tags[2].icon = icons.terminal
tags[2].default_app = apps.default.terminal
tags[2].gap = beautiful.useless_gap
tags[2].screen = 1

tags[3].type = 'files'
tags[3].icon = "/usr/share/icons/Flat-Remix-Green-Dark/places/scalable/folder.svg"
tags[3].default_app = apps.default.file_manager
tags[3].gap = beautiful.useless_gap
tags[3].screen = 1

tags[4].type = 'daw'
tags[4].icon = "/usr/share/icons/Papirus/64x64/apps/cockos-reaper.svg"
tags[4].default_app = 'reaper'
tags[4].gap = beautiful.useless_gap
tags[4].screen = 1

tags[5].type = 'any'
tags[5].icon = "/usr/share/icons/Flat-Remix-Green-Dark/apps/scalable/accessories-ebook-reader.svg"
tags[5].default_app = apps.default.graphics
tags[5].gap = beautiful.useless_gap
tags[5].screen = 1

tags[6].type = 'games'
tags[6].icon = icons.games
tags[6].default_app = apps.default.game
tags[6].gap = beautiful.useless_gap
tags[6].screen = 1

tags[7].type = 'multimedia'
tags[7].icon = icons.multimedia
tags[7].default_app = apps.default.multimedia
tags[7].gap = beautiful.useless_gap
tags[7].screen = 2

tags[8].type = 'slack'
tags[8].icon = "/usr/share/icons/Flat-Remix-Green-Dark/apps/scalable/slack.svg"
tags[8].default_app = 'slack'
tags[8].gap = beautiful.useless_gap
tags[8].screen = 2

tags[9].type = 'discord'
tags[9].icon = "/usr/share/discord/discord.png"
tags[9].default_app = 'discord'
tags[9].gap = beautiful.useless_gap
tags[9].screen = 2

tags[10].type = 'music'
tags[10].icon = "/usr/share/spotify/icons/spotify-linux-128.png"
tags[10].default_app = 'spotify'
tags[10].gap = beautiful.useless_gap
tags[10].screen = 2

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
