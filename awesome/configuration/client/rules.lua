local awful = require('awful')
local gears = require('gears')
local ruled = require('ruled')
local beautiful = require('beautiful')
local client_keys = require('configuration.client.keys')
local client_buttons = require('configuration.client.buttons')

ruled.client.connect_signal(
	'request::rules',
	function()
		-- All clients will match this rule.
		ruled.client.append_rule {
			id = 'global',
			rule = {},
			properties = {
				focus = awful.client.focus.filter,
				raise = true,
				floating = false,
				maximized = false,
				above = false,
				below = false,
				ontop = false,
				sticky = false,
				maximized_horizontal = false,
				maximized_vertical = false,
				keys = client_keys,
				buttons = client_buttons,
				screen = awful.screen.preferred,
				placement = awful.placement.no_overlap + awful.placement.no_offscreen
			}
		}

		ruled.client.append_rule {
			id = 'round_clients',
			rule_any = {
				type = {
					'normal',
					'dialog'
				}
			},
			except_any  = {
				name = {'Discord Updater'}
			},
			properties = {
				round_corners = true,
				shape = beautiful.client_shape_rounded
			}
		}

		-- Titlebar rules
		ruled.client.append_rule {
			id 		= 'titlebars',
			rule_any = {
				type = {
					'normal',
					'dialog',
					'modal',
					'utility'
				}
			},
			properties = {
				titlebars_enabled = true
			}
		}

		-- Dialogs
		ruled.client.append_rule {
			id = 'dialog',
			rule_any = {
				type  = {'dialog'},
				class = {'Wicd-client.py', 'calendar.google.com'}
			},
			properties = {
				titlebars_enabled = true,
				floating = true,
				above = true,
				skip_decoration = true,
				placement = awful.placement.centered
			}
		}

		-- Modals
		ruled.client.append_rule {
			id = 'modal',
			rule_any = {
				type = {'modal'}
			},
			properties = {
				titlebars_enabled = true,
				floating = true,
				above = true,
				skip_decoration = true,
				placement = awful.placement.centered
			}
		}

		-- Utilities
		ruled.client.append_rule {
			id = 'utility',
			rule_any = {
				type = {'utility'}
			},
			properties = {
				titlebars_enabled = false,
				floating = true,
				skip_decoration = true,
				placement = awful.placement.centered
			}
		}

		-- Splash
		ruled.client.append_rule {
			id = 'splash',
			rule_any = {
				type = {'splash'},
				name = {'Discord Updater'}
			},
			properties = {
				titlebars_enabled = false,
				round_corners = false,
				floating = true,
				above = true,
				skip_decoration = true,
				placement = awful.placement.centered
			}
		}
		
		-- Browsers 
		ruled.client.append_rule {
			id = 'internet',
			rule_any = {
				class = {
					'firefox',
					'Tor Browser',
					'Chromium',
					'Google-chrome',
				}
			},
			properties = {
				tag = '1'
			}
		}


		-- Terminal emulators
		ruled.client.append_rule {
			id = 'terminals',
			rule_any = {
				class = {
					'URxvt',
					'XTerm',
					'UXTerm',
					'kitty',
					'K3rmit'
				}
			},
			properties = {
				tag = '2',
				switch_to_tags = true,
				size_hints_honor = false,
				titlebars_enabled = true
			}
		}

		-- File managers
		ruled.client.append_rule {
			id = 'files',
			rule_any = {
				class = {
					'dolphin',
					'ark',
					'Nemo',
					'File-roller'
				}
			},
			properties = {
				tag = '3',
				switch_to_tags = true
			}
		}
		
		-- DAW
		ruled.client.append_rule {
			id = 'daw',
			rule_any = {
				class = {
					'REAPER',
				}
			},
			properties = {
				tag = '4',
				switch_to_tags = true
			}
		}
		
		-- Note taking
		ruled.client.append_rule {
			id = 'notes',
			rule_any = {
				class = {
					'Xournal',
					'Xournalpp',
                    'com.github.xournalpp.xournalpp',
                    'Com.github.xournalpp.xournalpp',
				}
			},
			properties = {
				tag = '4',
				switch_to_tags = true
			}
		}

		-- Media and Text Editing, Word Processing
		ruled.client.append_rule {
			id = 'graphics',
			rule_any = {
				class = {
					'Geany',
					'Atom',
					'Subl3',
					'code-oss',
					'Gimp-2.10',
					'Inkscape',
					'Flowblade',
					'Xreader',
				},
				name  = {
					'LibreOffice',
					'libreoffice'
				}
			},
			properties = {
				tag = '5'
			}
		}

		-- Gaming
		ruled.client.append_rule {
			id = 'gaming',
			rule_any = {
				class = {
					'dolphin-emu',
					'Steam',
					'Citra',
					'supertuxkart'
				},
				name = {'Steam'}
			},
			properties = {
				tag = '6',
				skip_decoration = true,
				switch_to_tags = true,
				placement = awful.placement.centered
			}
		}

		-- Multimedia
		ruled.client.append_rule {
			id = 'multimedia',
			rule_any = {
				class = {
					'vlc',
				}
			},
			properties = {
				tag = '7',
				placement = awful.placement.centered
			}
		}

		-- Slack
		ruled.client.append_rule {
			rule_any = {
				class = {
					'slack'
				}
			},
			properties = {
				tag = '8',
			}
		}
		-- Discord
		ruled.client.append_rule {
			rule_any = {
				class = {'discord', 'TelegramDesktop' },
				name = {'Discord Updater'}
			},
			properties = {
				tag = '9',
			}
		}
		-- Spotify
		ruled.client.append_rule {
			rule_any = {
				class = {
					'spotify'
				}
			},
			properties = {
				tag = '10',
			}
		}

		-- Image viewers
		ruled.client.append_rule {
			id        = 'image_viewers',
			rule_any  = {
				class    = {
					'feh',
					'Pqiv',
					'Sxiv'
				},
			},
			properties = {
				titlebars_enabled = true,
				skip_decoration = true,
				floating = true,
				ontop = true,
				placement = awful.placement.centered
			}
		}

		-- Floating
		ruled.client.append_rule {
			id       = 'floating',
			rule_any = {
				name = { 'Volume Control', },
				instance    = {
					'file_progress',
					'Popup',
					'nm-connection-editor',
				},
				class = {
					'scrcpy',
					'Mugshot',
					'Pulseeffects',
					'Blueman-manager',
				},
				role    = {
					'AlarmWindow',
					'ConfigManager',
					'pop-up'
				}
			},
			properties = {
				titlebars_enabled = true,
				skip_decoration = true,
				ontop = true,
				floating = true,
				focus = awful.client.focus.filter,
				raise = true,
				keys = client_keys,
				buttons = client_buttons,
				placement = awful.placement.centered
			}
		}

        -- emacs-everwhere
		ruled.client.append_rule {
			rule_any = {
				name = { 'emacs-everywhere', },
			},
			properties = {
				titlebars_enabled = true,
				skip_decoration = true,
				ontop = true,
				floating = true,
				focus = awful.client.focus.filter,
				raise = true,
				keys = client_keys,
				buttons = client_buttons,
                width = awful.screen.focused().workarea.width * 0.7,
                height = awful.screen.focused().workarea.height * 0.7,
                -- somehow, this roundabout way of calling awful.placement.centered makes sure the window gets resized first and then moved
                -- source: https://github.com/awesomeWM/awesome/issues/2497#issuecomment-1458899649
                placement = function(...) return awful.placement.centered(...)end,
			}
		}
	end
)

-- Normally we'd do this with a rule, but some program like spotify doesn't set its class or name
-- until after it starts up, so we need to catch that signal.
client.connect_signal(
	'property::class',
	function(c)
		if c.class == 'Spotify' then
			local window_mode = false

			-- Check if fullscreen or window mode
			if c.fullscreen then
				window_mode = false
				c.fullscreen = false
			else
				window_mode = true
			end

			-- Check if Spotify is already open
			local app = function (c)
				return ruled.client.match(c, {class = 'Spotify'})
			end

			local app_count = 0
			for c in awful.client.iterate(app) do
				app_count = app_count + 1
			end

			-- If Spotify is already open, don't open a new instance
			if app_count > 1 then
				c:kill()
				-- Switch to previous instance
				for c in awful.client.iterate(app) do
					c:jump_to(false)
				end
			else
				-- Move the instance to specified tag on this screen
				local t = awful.tag.find_by_name(awful.screen.focused(), '10')
				c:move_to_tag(t)
				t:view_only()

				-- Fullscreen mode if not window mode
				if not window_mode then
					c.fullscreen = true
				else
					c.floating = true
					awful.placement.centered(c, {honor_workarea = true})
				end
			end
		end
	end
)
