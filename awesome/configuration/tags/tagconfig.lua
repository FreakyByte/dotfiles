local icons = require('theme.icons')
local apps = require('configuration.apps')
local system = require('configuration.config').system or "One"

return function(tags)
  for i = 1, #tags do
    tags[i].gap = beatiful.useless_gap
  end

  tags[1].type = 'internet'
  tags[1].icon = icons.web_browser
  tags[1].default_app = apps.default.web_browser

  tags[2].type = 'terminal'
  tags[2].icon = icons.terminal
  tags[2].default_app = apps.default.terminal

  tags[3].type = 'files'
  tags[3].icon = "/usr/share/icons/Flat-Remix-Green-Dark/places/scalable/folder.svg"
  tags[3].default_app = apps.default.file_manager


  tags[5].type = 'any'
  tags[5].icon = "/usr/share/icons/Flat-Remix-Green-Dark/apps/scalable/accessories-ebook-reader.svg"
  tags[5].default_app = apps.default.graphics

  tags[6].type = 'games'
  tags[6].icon = icons.games
  tags[6].default_app = apps.default.game

  tags[7].type = 'multimedia'
  tags[7].icon = icons.multimedia
  tags[7].default_app = apps.default.multimedia

  tags[8].type = 'slack'
  tags[8].icon = "/usr/share/icons/Flat-Remix-Green-Dark/apps/scalable/slack.svg"
  tags[8].default_app = 'slack'

  tags[9].type = 'discord'
  tags[9].icon = "/usr/share/discord/discord.png"
  tags[9].default_app = 'discord'

  tags[10].type = 'music'
  tags[10].icon = "/usr/share/spotify/icons/spotify-linux-128.png"
  tags[10].default_app = 'spotify'

  if config.system == "One" then
    -- default screens for tags
    tags[1].screen = 1
    tags[2].screen = 1
    tags[3].screen = 1
    tags[4].screen = 1
    tags[5].screen = 1
    tags[6].screen = 2
    tags[7].screen = 2
    tags[8].screen = 2
    tags[9].screen = 1
    tags[10].screen = 2

    tags[4].type = 'daw'
    tags[4].icon = "/usr/share/icons/Papirus/64x64/apps/cockos-reaper.svg"
    tags[4].default_app = 'reaper'

  elseif config.system == "Ideapad" then
    tags[4].type = 'notes'
    tags[4].icon = "/usr/share/icons/Mint-Y/apps/96/xournal.png"
    tags[4].default_app = 'xournalpp'
    tags[4].gap = beautiful.useless_gap
  end

  return tags
end
