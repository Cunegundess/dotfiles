return {
  'echasnovski/mini.nvim',
  version = false,
  config = function()
    require('mini.ai').setup { n_lines = 500 }

    local pad = function(str, n)
      return string.rep(' ', n) .. str
    end

    -- Function from [echasnovski](https://github.com/echasnovski/nvim).
    local greeting = function()
      local hour = tonumber(vim.fn.strftime '%H')
      -- [04:00, 12:00) - morning, [12:00, 20:00) - day, [20:00, 04:00) - evening
      local part_id = math.floor((hour + 4) / 8) + 1
      local day_part = ({ 'evening', 'morning', 'afternoon', 'evening' })[part_id]
      local username = vim.loop.os_get_passwd()['username'] or 'USERNAME'
      return ('Good %s, %s'):format(day_part, username)
    end

    local fortune = function()
      local ok, quote = pcall(function()
        local f = assert(io.popen('fortune -s', 'r'))
        local s = assert(f:read '*a')
        f:close()
        return s
      end)
      return ok and quote or nil
    end

    local longest_line = function(s)
      local lines = vim.fn.split(s, '\n')
      local lengths = vim.tbl_map(vim.fn.strdisplaywidth, lines)
      return math.max(unpack(lengths))
    end

    local random_banners = function()
      local headers = {
        lazyvim = [[
                ██╗      █████╗ ███████╗██╗   ██╗██╗   ██╗██╗███╗   ███╗          Z
                ██║     ██╔══██╗╚══███╔╝╚██╗ ██╔╝██║   ██║██║████╗ ████║      Z
                ██║     ███████║  ███╔╝  ╚████╔╝ ██║   ██║██║██╔████╔██║   z
                ██║     ██╔══██║ ███╔╝    ╚██╔╝  ╚██╗ ██╔╝██║██║╚██╔╝██║ z
                ███████╗██║  ██║███████╗   ██║    ╚████╔╝ ██║██║ ╚═╝ ██║
                ╚══════╝╚═╝  ╚═╝╚══════╝   ╚═╝     ╚═══╝  ╚═╝╚═╝     ╚═╝
            ]],

        neovim_modern = [[
                                                          
                    ████ ██████           █████      ██
                  ███████████             █████ 
                  █████████ ███████████████████ ███   ███████████
                  █████████  ███    █████████████ █████ ██████████████
                █████████ ██████████ █████████ █████ █████ ████ █████
              ███████████ ███    ███ █████████ █████ █████ ████ █████
              ██████  █████████████████████ ████ █████ █████ ████ ██████
            ]],

        neovim_bloody = [[
                ███▄    █ ▓█████  ▒█████   ██▒   █▓ ██▓ ███▄ ▄███▓
                ██ ▀█   █ ▓█   ▀ ▒██▒  ██▒▓██░   █▒▓██▒▓██▒▀█▀ ██▒
              ▓██  ▀█ ██▒▒███   ▒██░  ██▒ ▓██  █▒░▒██▒▓██    ▓██░
              ▓██▒  ▐▌██▒▒▓█  ▄ ▒██   ██░  ▒██ █░░░██░▒██    ▒██
              ▒██░   ▓██░░▒████▒░ ████▓▒░   ▒▀█░  ░██░▒██▒   ░██▒
              ░ ▒░   ▒ ▒ ░░ ▒░ ░░ ▒░▒░▒░    ░ ▐░  ░▓  ░ ▒░   ░  ░
              ░ ░░   ░ ▒░ ░ ░  ░  ░ ▒ ▒░    ░ ░░   ▒ ░░  ░      ░
                  ░   ░ ░    ░   ░ ░ ░ ▒       ░░   ▒ ░░      ░
                        ░    ░  ░    ░ ░        ░   ░         ░
                                              ░
            ]],

        pacman = [[
                          ██████
                      ████▒▒▒▒▒▒████
                    ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒██
                  ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██
                ██▒▒▒▒▒▒▒▒    ▒▒▒▒▒▒▒▒
                ██▒▒▒▒▒▒  ▒▒▓▓▒▒▒▒▒▒  ▓▓▓▓
                ██▒▒▒▒▒▒  ▒▒▓▓▒▒▒▒▒▒  ▒▒▓▓
              ██▒▒▒▒▒▒▒▒▒▒    ▒▒▒▒▒▒▒▒    ██
              ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██
              ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██
              ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██
              ██▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒██
              ██▒▒██▒▒▒▒▒▒██▒▒▒▒▒▒▒▒██▒▒▒▒██
              ████  ██▒▒██  ██▒▒▒▒██  ██▒▒██
              ██      ██      ████      ████
            ]],

        hydra = [[
                ⣴⣶⣤⡤⠦⣤⣀⣤⠆     ⣈⣭⣿⣶⣿⣦⣼⣆
                  ⠉⠻⢿⣿⠿⣿⣿⣶⣦⠤⠄⡠⢾⣿⣿⡿⠋⠉⠉⠻⣿⣿⡛⣦
                        ⠈⢿⣿⣟⠦ ⣾⣿⣿⣷    ⠻⠿⢿⣿⣧⣄
                        ⣸⣿⣿⢧ ⢻⠻⣿⣿⣷⣄⣀⠄⠢⣀⡀⠈⠙⠿⠄
                        ⢠⣿⣿⣿⠈    ⣻⣿⣿⣿⣿⣿⣿⣿⣛⣳⣤⣀⣀
                ⢠⣧⣶⣥⡤⢄ ⣸⣿⣿⠘  ⢀⣴⣿⣿⡿⠛⣿⣿⣧⠈⢿⠿⠟⠛⠻⠿⠄
                ⣰⣿⣿⠛⠻⣿⣿⡦⢹⣿⣷   ⢊⣿⣿⡏  ⢸⣿⣿⡇ ⢀⣠⣄⣾⠄
              ⣠⣿⠿⠛ ⢀⣿⣿⣷⠘⢿⣿⣦⡀ ⢸⢿⣿⣿⣄ ⣸⣿⣿⡇⣪⣿⡿⠿⣿⣷⡄
              ⠙⠃   ⣼⣿⡟  ⠈⠻⣿⣿⣦⣌⡇⠻⣿⣿⣷⣿⣿⣿ ⣿⣿⡇ ⠛⠻⢷⣄
                    ⢻⣿⣿⣄   ⠈⠻⣿⣿⣿⣷⣿⣿⣿⣿⣿⡟ ⠫⢿⣿⡆
                    ⠻⣿⣿⣿⣿⣶⣶⣾⣿⣿⣿⣿⣿⣿⣿⣿⡟⢀⣀⣤⣾⡿⠃
            ]],

        saturn = [[
                                                    _.oo.
                            _.u[[/;:,.         .odMMMMMM'
                          .o888UU[[[/;:-.  .o@P^    MMM^
                        oN88888UU[[[/;::-.        dP^
                        dNMMNN888UU[[[/;:--.   .o@P^
                      ,MMMMMMN888UU[[/;::-. o@^
                      NNMMMNN888UU[[[/~.o@P^
                      888888888UU[[[/o@^-..
                      oI8888UU[[[/o@P^:--..
                  .@^  YUU[[[/o@^;::---..
                oMP     ^/o@P^;:::---..
              .dMMM    .o@^ ^;::---...
            dMMMMMMM@^`       `^^^^
            YMMMUP^
            ^^
            ]],

        saturn_plus = [[
                                                        ___
                                                      ,o88888
                                                  ,o8888888'
                            ,:o:o:oooo.        ,8O88Pd8888"
                        ,.::.::o:ooooOoOoO. ,oO8O8Pd888'"
                      ,.:.::o:ooOoOoOO8O8OOo.8OOPd8O8O"
                      , ..:.::o:ooOoOOOO8OOOOo.FdO8O8"
                    , ..:.::o:ooOoOO8O888O8O,COCOO"
                    , . ..:.::o:ooOoOOOO8OOOOCOCO"
                    . ..:.::o:ooOoOoOO8O8OCCCC"o
                        . ..:.::o:ooooOoCoCCC"o:o
                        . ..:.::o:o:,cooooCo"oo:o:
                    `   . . ..:.:cocoooo"'o:o:::'
                    .`   . ..::ccccoc"'o:o:o:::'
                    :.:.    ,c:cccc"':.:.:.:.:.'
                  ..:.:"'`::::c:"'..:.:.:.:.:.'
                ...:.'.:.::::"'    . . . . .'
              .. . ....:."' `   .  . . ''
            . . . ...."'
            .. . ."'
            .
            ]],

        skull = [[
                  ⠀⠀⠀⢀⣀⣤⣤⣤⣤⣄⡀⠀⠀⠀⠀
                  ⠀⢀⣤⣾⣿⣾⣿⣿⣿⣿⣿⣿⣷⣄⠀⠀
                  ⢠⣾⣿⢛⣼⣿⣿⣿⣿⣿⣿⣿⣿⣿⣷⡀
                  ⣾⣯⣷⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣧
                  ⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿⣿
                  ⣿⡿⠻⢿⣿⣿⣿⣿⣿⣿⣿⣿⡿⠻⢿⡵
                  ⢸⡇⠀⠀⠉⠛⠛⣿⣿⠛⠛⠉⠀⠀⣿⡇
                  ⢸⣿⣀⠀⢀⣠⣴⡇⠹⣦⣄⡀⠀⣠⣿⡇
                  ⠈⠻⠿⠿⣟⣿⣿⣦⣤⣼⣿⣿⠿⠿⠟⠀
                  ⠀⠀⠀⠀⠸⡿⣿⣿⢿⡿⢿⠇⠀⠀⠀⠀
                  ⠀⠀⠀⠀⠀⠀⠈⠁⠈⠁⠀⠀⠀⠀⠀⠀
              ]],

        mini = [[

                    █              █

    ████████████ ███ ████████ ███
    ██████████████ ████ ██████████ ████
    █████ ████ █████ ████ █████ █████ ████
    █████ ████ █████ ████ █████ █████ ████
    █████ ████ ████████ █████ ████████
    ]],
      }

      local header_keys = {}
      for k in pairs(headers) do
        table.insert(header_keys, k)
      end

      math.randomseed(os.time()) -- garante aleatório a cada abertura
      local random_header = headers[header_keys[math.random(#header_keys)]]
      return random_header
    end

    local starter = require 'mini.starter'
    starter.setup {
      -- Default values with exception of '-' as I use it to open mini.files.
      query_updaters = 'abcdefghijklmnopqrstuvwxyz0123456789_.',

    -- stylua: ignore
    items = {
      starter.sections.sessions(5, true),
      starter.sections.recent_files(3, false, false),
      {
        { name = "Mason",          action = "Mason",                 section = "Updaters"},
        { name = "Update plugins", action = "lua vim.pack.update()", section = "Updaters"},
        { name = "Visited files",  action = "Pick visit_paths",      section = "Actions"},
        { name = "Quit Neovim",    action = "qall",                  section = "Actions"},
      },
    },

      header = function()
        local banner = random_banners()
        local msg = greeting()
        local msg_pad = longest_line(banner) - msg:len()
        return banner .. pad(msg, msg_pad)
      end,

      -- Fortune slows startup a small amount, but I like it.
      footer = fortune(),
    }

    require('mini.surround').setup()
    require('mini.pairs').setup()
    require('mini.comment').setup()
    require('mini.indentscope').setup { delay = 50, symbol = '│' }
    -- require('mini.notify').setup()
    require('mini.diff').setup()
    require('mini.animate').setup()
    require('mini.fuzzy').setup()
    -- require('mini.statusline').setup { use_icons = vim.g.have_nerd_font }
    --
    -- require('mini.files').setup {
    --   mappings = {
    --     close = 'q',
    --     go_in = '<CR>',
    --     go_out = '<ESC>',
    --     mark_goto = "'",
    --     mark_set = 'm',
    --     reset = '<BS>',
    --     reveal_cwd = '@',
    --     show_help = 'g?',
    --     synchronize = '=',
    --     trim_left = '<',
    --     trim_right = '>',
    --   },
    --   vim.keymap.set('n', '<leader>e', require('mini.files').open, { desc = 'Abrir explorador de arquivos (mini.files)' }),
    -- }

    -- require('mini.pick').setup {
    --   options = {
    --     use_cache = true,
    --   },
    -- }
    --
    -- local pickers = {
    --   files = function()
    --     require('mini.pick').builtin.files()
    --   end,
    --
    --   grep = function()
    --     require('mini.pick').builtin.grep_live()
    --   end,
    --
    --   buffers = function()
    --     require('mini.pick').builtin.buffers()
    --   end,
    --
    --   help = function()
    --     require('mini.pick').builtin.help()
    --   end,
    --
    --   recent = function()
    --     require('mini.pick').builtin.oldfiles()
    --   end,
    -- }
    --
    -- vim.keymap.set('n', '<leader>ff', pickers.files, { desc = '[F]ind [F]ile' })
    -- vim.keymap.set('n', '<leader>fg', pickers.grep, { desc = '[F]ind by [G]rep' })
    -- vim.keymap.set('n', '<leader>fb', pickers.buffers, { desc = '[F]ind [B]uffer' })
    -- vim.keymap.set('n', '<leader>fh', pickers.help, { desc = '[F]ind [H]elp' })
    -- vim.keymap.set('n', '<leader>fr', pickers.recent, { desc = '[F]ind [R]ecent Files' })
    --
    -- require('mini.clue').setup {
    --   triggers = {
    --     { mode = 'n', keys = '<Leader>' },
    --     { mode = 'x', keys = '<Leader>' },
    --   },
    --   clues = {
    --     require('mini.clue').gen_clues.builtin_completion(),
    --     require('mini.clue').gen_clues.g(),
    --     require('mini.clue').gen_clues.marks(),
    --     require('mini.clue').gen_clues.registers(),
    --     require('mini.clue').gen_clues.windows(),
    --     require('mini.clue').gen_clues.z(),
    --     { mode = 'n', keys = '<leader>f', desc = '+find' },
    --     { mode = 'n', keys = '<leader>e', desc = '+explorer' },
    --   },
    --   window = {
    --     delay = 100,
    --   },
    -- }
  end,
}
