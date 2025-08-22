return {
  {
    "epwalsh/obsidian.nvim",
    version = "*", -- recommended, use latest release instead of latest commit
    lazy = true,
    ft = "markdown",
    -- Replace the above line with this if you only want to load obsidian.nvim for markdown files in your vault:
    -- event = {
    --   -- If you want to use the home shortcut '~' here you need to call 'vim.fn.expand'.
    --   -- E.g. "BufReadPre " .. vim.fn.expand "~" .. "/my-vault/*.md"
    --   -- refer to `:h file-pattern` for more examples
    --   "BufReadPre path/to/my-vault/*.md",
    --   "BufNewFile path/to/my-vault/*.md",
    -- },
    dependencies = {
      "nvim-lua/plenary.nvim",
      "hrsh7th/nvim-cmp",
      "nvim-telescope/telescope.nvim",
      "nvim-treesitter/nvim-treesitter",
    },
    opts = {
      workspaces = {
        {
          name = "notes",
          path = "~/Documentos/notes",
        },
      },
      daily_notes = {
        folder = "~/Documentos/notes/0-inbox/",
        -- Optional, if you want to automatically insert a template from your template directory like 'daily.md'
        template = nil,
      },
      templates = {
        folder = "~/Documentos/notes/4-templates/",
        date_format = "%Y-%m-%d-%a",
        time_format = "%H:%M",
      },
      mappings = {
        -- Overrides the 'gf' mapping to work on markdown/wiki links within your vault.
        ["gf"] = {
          action = function()
            return require("obsidian").util.gf_passthrough()
          end,
          opts = { noremap = false, expr = true, buffer = true },
        },
        -- Toggle check-boxes.
        ["<leader>ch"] = {
          action = function()
            return require("obsidian").util.toggle_checkbox()
          end,
          opts = { buffer = true },
        },
        -- Smart action depending on context, either follow link or toggle checkbox.
        ["<cr>"] = {
          action = function()
            return require("obsidian").util.smart_action()
          end,
          opts = { buffer = true, expr = true },
        },
      },
      ui = {
        enable = false,
        bullets = { char = "•", hl_group = "ObsidianBullet" },
        external_link_icon = { char = "", hl_group = "ObsidianExtLinkIcon" },
        hl_groups = {
          -- The options are passed directly to `vim.api.nvim_set_hl()`. See `:help nvim_set_hl`.
          ObsidianTodo = { bold = true, fg = "#f78c6c" },
          ObsidianDone = { bold = true, fg = "#89ddff" },
          ObsidianRightArrow = { bold = true, fg = "#f78c6c" },
          ObsidianTilde = { bold = true, fg = "#ff5370" },
          ObsidianImportant = { bold = true, fg = "#d73128" },
          ObsidianBullet = { bold = true, fg = "#89ddff" },
          ObsidianRefText = { underline = true, fg = "#c792ea" },
          ObsidianExtLinkIcon = { fg = "#c792ea" },
          ObsidianTag = { italic = true, fg = "#89ddff" },
          ObsidianBlockID = { italic = true, fg = "#89ddff" },
          ObsidianHighlightText = { bg = "#75662e" },
        },
      },
      follow_url_func = function(url)
        vim.ui.open(url)
        -- vim.ui.open(url, { cmd = { "firefox" } })
      end,
    },
  },
  {
    "MeanderingProgrammer/render-markdown.nvim",
    enabled = true,
    dependencies = { "nvim-treesitter/nvim-treesitter", "echasnovski/mini.nvim" },
    ---@module 'render-markdown'
    config = function()
      local colors = require("vesper.colors")

      -- Headings com cores do Vesper
      vim.cmd(string.format([[highlight Headline1Bg guifg=%s guibg=%s gui=bold]], colors.info, colors.bgDark))
      vim.cmd(string.format([[highlight Headline2Bg guifg=%s guibg=%s gui=bold]], colors.purple, colors.bgDark))
      vim.cmd(string.format([[highlight Headline3Bg guifg=%s guibg=%s gui=bold]], colors.primary, colors.bgDark))
      vim.cmd(string.format([[highlight Headline4Bg guifg=%s guibg=%s gui=bold]], colors.green, colors.bgDark))
      vim.cmd(string.format([[highlight Headline5Bg guifg=%s guibg=%s gui=bold]], colors.yellowDark, colors.bgDark))
      vim.cmd(string.format([[highlight Headline6Bg guifg=%s guibg=%s gui=bold]], colors.orange, colors.bgDark))

      -- Headings somente FG
      vim.cmd(string.format([[highlight Headline1Fg guifg=%s gui=bold]], colors.info))
      vim.cmd(string.format([[highlight Headline2Fg guifg=%s gui=bold]], colors.purple))
      vim.cmd(string.format([[highlight Headline3Fg guifg=%s gui=bold]], colors.primary))
      vim.cmd(string.format([[highlight Headline4Fg guifg=%s gui=bold]], colors.green))
      vim.cmd(string.format([[highlight Headline5Fg guifg=%s gui=bold]], colors.yellowDark))
      vim.cmd(string.format([[highlight Headline6Fg guifg=%s gui=bold]], colors.orange))

      -- Code Blocks
      vim.cmd(string.format([[highlight! RenderMarkdownCode guibg=%s ctermbg=NONE]], colors.bgDarker))
      vim.cmd(string.format([[highlight! RenderMarkdownCodeInline guibg=%s ctermbg=NONE]], colors.border))

      -- Lists
      vim.cmd(string.format([[highlight RenderMarkdownBullet guifg=%s gui=bold]], colors.info))
      vim.cmd(string.format([[highlight @markup.list.markdown guifg=%s]], colors.fg))

      -- Callouts
      vim.cmd(string.format([[highlight RenderMarkdownInfo guifg=%s guibg=%s gui=bold]], colors.info, colors.bgFloat))
      vim.cmd(string.format([[highlight RenderMarkdownHint guifg=%s guibg=%s gui=bold]], colors.green, colors.bgFloat))
      vim.cmd(
        string.format([[highlight RenderMarkdownWarn guifg=%s guibg=%s gui=bold]], colors.yellowDark, colors.bgFloat)
      )
      vim.cmd(string.format([[highlight RenderMarkdownError guifg=%s guibg=%s gui=bold]], colors.error, colors.bgFloat))
      vim.cmd(string.format([[highlight RenderMarkdownNote guifg=%s guibg=%s gui=bold]], colors.purple, colors.bgFloat))

      -- Links
      vim.cmd(string.format([[highlight RenderMarkdownLink guifg=%s gui=underline]], colors.primary))
      vim.cmd(string.format([[highlight @markup.link.markdown_inline guifg=%s gui=underline]], colors.primary))

      -- Quotes
      vim.cmd(string.format([[highlight RenderMarkdownQuote guifg=%s guibg=%s gui=italic]], colors.fg, colors.bgDark))
      vim.cmd(string.format([[highlight @markup.quote.markdown guifg=%s gui=italic]], colors.comment))

      -- Cabeçalho da Tabela
      vim.api.nvim_set_hl(0, "RenderMarkdownTableHead", {
        fg = colors.primary, -- texto mais claro
        bg = colors.bgDark, -- fundo mais escuro
        bold = true,
      })

      -- Linhas pares
      vim.api.nvim_set_hl(0, "RenderMarkdownTableRowEven", {
        bg = colors.bgDarker, -- sutilmente mais escuro
      })

      -- Linhas ímpares
      vim.api.nvim_set_hl(0, "RenderMarkdownTableRowOdd", {
        bg = colors.bg, -- mantém o fundo padrão
      })

      -- Checkboxes
      vim.cmd(string.format([[highlight RenderMarkdownChecked guifg=%s gui=bold]], colors.green))
      vim.cmd(string.format([[highlight RenderMarkdownUnchecked guifg=%s]], colors.comment))

      -- Separadores
      vim.cmd(string.format([[highlight RenderMarkdownDash guifg=%s]], colors.border))

      -- Texto forte e itálico
      vim.cmd(string.format([[highlight @markup.strong.markdown_inline guifg=%s gui=bold]], colors.secondary))
      vim.cmd(string.format([[highlight @markup.italic.markdown_inline guifg=%s gui=italic]], colors.purple))

      -- Math
      vim.cmd(string.format([[highlight RenderMarkdownMath guifg=%s guibg=%s]], colors.orange, colors.bgFloat))

      require("render-markdown").setup({
        heading = {
          -- render_modes = true,
          icons = { "󰬺  ", "󰬻  ", "󰬼  ", "󰬽  ", "󰬾  ", "󰬿  " },
          position = "inline",
          backgrounds = {
            "Headline1Bg",
            "Headline2Bg",
            "Headline3Bg",
            "Headline4Bg",
            "Headline5Bg",
            "Headline6Bg",
          },
          foregrounds = {
            "Headline1Fg",
            "Headline2Fg",
            "Headline3Fg",
            "Headline4Fg",
            "Headline5Fg",
            "Headline6Fg",
          },
        },
        code = {
          sign = false,
          border = "thin",
          position = "right",
          width = "block",
          above = "▁",
          below = "▔",
          language_left = "█",
          language_right = "█",
          language_border = "▁",
          left_pad = 1,
          right_pad = 1,
        },
        bullet = {
          enabled = true,
        },
        pipe_table = {
          preset = "round",
          alignment_indicator = "",
        },
        quote = { repeat_linebreak = true },
        link = {
          wiki = { icon = "󰇈 " },
          custom = {
            python = { pattern = "%.py$", icon = "󰌠 " },
            markdown = { pattern = "%.md$", icon = "󰍔 " },
          },
        },
        --checkbox = {
        --  enabled = true,
        --  position = "inline",
        --  unchecked = {
        --    icon = "   󰄱 ",
        --    highlight = "RenderMarkdownUnchecked",
        --    scope_highlight = nil,
        --  },
        --
        --  checked = {
        --    icon = "   󰱒 ",
        --    highlight = "RenderMarkdownChecked",
        --    scope_highlight = nil,
        --  },
        --},
        checkbox = {
          unchecked = { icon = "󰄱 " },
          checked = { icon = "󰄵 " },
          custom = { todo = { rendered = " " } },
        },
      })
    end,
  },
}
