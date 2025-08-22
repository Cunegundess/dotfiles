return {
  'RRethy/base16-nvim',
  enabled = false,
  config = function()
    vim.cmd 'colorscheme base16-ayu-mirage'

    -- Get the base16 colors after setting the colorscheme
    local colors = require('base16-colorscheme').colors

    -- Map base16 colors to catppuccin-like names for easier reference
    local ayu_colors = {
      -- Background colors
      base = colors.base00, -- main background
      mantle = colors.base01, -- darker background for floating windows
      surface0 = colors.base02, -- surface color
      surface1 = colors.base03, -- lighter surface
      surface2 = colors.base04, -- even lighter surface

      -- Text colors
      text = colors.base05, -- main text
      subtext1 = colors.base04, -- dimmer text
      subtext0 = colors.base03, -- even dimmer text

      -- Overlay colors
      overlay0 = colors.base03,
      overlay1 = colors.base04,
      overlay2 = colors.base06,

      -- Accent colors
      red = colors.base08, -- red accent
      peach = colors.base09, -- orange/peach accent
      yellow = colors.base0A, -- yellow accent
      green = colors.base0B, -- green accent
      teal = colors.base0C, -- teal/cyan accent
      blue = colors.base0D, -- blue accent
      mauve = colors.base0E, -- purple/mauve accent
    }

    -- Apply all the custom highlight groups
    local highlights = {
      -- Completion menu styling
      Pmenu = { bg = ayu_colors.mantle, fg = ayu_colors.text },
      PmenuSel = { bg = ayu_colors.surface0, fg = ayu_colors.text },
      PmenuSbar = { bg = ayu_colors.surface0 },
      PmenuThumb = { bg = ayu_colors.surface2 },
      PmenuExtra = { bg = ayu_colors.mantle, fg = ayu_colors.subtext1 },

      -- Floating windows
      NormalFloat = { bg = ayu_colors.mantle },
      FloatBorder = { bg = ayu_colors.mantle, fg = ayu_colors.surface2 },
      FloatTitle = { bg = ayu_colors.mantle, fg = ayu_colors.text },

      -- Blink.cmp specific highlighting
      BlinkCmpMenu = { bg = ayu_colors.mantle, fg = ayu_colors.text },
      BlinkCmpMenuBorder = { bg = ayu_colors.mantle, fg = ayu_colors.surface2 },
      BlinkCmpMenuSelection = { bg = ayu_colors.surface0, fg = ayu_colors.text },
      BlinkCmpScrollBarThumb = { bg = ayu_colors.surface2 },
      BlinkCmpScrollBarGutter = { bg = ayu_colors.surface0 },
      BlinkCmpLabel = { bg = ayu_colors.mantle, fg = ayu_colors.text },
      BlinkCmpLabelDeprecated = { bg = ayu_colors.mantle, fg = ayu_colors.overlay0, strikethrough = true },
      BlinkCmpLabelDetail = { bg = ayu_colors.mantle, fg = ayu_colors.subtext1 },
      BlinkCmpLabelDescription = { bg = ayu_colors.mantle, fg = ayu_colors.subtext1 },
      BlinkCmpKind = { bg = ayu_colors.mantle, fg = ayu_colors.peach },
      BlinkCmpSource = { bg = ayu_colors.mantle, fg = ayu_colors.overlay1 },
      BlinkCmpGhostText = { fg = ayu_colors.overlay0, italic = true },
      BlinkCmpDoc = { bg = ayu_colors.mantle, fg = ayu_colors.text },
      BlinkCmpDocBorder = { bg = ayu_colors.mantle, fg = ayu_colors.surface2 },
      BlinkCmpDocSeparator = { bg = ayu_colors.mantle, fg = ayu_colors.surface1 },
      BlinkCmpDocCursorLine = { bg = ayu_colors.surface0 },
      BlinkCmpSignatureHelp = { bg = ayu_colors.mantle, fg = ayu_colors.text },
      BlinkCmpSignatureHelpBorder = { bg = ayu_colors.mantle, fg = ayu_colors.surface2 },
      BlinkCmpSignatureHelpActiveParameter = { bg = ayu_colors.surface0, fg = ayu_colors.peach, bold = true },

      -- Snacks.nvim picker NvChad style
      SnacksPicker = { bg = ayu_colors.base },
      SnacksPickerBorder = { fg = ayu_colors.surface0, bg = ayu_colors.base },
      SnacksPickerPreview = { bg = ayu_colors.base },
      SnacksPickerPreviewBorder = { fg = ayu_colors.base, bg = ayu_colors.base },
      SnacksPickerPreviewTitle = { fg = ayu_colors.base, bg = ayu_colors.green },
      SnacksPickerBoxBorder = { fg = ayu_colors.base, bg = ayu_colors.base },
      SnacksPickerInputBorder = { fg = ayu_colors.surface2, bg = ayu_colors.base },
      SnacksPickerInputSearch = { fg = ayu_colors.text, bg = ayu_colors.base },
      SnacksPickerList = { bg = ayu_colors.base },
      SnacksPickerListBorder = { fg = ayu_colors.base, bg = ayu_colors.base },
      SnacksPickerListTitle = { fg = ayu_colors.base, bg = ayu_colors.base },

      -- Additional picker elements
      SnacksPickerDir = { fg = ayu_colors.blue },
      SnacksPickerFile = { fg = ayu_colors.text },
      SnacksPickerMatch = { fg = ayu_colors.peach, bold = true },
      SnacksPickerCursor = { bg = ayu_colors.surface0, fg = ayu_colors.text },
      SnacksPickerSelected = { bg = ayu_colors.surface0, fg = ayu_colors.text },
      SnacksPickerIcon = { fg = ayu_colors.blue },
      SnacksPickerSource = { fg = ayu_colors.overlay1 },
      SnacksPickerCount = { fg = ayu_colors.overlay1 },
      SnacksPickerFooter = { fg = ayu_colors.overlay1 },
      SnacksPickerHeader = { fg = ayu_colors.text, bold = true },
      SnacksPickerSpecial = { fg = ayu_colors.peach },
      SnacksPickerIndent = { fg = ayu_colors.surface1 },
      SnacksPickerMulti = { fg = ayu_colors.peach },
      SnacksPickerTitle = { fg = ayu_colors.text, bold = true },
      SnacksPickerPrompt = { fg = ayu_colors.text },

      -- Snacks core components
      SnacksNotifierNormal = { bg = ayu_colors.mantle, fg = ayu_colors.text },
      SnacksNotifierBorder = { bg = ayu_colors.mantle, fg = ayu_colors.surface2 },
      SnacksNotifierTitle = { bg = ayu_colors.mantle, fg = ayu_colors.text, bold = true },
      SnacksNotifierIcon = { bg = ayu_colors.mantle, fg = ayu_colors.blue },
      SnacksNotifierIconInfo = { bg = ayu_colors.mantle, fg = ayu_colors.blue },
      SnacksNotifierIconWarn = { bg = ayu_colors.mantle, fg = ayu_colors.yellow },
      SnacksNotifierIconError = { bg = ayu_colors.mantle, fg = ayu_colors.red },

      -- Snacks Dashboard
      SnacksDashboardNormal = { bg = ayu_colors.base, fg = ayu_colors.text },
      SnacksDashboardDesc = { bg = ayu_colors.base, fg = ayu_colors.subtext1 },
      SnacksDashboardFile = { bg = ayu_colors.base, fg = ayu_colors.text },
      SnacksDashboardDir = { bg = ayu_colors.base, fg = ayu_colors.blue },
      SnacksDashboardFooter = { bg = ayu_colors.base, fg = ayu_colors.overlay1 },
      SnacksDashboardHeader = { bg = ayu_colors.base, fg = ayu_colors.text, bold = true },
      SnacksDashboardIcon = { bg = ayu_colors.base, fg = ayu_colors.blue },
      SnacksDashboardKey = { bg = ayu_colors.base, fg = ayu_colors.peach },
      SnacksDashboardTerminal = { bg = ayu_colors.base, fg = ayu_colors.text },
      SnacksDashboardSpecial = { bg = ayu_colors.base, fg = ayu_colors.peach },

      -- Snacks Terminal
      SnacksTerminalNormal = { bg = ayu_colors.mantle, fg = ayu_colors.text },
      SnacksTerminalBorder = { bg = ayu_colors.mantle, fg = ayu_colors.surface2 },
      SnacksTerminalTitle = { bg = ayu_colors.mantle, fg = ayu_colors.text, bold = true },

      -- Other UI elements
      CmpItemMenu = { fg = ayu_colors.surface2 },
      CursorLineNr = { fg = ayu_colors.text },
      GitSignsChange = { fg = ayu_colors.peach },
      LineNr = { fg = ayu_colors.overlay0 },
      LspInfoBorder = { link = 'FloatBorder' },
      VertSplit = { bg = ayu_colors.base, fg = ayu_colors.surface0 },
      WhichKeyFloat = { bg = ayu_colors.mantle },
      YankHighlight = { bg = ayu_colors.surface2 },
      FidgetTask = { fg = ayu_colors.subtext1 },
      FidgetTitle = { fg = ayu_colors.peach },

      -- Indent guides
      IblIndent = { fg = ayu_colors.surface0 },
      IblScope = { fg = ayu_colors.overlay0 },

      -- Syntax highlighting
      Boolean = { fg = ayu_colors.mauve },
      Number = { fg = ayu_colors.mauve },
      Float = { fg = ayu_colors.mauve },

      PreProc = { fg = ayu_colors.mauve },
      PreCondit = { fg = ayu_colors.mauve },
      Include = { fg = ayu_colors.mauve },
      Define = { fg = ayu_colors.mauve },
      Conditional = { fg = ayu_colors.red },
      Repeat = { fg = ayu_colors.red },
      Keyword = { fg = ayu_colors.red },
      Typedef = { fg = ayu_colors.red },
      Exception = { fg = ayu_colors.red },
      Statement = { fg = ayu_colors.red },

      Error = { fg = ayu_colors.red },
      StorageClass = { fg = ayu_colors.peach },
      Tag = { fg = ayu_colors.peach },
      Label = { fg = ayu_colors.peach },
      Structure = { fg = ayu_colors.peach },
      Operator = { fg = ayu_colors.peach },
      Title = { fg = ayu_colors.peach },
      Special = { fg = ayu_colors.yellow },
      SpecialChar = { fg = ayu_colors.yellow },
      Type = { fg = ayu_colors.yellow, bold = true },
      Function = { fg = ayu_colors.green, bold = true },
      Delimiter = { fg = ayu_colors.subtext1 },
      Ignore = { fg = ayu_colors.subtext1 },
      Macro = { fg = ayu_colors.teal },

      -- Treesitter highlights
      TSAnnotation = { fg = ayu_colors.mauve },
      TSAttribute = { fg = ayu_colors.mauve },
      TSBoolean = { fg = ayu_colors.mauve },
      TSCharacter = { fg = ayu_colors.teal },
      TSCharacterSpecial = { link = 'SpecialChar' },
      TSComment = { link = 'Comment' },
      TSConditional = { fg = ayu_colors.red },
      TSConstBuiltin = { fg = ayu_colors.mauve },
      TSConstMacro = { fg = ayu_colors.mauve },
      TSConstant = { fg = ayu_colors.text },
      TSConstructor = { fg = ayu_colors.green },
      TSDebug = { link = 'Debug' },
      TSDefine = { link = 'Define' },
      TSEnvironment = { link = 'Macro' },
      TSEnvironmentName = { link = 'Type' },
      TSError = { link = 'Error' },
      TSException = { fg = ayu_colors.red },
      TSField = { fg = ayu_colors.blue },
      TSFloat = { fg = ayu_colors.mauve },
      TSFuncBuiltin = { fg = ayu_colors.green },
      TSFuncMacro = { fg = ayu_colors.green },
      TSFunction = { fg = ayu_colors.green },
      TSFunctionCall = { fg = ayu_colors.green },
      TSInclude = { fg = ayu_colors.red },
      TSKeyword = { fg = ayu_colors.red },
      TSKeywordFunction = { fg = ayu_colors.red },
      TSKeywordOperator = { fg = ayu_colors.peach },
      TSKeywordReturn = { fg = ayu_colors.red },
      TSLabel = { fg = ayu_colors.peach },
      TSLiteral = { link = 'String' },
      TSMath = { fg = ayu_colors.blue },
      TSMethod = { fg = ayu_colors.green },
      TSMethodCall = { fg = ayu_colors.green },
      TSNamespace = { fg = ayu_colors.yellow },
      TSNone = { fg = ayu_colors.text },
      TSNumber = { fg = ayu_colors.mauve },
      TSOperator = { fg = ayu_colors.peach },
      TSParameter = { fg = ayu_colors.text },
      TSParameterReference = { fg = ayu_colors.text },
      TSPreProc = { link = 'PreProc' },
      TSProperty = { fg = ayu_colors.blue },
      TSPunctBracket = { fg = ayu_colors.text },
      TSPunctDelimiter = { link = 'Delimiter' },
      TSPunctSpecial = { fg = ayu_colors.blue },
      TSRepeat = { fg = ayu_colors.red },
      TSStorageClass = { fg = ayu_colors.peach },
      TSStorageClassLifetime = { fg = ayu_colors.peach },
      TSStrike = { fg = ayu_colors.subtext1 },
      TSString = { fg = ayu_colors.teal },
      TSStringEscape = { fg = ayu_colors.green },
      TSStringRegex = { fg = ayu_colors.green },
      TSStringSpecial = { link = 'SpecialChar' },
      TSSymbol = { fg = ayu_colors.text },
      TSTag = { fg = ayu_colors.peach },
      TSTagAttribute = { fg = ayu_colors.green },
      TSTagDelimiter = { fg = ayu_colors.green },
      TSText = { fg = ayu_colors.green },
      TSTextReference = { link = 'Constant' },
      TSTitle = { link = 'Title' },
      TSTodo = { link = 'Todo' },
      TSType = { fg = ayu_colors.yellow, bold = true },
      TSTypeBuiltin = { fg = ayu_colors.yellow, bold = true },
      TSTypeDefinition = { fg = ayu_colors.yellow, bold = true },
      TSTypeQualifier = { fg = ayu_colors.peach, bold = true },
      TSURI = { fg = ayu_colors.blue },
      TSVariable = { fg = ayu_colors.text },
      TSVariableBuiltin = { fg = ayu_colors.mauve },
    }

    -- Apply all highlights
    for group, opts in pairs(highlights) do
      vim.api.nvim_set_hl(0, group, opts)
    end
  end,
}
