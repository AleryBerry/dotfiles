require("flutter-tools").setup({
  root_patterns = { ".git", "pubspec.yaml" }, -- patterns to find the root of your flutter project
  widget_guides = {
    enabled = true,
  },
  closing_tags = {
    -- highlight = "ErrorMsg", -- highlight for the closing tag
    prefix = "", -- character to use for close tag e.g. > Widget
    priority = 10, -- priority of virtual text in current line
    -- consider to configure this when there is a possibility of multiple virtual text items in one line
    -- see `priority` option in |:help nvim_buf_set_extmark| for more info
    enabled = true -- set to false to disable
  },
  lsp = {
    color = { -- show the derived colours for dart variables
      enabled = true, -- whether or not to highlight color variables at all, only supported on flutter >= 2.10
      background = true, -- highlight the background
      background_color = { r = 19, g = 17, b = 24 }, -- required, when background is transparent (i.e. background_color = { r = 19, g = 17, b = 24},)
      foreground = false, -- highlight the foreground
      virtual_text = false, -- show the highlight using virtual text
      virtual_text_str = "■", -- the virtual text character to highlight
    },
    -- see the link below for details on each option:
    -- https://github.com/dart-lang/sdk/blob/master/pkg/analysis_server/tool/lsp_spec/README.md#client-workspace-configuration
    settings = {
      showTodos = true,
      completeFunctionCalls = true,
      renameFilesWithClasses = "always", -- "always"
      enableSnippets = true,
      updateImportsOnRename = true,      -- Whether to update imports and other directives when files are renamed. Required for `FlutterRename` command.
    },
  },
})
