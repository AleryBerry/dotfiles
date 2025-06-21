return {
  "folke/snacks.nvim",
  ---@type snacks.Config
  opts = {
    dashboard= {
      autokeys = "12345asdfqwert",
      formats = {
        key = function(item)
          return { { "[", hl = "special" }, { item.key, hl = "key" }, { "]", hl = "special" } }
        end,
      },
      sections = {
        { section = "terminal", cmd = "fortune -s | cowsay", hl = "header", padding = 1, indent = 8 },
        { icon = " ", title = "Projects", section = "projects", indent = 2, padding = 2 },
        { icon = " ", title = "Recent Files", section = "recent_files", indent = 2, padding = 2 },
      },
    }
  }
}
