require("config.lazy")
require("config.options")
require("lazy").setup("plugins")
require("builtin")
vim.cmd [[
  set termguicolors
  colorscheme gruvbox-material
]]
