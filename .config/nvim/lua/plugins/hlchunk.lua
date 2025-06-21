local config = function()
  require("hlchunk").setup({
    chunk = {
      enable = true,
      use_treesitter = true,
      textobject = "ic"
    },
    indent = {
      enable = true
    }
  })
end 

return {
  {
    "shellRaining/hlchunk.nvim",
    event = { "BufReadPre", "BufNewFile" },
    config = config,
  },
}
