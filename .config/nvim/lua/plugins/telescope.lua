local init = function()
  local builtin = require('telescope.builtin')
  vim.keymap.set('n', '<leader>/', builtin.live_grep, { desc = 'Telescope live grep' })
  vim.keymap.set("n", "<leader>f", builtin.find_files, { desc = 'Telescope find files' })
  vim.keymap.set('n', '<leader>p', ":Telescope projects<CR>", {})
end

return {
  'nvim-telescope/telescope.nvim',
  tag = '0.1.8',
  dependencies = { 'nvim-lua/plenary.nvim' },
  config = function()
    require('telescope').setup {
      pickers = {
        find_files = {
          find_command = { 'rg', '--one-file-system', '--files', '--iglob', '!.git', '--hidden' },
        },
        grep_string = {
          additional_args = { '--one-file-system', '--iglob', '!.git', '--hidden' },
        },
        live_grep = {
          additional_args = { '--one-file-system', '--iglob', '!.git', '--hidden' },
        },
      },
    }
  end,
  init = init,
}
