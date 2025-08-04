vim.pack.add({
  "https://github.com/ggandor/leap.nvim",
  "https://github.com/ggandor/flit.nvim",
})

require('leap').set_default_mappings()
require('leap').opts.preview_filter =
  function (ch0, ch1, ch2)
    return not (
      ch1:match('%s') or
      ch0:match('%a') and ch1:match('%a') and ch2:match('%a')
    )
  end
require('leap').opts.equivalence_classes = { ' \t\r\n', '([{', ')]}', '\'"`' }
require('leap.user').set_repeat_keys('<enter>', '<backspace>')
require('leap').opts.safe_labels = {}
require('leap').opts.preview_filter = function () return false end
vim.api.nvim_set_hl(0, 'LeapBackdrop', { link = 'Comment' })
