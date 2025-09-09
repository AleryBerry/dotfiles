vim.pack.add({
	"https://github.com/ggandor/leap.nvim",
	"https://github.com/gbprod/substitute.nvim",
	"https://github.com/gbprod/yanky.nvim",
})

require("substitute").setup({
  on_substitute = require("yanky.integration").substitute(),
})
require('leap').opts.equivalence_classes = { ' \t\r\n', '([{', ')]}', '\'"`' }
require('leap').opts.preview_filter = function() return false end
vim.api.nvim_set_hl(0, 'LeapBackdrop', { link = 'Comment' })

do
	-- Returns an argument table for `leap()`, tailored for f/t-motions.
	local function as_ft(key_specific_args)
		local common_args = {
			inputlen = 1,
			inclusive_op = true,
			-- To limit search scope to the current line:
			-- pattern = function (pat) return '\\%.l'..pat end,
			opts = {
				labels = {},                                           -- force autojump
				safe_labels = vim.fn.mode(1):match('o') and {} or nil, -- [1]
				case_sensitive = true,                                 -- [2]
			},
		}
		return vim.tbl_deep_extend('keep', common_args, key_specific_args)
	end

	local clever = require('leap.user').with_traversal_keys -- [3]
	local clever_f = clever('f', 'F')
	local clever_t = clever('t', 'T')

	for key, args in pairs {
		f = { opts = clever_f, },
		F = { backward = true, opts = clever_f },
		t = { offset = -1, opts = clever_t },
		T = { backward = true, offset = 1, opts = clever_t },
	} do
		vim.keymap.set({ 'n', 'x', 'o' }, key, function()
			require('leap').leap(as_ft(args))
		end)
	end
end
vim.keymap.set({ 'n', 'x', 'o' }, 'gs', require('leap.remote').action)
vim.keymap.set({ 'n', 'x', 'o' }, 's',
	function()
		require('leap').leap {
			target_windows = { vim.api.nvim_get_current_win() },
			inclusive_op = true
		}
	end)
