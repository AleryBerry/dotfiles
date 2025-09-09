vim.pack.add({
	"https://github.com/nvim-lualine/lualine.nvim",
	"https://github.com/SmiteshP/nvim-navic",
})
local navic = require("nvim-navic")
require("nvim-navbuddy").setup({
	lsp = {
		auto_attach = true
	}
})
navic.setup {
	icons = {
		File          = "󰈙 ",
		Module        = " ",
		Namespace     = "󰌗 ",
		Package       = " ",
		Class         = "󰌗 ",
		Method        = "󰆧 ",
		Property      = " ",
		Field         = " ",
		Constructor   = " ",
		Enum          = "󰕘",
		Interface     = "󰕘",
		Function      = "󰊕 ",
		Variable      = "󰆧 ",
		Constant      = "󰏿 ",
		String        = "󰀬 ",
		Number        = "󰎠 ",
		Boolean       = "◩ ",
		Array         = "󰅪 ",
		Object        = "󰅩 ",
		Key           = "󰌋 ",
		Null          = "󰟢 ",
		EnumMember    = " ",
		Struct        = "󰌗 ",
		Event         = " ",
		Operator      = "󰆕 ",
		TypeParameter = "󰊄 ",
	},
	lsp = {
		auto_attach = true,
		preference = nil,
	},
	highlight = true,
	separator = " > ",
	depth_limit = 0,
	depth_limit_indicator = "..",
	safe_output = true,
	lazy_update_context = false,
	click = false,
	format_text = function(text)
		return text
	end,
}

require('lualine').setup({
	options = {
		icons_enabled = true,
		theme = 'auto',
		component_separators = { left = '', right = '' },
		section_separators = { left = '', right = '' },
		disabled_filetypes = {
			statusline = {},
			winbar = {},
		},
		ignore_focus = {},
		always_divide_middle = true,
		always_show_tabline = true,
		globalstatus = false,
		refresh = {
			statusline = 1000,
			tabline = 1000,
			winbar = 1000,
			refresh_time = 16,
			events = {
				'WinEnter',
				'BufEnter',
				'BufWritePost',
				'SessionLoadPost',
				'FileChangedShellPost',
				'VimResized',
				'Filetype',
				'CursorMoved',
				'CursorMovedI',
				'ModeChanged',
			},
		}
	},
	sections = {
		lualine_a = { 'mode' },
		lualine_b = { 'branch', 'diff', 'diagnostics' },
		lualine_c = { {
			function()
				return navic.get_location()
			end,
			cond = function()
				return navic.is_available()
			end
		} },
		lualine_x = {
			'filename', 'filetype' },
		lualine_y = {},
		lualine_z = { 'location' },
	},
	inactive_sections = {
		lualine_a = {},
		lualine_b = {},
		lualine_c = {},
		lualine_x = {},
		lualine_y = {},
		lualine_z = {}
	},
	tabline = {},
	winbar = {},
	inactive_winbar = {},
	extensions = {}
})
