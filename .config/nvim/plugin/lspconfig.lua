vim.pack.add({
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/nvim-lua/plenary.nvim",
	"https://github.com/pmizio/typescript-tools.nvim",
	"https://github.com/aznhe21/actions-preview.nvim",
	"https://github.com/hasansujon786/nvim-navbuddy",
	"https://github.com/neovim/nvim-lspconfig",
	"https://github.com/SmiteshP/nvim-navic",
	"https://github.com/MunifTanjim/nui.nvim",
})

require("nvim-navbuddy").setup({
	lsp = {
		auto_attach = true
	}
})

require("nvim-navic").setup {
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

vim.opt.completeopt = { "menu", "menuone", "preinsert" }
vim.api.nvim_set_hl(0, 'ComplMatchIns', { link = 'Comment' })

require("typescript-tools").setup {
  settings = {
    separate_diagnostic_server = true,
    publish_diagnostic_on = "insert_leave",
    expose_as_code_action = {},
    tsserver_path = nil,
    tsserver_plugins = {},
    tsserver_max_memory = "auto",
    tsserver_format_options = {},
    tsserver_file_preferences = {},
    tsserver_locale = "en",
    complete_function_calls = false,
    include_completions_with_insert_text = true,
    code_lens = "off",
    disable_member_code_lens = true,
  },
}

local servers = {
	"pyright",
	"tailwindcss",
	"clangd",
	"gdscript",
	"nim_langserver",
	"lua_ls",
	"java_language_server",
	"csharp_ls",
	"sourcekit",
	"gleam",
	"zls",
	"gopls",
	"biome",
	"cssls",
	"superhtml",
	"eslint",
}

for _, lsp in pairs(servers) do
	vim.lsp.enable(lsp)
end
vim.lsp.config("lua_ls", {
	settings = {
		Lua = {
			diagnostics = {
				globals = { "vim" }
			}
		}
	}
})

local set_keymaps = function(client, bufnr)
	local bufopts = { noremap = true, silent = true, buffer = bufnr }
	if client:supports_method('textdocument/rename') then
		vim.keymap.set("n", "<leader>r", vim.lsp.buf.rename, bufopts)
	end

	if client:supports_method('textdocument/codeaction') then
		vim.keymap.set("n", "<leader>c", require("actions-preview").code_actions, bufopts)
	end
	if client:supports_method('textdocument/hover') then
		vim.keymap.set('n', 'K', require("pretty_hover").hover, bufopts)
	end

	if client:supports_method('textdocument/diagnostics') then
		vim.keymap.set('n', '<leader>di', vim.diagnostic.open_float)
		vim.keymap.set("n", "<leader>w", function()
			vim.diagnostic.jump({ count = vim.v.count1, float = false })
		end, bufopts)
		vim.keymap.set("n", "<leader>q", function()
			vim.diagnostic.jump({ count = -vim.v.count1, float = false })
		end, bufopts)
	end
	vim.keymap.set("n", "<leader>n", require("nvim-navbuddy").open, bufopts)
end

---For replacing certain <C-x>... keymaps.
---@param keys string
local function feedkeys(keys)
	vim.api.nvim_feedkeys(vim.api.nvim_replace_termcodes(keys, true, false, true), 'n', true)
end

---Is the completion menu open?
local function pumvisible()
	return tonumber(vim.fn.pumvisible()) ~= 0
end

local function lsp(args)
	local bufnr = args.buf
	local client = assert(vim.lsp.get_client_by_id(args.data.client_id))
	local function keymap(lhs, rhs, opts, mode)
		opts = type(opts) == 'string' and { desc = opts }
				or vim.tbl_extend('error', opts --[[@as table]], { buffer = bufnr })
		mode = mode or 'n'
		vim.keymap.set(mode, lhs, rhs, opts)
	end
	set_keymaps(client, bufnr)
	if client:supports_method('textdocument/completion') then
		vim.lsp.completion.enable(true, client.id, bufnr, { autotrigger = false })
		keymap('<cr>', function()
			return pumvisible() and '<C-y>' or '<cr>'
		end, { expr = true }, 'i')
		keymap('<C-j>', function()
			if pumvisible() then
				feedkeys '<C-n>'
			else
				if next(vim.lsp.get_clients { bufnr = 0 }) then
					vim.lsp.completion.trigger()
				else
					if vim.bo.omnifunc == '' then
						feedkeys '<C-x><C-n>'
					else
						feedkeys '<C-x><C-o>'
					end
				end
			end
		end, 'Trigger/select next completion', 'i')
		local has_words_before = function()
			unpack = unpack or table.unpack
			local line, col = unpack(vim.api.nvim_win_get_cursor(0))
			if col == 0 then
				return false
			end
			local line_content = vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]
			local char_before = line_content:sub(col, col)
			local trigger_pattern = '[%a%d_%.%:]'
			return char_before:match(trigger_pattern) ~= nil
		end
		keymap('<Tab>', function()
			if pumvisible() then
				feedkeys '<C-n>'
			elseif vim.snippet.active { direction = 1 } then
				vim.snippet.jump(1)
			elseif has_words_before() then
				vim.lsp.completion.get()
			else
				feedkeys '<Tab>'
			end
		end, {}, { 'i', 's' })
		keymap('<S-Tab>', function()
			if pumvisible() then
				feedkeys '<C-p>'
			elseif vim.snippet.active { direction = -1 } then
				vim.snippet.jump(-1)
			else
				feedkeys '<S-Tab>'
			end
		end, {}, { 'i', 's' })
		keymap('<BS>', '<C-o>s', {}, 's')
	end
end

vim.api.nvim_create_autocmd("LspAttach", {
	pattern = "*",
	callback = lsp,
})

