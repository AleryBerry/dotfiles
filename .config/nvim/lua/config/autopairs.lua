require('nvim-autopairs').setup({
	enable_check_bracket_line = false,
	check_ts = true,
	fast_wrap = {
		map = '<C-e>',
		chars = { '{', '[', '(', '"', "'", "`" },
		pattern = string.gsub([[ [%'%"%)%>%]%)%}%,] ]], '%s+', ''),
		end_key = '$',
		keys = 'qwertyuiopzxcvbnmasdfghjkl',
		check_comma = true,
		highlight = 'Search',
		highlight_grey = 'Comment'
	}
})


