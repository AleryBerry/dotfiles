local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.superhtml = {
  install_info = {
    url = "~/Git/superhtml/tree-sitter-superhtml", -- local path or git repo
    files = { "src/parser.c" },                    -- note that some parsers also require src/scanner.c or src/scanner.cc
  },
  filetype = "html",                               -- if filetype does not match the parser name
}
vim.treesitter.language.register('superhtml', 'html')
