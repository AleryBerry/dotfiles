require('snacks').setup({
  terminal = {
    interactive = false,
    win = {
      show = true,
      fixbuf = true,
      relative = "editor",
      position = "float",
      minimal = true,
      wo = { },
      bo = {},
      keys = {
        term_normal = {
          "<esc>",
          function()
            return "<C-\\><C-n>"
          end,
          mode = "t",
          expr = true,
          desc = "Double escape to normal mode",
        },
        q = "hide",
        ["<esc>"] = "hide",
      },
    },
  },
  indent = {
    enabled = false
  },
  picker = {
    enabled = true,
    auto_close = true,
    actions = {
      delete_projects = function(picker, _)
        Snacks.picker.actions.close(picker)
        local items = picker:selected({ fallback = true })

        local data_dir = vim.fn.stdpath("data") .. "/snacks"
        local projects_file = data_dir .. "/custom_projects.json"

        -- Read the projects file
        local file = io.open(projects_file, "r")
        local projects
        if file then
          local data = file:read("*a")
          file:close()
          projects = vim.fn.json_decode(data)
        else
          projects = {}
        end

        -- Remove the selected projects
        local new_projects = {}
        for _, project in ipairs(projects) do
          local found = false
          for _, item in ipairs(items) do
            if project == item.file then
              found = true
              break
            end
          end
          if not found then
            table.insert(new_projects, project)
          end
        end

        -- Write the projects file
        local file = io.open(projects_file, "w")
        if not file then
          print("Error opening projects file for writing.")
          return
        end
        file:write(vim.fn.json_encode(new_projects))
        file:close()
      end,
    },
    sources = {
      explorer = {
        auto_close = true
      },
      projects = {
        win = {
          input = {
            keys = {
              ["<C-x>"] = { "delete_projects", mode = { "n", "i" } },
            },
          },
        },
      },
    },
  },
})
-- Add current pwd to projects
local add_project = function()
  local path = vim.fn.getcwd()
  local data_dir = vim.fn.stdpath("data") .. "/snacks"
  local projects_file = data_dir .. "/custom_projects.json"

  -- Create the data directory if it doesn't exist
  if vim.fn.isdirectory(data_dir) == 0 then
    vim.fn.mkdir(data_dir, "p")
  end

  -- Read the projects file
  local file = io.open(projects_file, "r")
  local projects
  if file then
    local data = file:read("*a")
    file:close()
    projects = vim.fn.json_decode(data)
  else
    projects = {}
  end

  -- Add the new project
  table.insert(projects, path)

  -- Write the projects file
  local file = io.open(projects_file, "w")
  if not file then
    print("Error opening projects file for writing.")
    return
  end
  file:write(vim.fn.json_encode(projects))
  file:close()
end


local projects_config = {
  finder = "recent_projects",
  format = "file",
  dev = { "~/dev", "~/Projects" },
  confirm = "load_session",
  recent = true,
  matcher = {
    frecency = true,   -- use frecency boosting
    sort_empty = true, -- sort even when the filter is empty
    cwd_bonus = false,
  },
  sort = { fields = { "score:desc", "idx" } },
  win = {
    preview = { minimal = true },
    input = {
      keys = {
        -- every action will always first change the cwd of the current tabpage to the project
        ["<c-e>"] = { { "tcd", "picker_explorer" }, mode = { "n", "i" } },
        ["<c-f>"] = { { "tcd", "picker_files" }, mode = { "n", "i" } },
        ["<c-g>"] = { { "tcd", "picker_grep" }, mode = { "n", "i" } },
        ["<c-r>"] = { { "tcd", "picker_recent" }, mode = { "n", "i" } },
        ["<c-w>"] = { { "tcd" }, mode = { "n", "i" } },
      },
    },
  },
}

vim.keymap.set("n", "<leader>p", function()
  local data_dir = vim.fn.stdpath("data") .. "/snacks"
  local projects_file = data_dir .. "/custom_projects.json"

  -- Read the projects file
  local file = io.open(projects_file, "r")
  local custom_projects
  if file then
    local data = file:read("*a")
    file:close()
    custom_projects = vim.fn.json_decode(data)
  else
    custom_projects = {}
  end

  projects_config.projects = custom_projects
  Snacks.picker.projects(projects_config)
end, { silent = true })

vim.keymap.set("n", ",p", function() Snacks.picker.yanky() end, { silent = true })
vim.keymap.set("n", "<leader>f", Snacks.picker.files, { silent = true })
vim.keymap.set("n", "<leader>g", Snacks.picker.grep, { silent = true })
vim.keymap.set("n", "<leader>e", function() Snacks.explorer() end, { silent = true })
vim.keymap.set("n", "<leader>t", function() Snacks.terminal() end, { silent = true })
vim.keymap.set("n", "<leader>b", Snacks.picker.buffers, { silent = true })
vim.keymap.set("n", "<leader>d", Snacks.picker.diagnostics, { silent = true })
vim.keymap.set("n", ",r", Snacks.picker.lsp_references, { silent = true })
vim.keymap.set("n", "<leader>z", Snacks.picker.zoxide, { silent = true })
vim.keymap.set("n", "<leader>ad", add_project, { silent = true })
