-- -------
-- Library
-- -------

function map (mode, shortcut, command)
vim.api.nvim_set_keymap(mode, shortcut, command, { noremap = true, silent = true })
end
function nmap(shortcut, command)
map('n', shortcut, command)
end
function imap(shortcut, command)
map('i', shortcut, command)
end

-- ------
-- Config
-- ------

vim.cmd([[
set nobackup
set number
set termguicolors " 24-bit colors
" let g:tokyonight_style = "day"
" let g:tokyonight_italic_functions = 1
" colorscheme tokyonight
" colorscheme sonokai
"colorscheme PaperColor
]])

vim.g.mapleader = ' '

vim.g.auto_refresh_enabled = false

function ToggleAutoRefresh()
  if vim.g.auto_refresh_enabled then
    vim.api.nvim_clear_autocmds({ group = "AutoRefresh" })
    vim.g.auto_refresh_enabled = false
    print("Auto refresh disabled")
  else
    vim.o.autoread = true
    vim.api.nvim_create_augroup("AutoRefresh", { clear = true })
    vim.api.nvim_create_autocmd({ "FocusGained", "BufEnter" }, {
      group = "AutoRefresh",
      command = "if mode() != 'c' | checktime | endif",
      pattern = "*",
    })
    vim.g.auto_refresh_enabled = true
    print("Auto refresh enabled")
  end
end

vim.keymap.set('n', '<leader>ar', ToggleAutoRefresh, {
  noremap = true,
  silent = false,
  desc = "Toggle auto refresh of files",
})

-- Enable auto refresh by default
ToggleAutoRefresh()

vim.g.auto_save_enabled = false

function ToggleAutoSave()
  if vim.g.auto_save_enabled then
    vim.api.nvim_clear_autocmds({ group = "AutoSave" })
    vim.g.auto_save_enabled = false
    print("Auto save disabled")
  else
    vim.api.nvim_create_augroup("AutoSave", { clear = true })
    vim.api.nvim_create_autocmd({ "TextChanged", "InsertLeave", "FocusLost" }, {
      group = "AutoSave",
      pattern = "*",
      callback = function()
        if vim.bo.modified and vim.bo.buftype == "" and vim.fn.expand("%") ~= "" then
          vim.cmd("silent! write")
        end
      end,
    })
    vim.g.auto_save_enabled = true
    print("Auto save enabled")
  end
end

vim.keymap.set('n', '<leader>as', ToggleAutoSave, {
  noremap = true,
  silent = false,
  desc = "Toggle auto save of files",
})

-- Enable auto save by default
ToggleAutoSave()

vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true

