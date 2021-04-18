local mapopts = { noremap = true, silent = true }

-- lsp {{{1
local function default_lsp_settings(_client, _bufnr)
  vim.api.nvim_buf_set_keymap(0, 'n', '<cr>',       '<cmd>lua vim.lsp.buf.definition()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'gd',         '<cmd>lua vim.lsp.buf.declaration()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', 'K',          '<cmd>lua vim.lsp.buf.hover()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<leader>jc', '<cmd>lua vim.lsp.buf.code_action()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<leader>jn', '<cmd>lua vim.lsp.buf.rename()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<leader>jr', '<cmd>lua vim.lsp.buf.references()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<leader>jt', '<cmd>lua vim.lsp.buf.type_definition()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<leader>js', '<cmd>lua vim.lsp.buf.signature_help()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', '<leader>jd', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics({show_header = false, border = "single"})<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', '[d',         '<cmd>lua vim.lsp.diagnostic.goto_prev()<cr>', mapopts)
  vim.api.nvim_buf_set_keymap(0, 'n', ']d',         '<cmd>lua vim.lsp.diagnostic.goto_next()<cr>', mapopts)

  vim.api.nvim_buf_set_option(0, 'omnifunc', 'v:lua.vim.lsp.omnifunc')
  -- if client.resolved_capabilities.document_formatting then
  --   buf_set_keymap("n", "<space>f", "<cmd>lua vim.lsp.buf.formatting()<CR>", opts)
  -- end
  -- if client.resolved_capabilities.document_range_formatting then
  --   buf_set_keymap("v", "<space>f", "<cmd>lua vim.lsp.buf.range_formatting()<CR>", opts)
  -- end
end

vim.fn.sign_define('LspDiagnosticsSignError',       {text = '', texthl = 'LspDiagnosticsSignError'})
vim.fn.sign_define('LspDiagnosticsSignWarning',     {text = '', texthl = 'LspDiagnosticsSignWarning'})
vim.fn.sign_define('LspDiagnosticsSignHint',        {text = '', texthl = 'LspDiagnosticsSignHint'})
vim.fn.sign_define('LspDiagnosticsSignInformation', {text = '', texthl = 'LspDiagnosticsSignInformation'})

vim.cmd('highlight! link FloatBorder NormalFloat')
vim.cmd('command! LspLog :lua vim.cmd("edit" .. vim.lsp.get_log_path())')

-- https://en.wikipedia.org/wiki/Box-drawing_character
vim.lsp.handlers['textDocument/hover'] = vim.lsp.with(
  vim.lsp.handlers.hover, {
    -- border = 'double'
    border = {
      {"╭", "NormalFloat"},
      {"─", "NormalFloat"},
      {"╮", "NormalFloat"},
      {"│", "NormalFloat"},
      {"╯", "NormalFloat"},
      {"─", "NormalFloat"},
      {"╰", "NormalFloat"},
      {"│", "NormalFloat"},
    }
  }
)

vim.lsp.handlers['textDocument/publishDiagnostics'] = vim.lsp.with(
  vim.lsp.diagnostic.on_publish_diagnostics, {
    signs = { priority = 11 },
    underline = true,
    virtual_text = true,
    update_in_insert = false,
  }
)

local lspconfig = require('lspconfig')

lspconfig.util.default_config = vim.tbl_extend('force', lspconfig.util.default_config, {
  autostart = false,
  on_attach = default_lsp_settings,
})

for _, v in pairs({'gopls', 'pyright', 'rust_analyzer'}) do
  lspconfig[v].setup({})
end

lspconfig.clangd.setup({
  cmd = {'/Library/Developer/CommandLineTools/usr/bin/clangd', '--background-index'},
  autostart = true,
})

local sumneko_root_path = '/Users/mhi/data/lua/lua-language-server'
local sumneko_bin = '/bin/macos/lua-language-server'

lspconfig.sumneko_lua.setup({
   cmd = {sumneko_root_path .. sumneko_bin, '-E', sumneko_root_path .. '/main.lua'},
   settings = {
      Lua = {
         runtime = {
            version = 'LuaJIT',
            -- path = vim.split(package.path, ';'),
         },
         workspace = {
            library = {
               -- ['/Users/mhi/data/github/neovim/neovim/runtime/lua'] = true,
               -- ['/Users/mhi/data/github/neovim/neovim/runtime/lua/vim/lsp'] = true,
               -- ['/Users/mhi/data/github/neovim/neovim/.deps/usr/share/lua/busted'] = true,
            },
            -- preloadFileSize = 50;
            -- ignoreDir = { 'data' }
         },
         diagnostics = {
            globals = {'vim', 'describe', 'it', 'before_each', 'after_each', 'pending',
                       'teardown'},
            -- disable = {'lowercase-global', 'unused-function'},
         },
         -- completion = {
         --    keywordSnippet = "Disable",
         -- },
      },
   },
})

-- treesitter {{{1
require('nvim-treesitter.configs').setup({
  highlight = {
    enable = {'c', 'lua', 'javascript', 'typescript'}
  },
  textobjects = {
    select = {
      enable = true
    }
  }
})

-- telescope {{{1
-- require('telescope').load_extension('spotlight')

vim.api.nvim_set_keymap('n', '<leader>ff', '<cmd>Telescope find_files<cr>', mapopts)
vim.api.nvim_set_keymap('n', '<leader>fg', '<cmd>Telescope live_grep<cr>', mapopts)
vim.api.nvim_set_keymap('n', '<leader>fb', '<cmd>Telescope buffers<cr>', mapopts)
vim.api.nvim_set_keymap('n', '<leader>fh', '<cmd>Telescope help_tags<cr>', mapopts)

-- hop {{{1
require('hop').setup({ teasing = false })

vim.api.nvim_set_keymap('n', '<space>',   '<cmd>HopChar2<cr>', mapopts)
vim.api.nvim_set_keymap('n', '<c-space>', '<cmd>HopWord<cr>', mapopts)

-- colorizer {{{1
if vim.o.termguicolors then
  require('colorizer').setup({'vim'})
end

-- vim: nowrap
