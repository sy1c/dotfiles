local ensure_packer = function()
    local fn = vim.fn
    local install_path = fn.stdpath('data')..'/site/pack/packer/start/packer.nvim'
    if fn.empty(fn.glob(install_path)) > 0 then
        fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
        vim.cmd [[packadd packer.nvim]]
        return true
    end
    return false
end

local packer_bootstrap = ensure_packer()

return require('packer').startup(function(use)
    use 'wbthomason/packer.nvim'                            -- Plugin manager
    use 'kyazdani42/nvim-tree.lua'                          -- File explorer
    use 'nvim-lualine/lualine.nvim'                         -- Statusline
    use {                                                   -- Fuzzy finder
        'nvim-telescope/telescope.nvim', branch = '0.1.x',
        requires = {
            'nvim-lua/plenary.nvim'
        },
    }
    use 'neovim/nvim-lspconfig'                             -- LSP
    use {                                                   -- Autocompletion
        'hrsh7th/nvim-cmp',
        requires = {
            'hrsh7th/cmp-cmdline',
            'hrsh7th/cmp-path',
            'hrsh7th/cmp-buffer',
            'hrsh7th/cmp-nvim-lsp',
            'L3MON4D3/LuaSnip',
            'saadparwaiz1/cmp_luasnip'
        },
    }
    use 'windwp/nvim-autopairs'                             -- Autopair plugin
    use {                                                   -- Highlight, edit and navigate code
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }
    use 'lewis6991/gitsigns.nvim'                           -- Git decorations
    use 'norcalli/nvim-colorizer.lua'                       -- Color highlighter
    use 'kyazdani42/nvim-web-devicons'                      -- Icons for plugins
    use 'arcticicestudio/nord-vim'                          -- Colorscheme

    -- Automatically set up your configuration after cloning packer.nvim
    -- Put this at the end after all plugins
    if packer_bootstrap then
        require('packer').sync()
    end
end)
