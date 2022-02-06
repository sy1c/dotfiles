-- install packer
local install_path = vim.fn.stdpath "data" .. "/site/pack/packer/start/packer.nvim"
if vim.fn.empty(vim.fn.glob(install_path)) > 0 then
    PACKER_BOOTSTRAP = vim.fn.system {
        "git",
        "clone",
        "--depth",
        "1",
        "https://github.com/wbthomason/packer.nvim",
        install_path,
    }
    print "Installing Packer..."
    vim.cmd [[packadd packer.nvim]]
end

-- reload neovim after saving the plugins file
vim.cmd [[
    augroup packer_user_config
        autocmd!
        autocmd BufWritePost plugins.lua source <afile> | PackerSync
    augroup end
]]

-- error handler
local status_ok, packer = pcall(require, "packer")
if not status_ok then
    return
end

-- plugins
return packer.startup(function(use)
    use "wbthomason/packer.nvim"
    use "nvim-lua/popup.nvim"
    use "nvim-lua/plenary.nvim"
    
    use "arcticicestudio/nord-vim"

    use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
    }

    -- automatically set up your configuration after cloning packer.nvim
    -- put this at the end after all plugins
    if PACKER_BOOTSTRAP then
        require("packer").sync()
    end
end)
