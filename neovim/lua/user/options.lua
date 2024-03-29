vim.opt.backup = false                                      	-- create a backup file 
vim.opt.clipboard = 'unnamedplus'                           	-- allow neovim to access the system clipboard
vim.opt.cmdheight = 1                                       	-- number of lines for the command line
vim.opt.completeopt = { 'menuone', 'noinsert', 'noselect' } 	-- modifie the auto-complete menu
vim.opt.cursorline = true                                   	-- highlight the current line
vim.opt.expandtab = true                                    	-- convert tabs to spaces
vim.opt.fileencoding = 'utf-8'                              	-- file-content encoding
vim.opt.hlsearch = true                                     	-- highlight all matches on previous search pattern
vim.opt.laststatus = 3                                          -- only one statusline with multiple windows
vim.opt.ignorecase = true                                  	-- ignore case in search pattern
vim.opt.mouse = 'a'                                         	-- allow the mouse to be used in the editor
vim.opt.number = true                                       	-- show line numbers
vim.opt.numberwidth = 4                                     	-- number column width
vim.opt.pumheight = 8                                       	-- pop up menu height
vim.opt.relativenumber = false                              	-- show relative line numbers
vim.opt.scrolloff = 8                                       	-- lines to keep above and below the cursor
vim.opt.shiftwidth = 4                                      	-- spaces inserted for indentation
vim.opt.showmode = false                                     	-- show mode in the command line
vim.opt.showtabline = 2                                     	-- show the line with tab page labels
vim.opt.sidescrolloff = 8                                   	-- columns to keep to the left and right of the cursor
vim.opt.signcolumn = 'yes'                                  	-- show sign column
vim.opt.smartcase = true                                    	-- smart case
vim.opt.smartindent = true                                  	-- autoindenting 
vim.opt.splitbelow = true                                   	-- the new window below the current one 
vim.opt.splitright = true                                   	-- the new window right of the current one
vim.opt.swapfile = false                                    	-- create a swap file
vim.opt.tabstop = 4                                         	-- spaces inserted for a tab
vim.opt.termguicolors = true                                	-- enable 24-bit rgb color
vim.opt.timeoutlen = 500                                    	-- time to wait for a mapped sequence to complete
vim.opt.undofile = true                                     	-- enable persistent undo
vim.opt.updatetime = 500                                    	-- time for CursorHold autocommand event
vim.opt.wildmenu = true                                     	-- enable "enhanced menu" of command line completion
vim.opt.wrap = false                                        	-- enable text wrap
vim.opt.writebackup = false					                    -- make a backup before overwriting a file

vim.cmd [[set fillchars=eob:\ ]]                                -- empty lines at the end of a buffer
