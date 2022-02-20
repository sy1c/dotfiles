require('telescope').setup{
    defaults = {
        path_display = { "smart" },
        mappings = {
            i = {
                ["<C-h>"] = "which_key"
            }
        }
    },
    pickers = {
    },
    extensions = {
    }
}
