-- File location: ~/.config/nvim/lua/plugins/remote-nvim.lua

return {
   "amitds1997/remote-nvim.nvim",
   version = "*", -- Pin to GitHub releases
   dependencies = {
       "nvim-lua/plenary.nvim", -- For standard functions
       "MunifTanjim/nui.nvim", -- To build the plugin UI
       "nvim-telescope/telescope.nvim", -- For picking between different remote methods
   },
   config = function()
     require("remote-nvim").setup({
       -- Configure to use a better terminal integration for client_callback
       -- This will open a new tab/window in your terminal instead of a floating window
       client_callback = function(port, _)
         -- Launch in a new terminal window instead of a floating window
         vim.fn.system(string.format("wezterm cli split-pane --horizontal -- nvim --server localhost:%s --remote-ui &", port))
       end,
       
       -- Configuration for remote sync
       remote = {
         app_name = "nvim",
         copy_dirs = {
           -- Copy your entire config directory
           config = {
             base = vim.fn.stdpath("config"),
             dirs = "*", -- Copy all directories
             compression = {
               enabled = true, -- Enable compression for faster transfers
             },
           },
           -- Copy only necessary data directories (adjust as needed)
           data = {
             base = vim.fn.stdpath("data"),
             dirs = {"lazy"}, -- Copy package manager data
             compression = {
               enabled = true,
             },
           },
         },
       },
     })
   end,
}
