return {
    "Exafunction/codeium.nvim",
    dependencies = {
        "nvim-lua/plenary.nvim",
        "hrsh7th/nvim-cmp",
    },
    config = function()
        require("codeium").setup({
            -- Change '<C-g>' here to any keycode you like.
            keymap = {
                ['i'] = {
                    ['<Tab>'] = function() return vim.fn['codeium#Accept']() end,
                    ['<c-;>'] = function() return vim.fn['codeium#CycleCompletions'](1) end,
                    ['<c-,>'] = function() return vim.fn['codeium#CycleCompletions'](-1) end,
                    ['<c-x>'] = function() return vim.fn['codeium#Clear']() end,
                }
            }
        })
    end
}
