return {
  { 'tpope/vim-dadbod' },

  {
    'kristijanhusak/vim-dadbod-ui',
    dependencies = {
      {
        'tpope/vim-dadbod',
        lazy = true,
      },
      {
        'kristijanhusak/vim-dadbod-completion',
        ft = { 'sql' },
        lazy = true,
      },
    },
    cmd = {
      'DBUI',
      'DBUIToggle',
      'DBUIAddConnection',
      'DBUIFindBuffer',
    },
    init = function()
      vim.g.db_ui_use_nerd_fonts = 1
      vim.gdb_ui_table_helpers = {
        postgresql = {
          Count = 'select count(*) from "{table}"',
        },
      }
      vim.g.dbs = {
        { name = 'hb-tracer-dev', url = 'postgres://DB_DEV_USER:DB_DEV_PASSWORD@localhost:5432/DB_DEV' },
        { name = 'jcn-dev', url = 'postgres://postgres:jmalianca2023@localhost:5432/JCN' },
        { name = 'edp-dev', url = 'postgresql://DB_DEV_USER:DB_DEV_PASSWORD@localhost:5432/DB_DEV' },
      }
    end,
  },
}
