return {
  cmd = { 'sql-language-server' },
  filetypes = { 'sql' },
  settings = {
    sqlLanguageServer = {
      lint = {
        rules = {
          align_column_to_the_first = 'error',
          column_new_line = 'error',
          linebreak_after_clause_keyword = 'off',
          reserved_word_case = { 'error', 'upper' },
          space_surrounding_operators = 'error',
          where_clause_new_line = 'error',
          align_where_clause_to_the_first = 'error',
        },
      },
    },
  },
}
