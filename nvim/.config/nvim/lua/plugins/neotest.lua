return {
  {
    'nvim-neotest/neotest',
    lazy = true,
    dependencies = {
      'nvim-neotest/nvim-nio',
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
      'nvim-neotest/neotest-python',
    },
    config = function()
      require('neotest').setup {

        icons = {
          expanded = '',
          child_prefix = ' ',
          child_indent = ' ',
          final_child_prefix = ' ',
          non_collapsible = ' ',
          collapsed = '',

          running_animated = { '󰄰', '󰪞', '󰪟', '󰪠', '󰪡', '󰪢', '󰪣', '󰪤', '󰪥', '󰪥' },

          passed = '',
          running = '',
          failed = '',
          unknown = '',
        },

        consumers = {
          notify = function(client)
            client.listeners.results = function(adapter_id, results, partial)
              if partial then
                return
              end

              local textsummary = {
                total = 0,
                passed = 0,
                failed = 0,
                skipped = 0,
              }

              for _, value in pairs(results) do
                textsummary.total = textsummary.total + 1

                if value.status == 'passed' then
                  textsummary.passed = textsummary.passed + 1
                elseif value.status == 'failed' then
                  textsummary.failed = textsummary.failed + 1
                elseif value.status == 'skipped' then
                  textsummary.skipped = textsummary.skipped + 1
                end
              end

              if textsummary.failed == 0 and textsummary.skipped == 0 then
                require('neotest.lib').notify(textsummary.passed .. ' tests passed.')
              elseif textsummary.failed > 0 then
                require('neotest.lib').notify(textsummary.passed .. ' tests passed, ' .. textsummary.failed .. ' failed.', vim.log.levels.ERROR)
              else
                require('neotest.lib').notify(textsummary.passed .. ' tests passed, ' .. textsummary.skipped .. ' skipped.', vim.log.levels.WARN)
              end
            end
            return {}
          end,
        },

        adapters = {
          require 'neotest-python' {
            dap = { justMyCode = false },
            runner = 'pytest',
            args = { '--maxfail=1', '--disable-warnings', '-q' },
            python = 'python3',
          },
        },
      }
    end,
  },
}
