return {
  {
    'nvim-neotest/neotest',
    lazy = false,
    dependencies = {
      'nvim-neotest/nvim-nio',
      'nvim-lua/plenary.nvim',
      'nvim-treesitter/nvim-treesitter',
      'nvim-neotest/neotest-python',
      'antoinemadec/FixCursorHold.nvim',
    },
    keys = {
      {
        '<leader>tn',
        function()
          require('neotest').run.run()
        end,
        desc = 'Neotest: Run Nearest',
      },
      {
        '<leader>tf',
        function()
          require('neotest').run.run(vim.fn.expand '%')
        end,
        desc = 'Neotest: Run File',
      },
      {
        '<leader>ts',
        function()
          require('neotest').summary.toggle()
        end,
        desc = 'Neotest: Toggle Summary',
      },
      {
        '<leader>to',
        function()
          require('neotest').output.open { enter = true }
        end,
        desc = 'Neotest: Open Output',
      },
      {
        '<leader>tl',
        function()
          require('neotest').run.run_last()
        end,
        desc = 'Neotest: Run Last',
      },
      {
        '<leader>ta',
        function()
          require('neotest').run.attach()
        end,
        desc = 'Neotest: Attach to Nearest',
      },
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
            runner = function()
              -- Pega o primeiro container cujo nome contém "app"
              local handle = io.popen "docker ps --filter 'name=app' --format '{{.Names}}' | head -n 1"
              local container = handle:read('*a'):gsub('%s+', '')
              handle:close()

              if container == '' then
                vim.notify("Nenhum container com 'app' no nome foi encontrado.", vim.log.levels.ERROR)
                return 'python3'
              end

              return 'docker exec -i ' .. container .. ' pytest'
            end,
            args = { '--maxfail=1', '--disable-warnings', '-q' },
            python = 'python3',
          },
        },
      }
    end,
  },
}
