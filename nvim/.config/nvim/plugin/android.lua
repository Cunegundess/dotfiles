local APP_ID = 'com.proxion.anytag'
local MAIN_ACTIVITY = APP_ID .. '/.MainActivity'

local function gradle_root()
  local base = vim.fn.getcwd()
  local found = vim.fs.find({ 'gradlew', 'settings.gradle.kts', 'settings.gradle' }, { path = base, upward = true })
  if #found > 0 then
    return vim.fn.fnamemodify(found[1], ':p:h')
  end
  return base
end

local function term_split(cmd, opts)
  opts = opts or {}
  local buf = vim.api.nvim_create_buf(true, false)
  local win = vim.api.nvim_open_win(buf, true, { split = 'below', height = opts.height or 12 })
  vim.bo[buf].filetype = 'terminal'
  local job = vim.fn.termopen(cmd, { cwd = opts.cwd or gradle_root() })
  if job > 0 then
    vim.api.nvim_win_set_buf(win, buf)
  end
  vim.cmd('startinsert')
  return buf
end

local function gradle(task)
  term_split('./gradlew ' .. task)
end

vim.api.nvim_create_user_command('Gradle', function(args)
  gradle(args.args)
end, { nargs = '*', complete = 'file' })

vim.api.nvim_create_user_command('AndroidBuild', function()
  gradle(':app:assembleDebug')
end, {})

vim.api.nvim_create_user_command('AndroidInstall', function()
  gradle(':app:installDebug')
end, {})

vim.api.nvim_create_user_command('AndroidRun', function()
  gradle(':app:installDebug && adb shell am start -n ' .. MAIN_ACTIVITY)
end, {})

vim.api.nvim_create_user_command('AdbDevices', function()
  term_split('adb devices -l')
end, {})

vim.api.nvim_create_user_command('AdbLogcat', function()
  local pid = vim.fn.system('adb shell pidof ' .. APP_ID):gsub('%s+', '')
  local cmd = (pid ~= '' and 'adb logcat --pid=' .. pid) or 'adb logcat'
  term_split(cmd)
end, {})

vim.api.nvim_create_user_command('AndroidHelp', function()
  local lines = {
    '=== Anytag / Android workflow ===',
    '',
    ':AndroidBuild    build debug APK (app/build/outputs/apk/debug/app-debug.apk)',
    ':AndroidInstall  install debug build on connected device',
    ':AndroidRun      install + launch com.proxion.anytag/.MainActivity',
    ':Gradle <task>   run any gradle task (e.g. :app:assembleRelease, :app:bundleRelease)',
    ':AdbDevices      list connected devices/emulators',
    ':AdbLogcat       follow logcat (filtered to app pid when running)',
    ':AdbForward      forward tcp:5005 -> jdwp:pid (for DAP attach)',
    '',
    'Keymaps:',
    '  <leader>ab  build    <leader>ai  install',
    '  <leader>ar  run      <leader>al  logcat',
    '  <leader>aa  devices',
    '',
    'Debug (Android): build+install+run, set breakpoint (<leader>bb),',
    ':AdbForward, then DAP config "Android App (Anytag)" (attach :5005) and <leader>bc.',
    'App must be running and built as debug (debuggable).',
    '',
    'Emulator: emulator -avd medium_phone   (KVM ok)',
    'APK release: ./gradlew :app:assembleRelease',
    'AAB (Play): ./gradlew :app:bundleRelease',
  }
  local buf = vim.api.nvim_create_buf(true, false)
  vim.api.nvim_buf_set_lines(buf, 0, -1, false, lines)
  vim.api.nvim_buf_set_name(buf, 'android-help')
  vim.bo[buf].filetype = 'help'
  vim.api.nvim_open_win(buf, true, { split = 'above', height = #lines })
end, {})

local map = vim.keymap.set

map('n', '<leader>ab', '<cmd>AndroidBuild<CR>', { desc = 'Android: build debug APK' })
map('n', '<leader>ai', '<cmd>AndroidInstall<CR>', { desc = 'Android: install debug APK' })
map('n', '<leader>ar', '<cmd>AndroidRun<CR>', { desc = 'Android: install + run app' })
map('n', '<leader>al', '<cmd>AdbLogcat<CR>', { desc = 'Android: follow logcat' })
map('n', '<leader>aa', '<cmd>AdbDevices<CR>', { desc = 'Android: list devices' })
map('n', '<leader>ah', '<cmd>AndroidHelp<CR>', { desc = 'Android: show workflow help' })
