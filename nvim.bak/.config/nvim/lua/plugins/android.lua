local ok_android, android = pcall(require, "android")
if not ok_android then
	return
end

android.setup({
	sdk = {
		local_properties = true,
		local_properties_paths = { "local.properties" },
		root_env_keys = { "ANDROID_SDK_ROOT", "ANDROID_HOME" },
	},
	build = {
		scan_all_apk_outputs = true,
	},
	ui = {
		file_watcher = true,
		autosave = true,
		restore_logcat = true,
	},
	keymaps = {
		enabled = true,
		mappings = {
			menu = "<leader>am",
			targets = "<leader>at",
			tools = "<leader>ao",
			actions = "<leader>aa",
			build = "<leader>ab",
		},
	},
})

vim.keymap.set("n", "<leader>ar", "<Plug>(AndroidRun)", { desc = "Android: Run app" })
vim.keymap.set("n", "<leader>aS", "<Plug>(AndroidRunStop)", { desc = "Android: Stop run" })
vim.keymap.set("n", "<leader>al", "<Plug>(AndroidLogcat)", { desc = "Android: Logcat" })
vim.keymap.set("n", "<leader>aB", "<Plug>(AndroidBuildPrompt)", { desc = "Android: Build with prompt" })
vim.keymap.set("n", "<leader>ag", "<Plug>(AndroidGradleTasks)", { desc = "Android: Gradle tasks" })
