local ok, django = pcall(require, "django")
if not ok then
	return
end

django.setup({
	views = {
		auto_refresh = {
			on_picker_open = true,
			file_watch_patterns = {
				"*/urls.py",
				"*/views.py",
				"*/view.py",
				"*/*views.py",
				"*/*view.py",
				"*/*viewset.py",
				"*/*view_set.py",
				"*/*api.py",
			},
		},
	},
	models = {
		auto_refresh = {
			on_picker_open = true,
			file_watch_patterns = { "*/models.py", "*/models/*.py" },
		},
	},
	shell = {
		command = "shell_plus",
		position = "right",
	},
})
