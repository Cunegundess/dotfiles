local root_markers = {
	"settings.gradle.kts",
	"settings.gradle",
	"build.gradle.kts",
	"build.gradle",
	"gradlew",
	"pom.xml",
}

local home = vim.fn.expand("$HOME")

return {
	cmd = {
		"intellij-server",
		"--stdio",
		"--system-path",
		home .. "/.cache/kotlin-lsp/",
	},
	filetypes = { "kotlin" },
	root_markers = root_markers,
	single_file_support = true,
	cmd_env = {
		IJ_JAVA_OPTIONS = "-Xmx4g -Xms2g -XX:+UseG1GC -Dcom.jetbrains.ls.imports.gradle.offline=true",
	},
}
