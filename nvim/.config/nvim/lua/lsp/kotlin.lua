local root_markers = {
	"settings.gradle.kts",
	"settings.gradle",
	"build.gradle.kts",
	"build.gradle",
	"gradlew",
	"pom.xml",
}

return {
	cmd = { "intellij-server", "--stdio" },
	filetypes = { "kotlin" },
	root_markers = root_markers,
	single_file_support = true,
}
