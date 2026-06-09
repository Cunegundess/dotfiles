local ok, kotlin = pcall(require, "kotlin")
if not ok then
	return
end

kotlin.setup({
	root_markers = {
		"gradlew",
		".git",
		"mvnw",
		"settings.gradle",
		"settings.gradle.kts",
	},
	jre_path = nil,
	jdk_for_symbol_resolution = nil,
	jvm_args = {
		"-Xmx4g",
	},
	inlay_hints = {
		enabled = true,
		parameters = true,
		types_property = true,
		types_variable = true,
		function_return = true,
	},
})
