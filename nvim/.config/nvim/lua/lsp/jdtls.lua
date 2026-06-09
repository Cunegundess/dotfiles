return {
	cmd = { "jdtls" },
	filetypes = { "java" },
	root_markers = {
		"gradlew",
		"settings.gradle",
		"settings.gradle.kts",
		"build.gradle",
		"build.gradle.kts",
		"pom.xml",
		"mvnw",
		".git",
	},
	settings = {
		java = {
			configuration = {
				runtimes = {},
			},
			eclipse = {
				downloadSources = true,
			},
			maven = {
				downloadSources = true,
			},
			referencesCodeLens = {
				enabled = true,
			},
			signatureHelp = {
				enabled = true,
			},
			completion = {
				guessMethodArguments = true,
			},
			implementationsCodeLens = {
				enabled = true,
			},
		},
	},
}
