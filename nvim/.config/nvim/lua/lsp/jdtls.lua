local root_markers = {
	"gradlew",
	"settings.gradle.kts",
	"settings.gradle",
	"build.gradle.kts",
	"build.gradle",
	"pom.xml",
	"mvnw",
	".git",
}

local root_dir = vim.fs.root(0, root_markers)

return {
	cmd = {
		"jdtls",
		"-data",
		vim.fn.stdpath("data") .. "/jdtls-workspace/" .. vim.fn.fnamemodify(root_dir or vim.fn.getcwd(), ":t"),
	},
	filetypes = { "java" },
	root_markers = root_markers,
	settings = {
		java = {
			eclipse = {
				downloadSources = true,
			},
			configuration = {
				updateBuildConfiguration = "interactive",
				runtimes = {},
			},
			maven = {
				downloadSources = true,
			},
			references = {
				includeDecompiledSources = true,
			},
		},
	},
}
