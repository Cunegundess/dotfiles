local ok_mason, mason = pcall(require, "mason")
if not ok_mason then
	return
end

local ok_tool_installer, tool_installer = pcall(require, "mason-tool-installer")
if not ok_tool_installer then
	mason.setup()
	return
end

mason.setup()

tool_installer.setup({
	ensure_installed = {
		"basedpyright",
		"lua-language-server",
		"typescript-language-server",
		"jdtls",
		"groovy-language-server",
		"stylua",
		"ruff",
	},
	auto_update = false,
})

tool_installer.setup_handlers({
	function(tool)
		if tool == "basedpyright" then
			require("mason-tool-installer").install({ "basedpyright" })
		end
	end,
})
