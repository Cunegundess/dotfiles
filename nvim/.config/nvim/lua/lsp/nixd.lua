return {
   cmd = { "nixd" },
   filetype = { "nix" },
   settings = {
      nixd = {
         nixpkgs = {
            expr = "import <nixpkgs> { }",
         },
         formatting = {
            command = { "nixfmt-classic" },
         },
         options = {
            nixos = {
               expr = '(builtins.getFlake ("git+file://" + toString ./.)).nixosConfigurations.k-on.options',
            },
            home_manager = {
               expr = '(builtins.getFlake ("git+file://" + toString ./.)).homeConfigurations."ruixi@k-on".options',
            },
         },
      },
   },
}

