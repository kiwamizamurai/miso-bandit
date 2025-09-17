{
  description = "Multi-Armed Bandit Simulation with Miso";

  inputs = {
    # Pin to NixOS 24.05 stable for reproducibility
    nixpkgs.url = "github:NixOS/nixpkgs/b134951a4c9f3c995fd7be05f3243f8ecd65d798";
    flake-utils.url = "github:numtide/flake-utils";
    
    # WASM backend for GHC (following miso's approach)
    ghc-wasm-meta.url = "gitlab:haskell-wasm/ghc-wasm-meta?host=gitlab.haskell.org";
  };

  outputs = { self, nixpkgs, flake-utils, ghc-wasm-meta }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        
        # Haskell packages for development
        haskellPackages = pkgs.haskell.packages.ghc96;
        
        # Project package
        miso-bandit = haskellPackages.callCabal2nix "miso-bandit" ./. {};
        
        # Development tools
        devTools = with haskellPackages; [
          cabal-install
          ghcid
          hlint
          ormolu
          haskell-language-server
        ];
        
        # WASM build tools
        wasmTools = [
          pkgs.wabt           # wasm-opt, wasm-strip
          pkgs.binaryen       # Additional WASM tools
        ];
        
      in {
        # Package for regular GHC build
        packages = {
          default = miso-bandit;
          miso-bandit = miso-bandit;
        };
        
        # Development shells
        devShells = {
          # Default development shell (JSaddle)
          default = pkgs.mkShell {
            buildInputs = [
              haskellPackages.ghc
              pkgs.cabal-install
              pkgs.pkg-config
            ] ++ devTools ++ wasmTools ++ [
              pkgs.nodejs
              pkgs.python3
            ];
            
            shellHook = ''
              echo "🎰 Multi-Armed Bandit Simulation Development Environment"
              echo ""
              echo "Available commands:"
              echo "  make dev      - Run development server (JSaddle)"
              echo "  make build    - Build WASM"
              echo "  make serve    - Serve WASM locally"
              echo "  make clean    - Clean build artifacts"
              echo ""
              echo "GHC version: $(ghc --version)"
              echo "Cabal version: $(cabal --version | head -1)"
            '';
          };
          
          # WASM development shell (following miso's approach)
          wasm = pkgs.mkShell {
            buildInputs = wasmTools ++ [
              ghc-wasm-meta.packages.${system}.all_9_12
              pkgs.cabal-install
              pkgs.nodejs
              pkgs.python3
              # Development tools for code quality
              haskellPackages.hlint
              haskellPackages.ormolu
              haskellPackages.haskell-language-server
            ];
            
            shellHook = ''
              echo "🎰 Multi-Armed Bandit WASM Development Environment"
              echo ""
              echo "Available commands:"
              echo "  wasm32-wasi-cabal build  - Build WASM"
              echo "  make build               - Full WASM build and deployment"
              echo "  make serve               - Serve WASM locally"
              echo "  make clean               - Clean build artifacts"
              echo ""
              echo "WASM GHC version: $(wasm32-wasi-ghc --version 2>/dev/null || echo 'Loading...')"
              
              # Helper functions like miso
              function build() {
                wasm32-wasi-cabal build $1
              }
              function clean() {
                wasm32-wasi-cabal clean
              }
              function update() {
                wasm32-wasi-cabal update
              }
            '';
          };
        };
        
        # Apps (executables)
        apps = {
          default = flake-utils.lib.mkApp {
            drv = miso-bandit;
          };
          
          # Development server
          dev-server = {
            type = "app";
            program = "${pkgs.writeShellScript "dev-server" ''
              ${haskellPackages.ghc}/bin/runghc \
                -package-env=- \
                -package miso \
                -package jsaddle-warp \
                src/Main.hs
            ''}";
          };
        };
      });
}