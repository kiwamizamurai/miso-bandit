.PHONY: help setup dev build serve clean lint format format-check

.DEFAULT_GOAL := help

# Project variables
PROJECT_NAME := miso-bandit
PORT := 8080

help: ## Show this help message
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-20s\033[0m %s\n", $$1, $$2}'

setup: ## Initial project setup (download dependencies)
	@if command -v nix > /dev/null; then \
		echo "📦 Setting up project dependencies..."; \
		nix develop .#wasm --command echo "✅ Dependencies cached and ready"; \
		echo "📝 Run 'make dev' to start development server"; \
		echo "📝 Run 'nix develop .#wasm' to enter WASM development shell"; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

dev: ## Run development server with JSaddle (port 8080)
	@echo "🚀 Starting development server on http://localhost:$(PORT)"
	@cabal run || echo "❌ Build failed. Check for errors above."

build: ## Build WASM for deployment
	@if command -v nix > /dev/null; then \
		echo "🚀 Building WASM..."; \
		nix develop .#wasm --command sh -c 'wasm32-wasi-cabal update && \
			wasm32-wasi-cabal build --allow-newer && \
			WASM_PATH=$$(wasm32-wasi-cabal list-bin miso-bandit --allow-newer 2>/dev/null | tail -1) && \
			mkdir -p docs && \
			cp -v "$$WASM_PATH" docs/app.wasm && \
			$$(wasm32-wasi-ghc --print-libdir)/post-link.mjs --input "$$WASM_PATH" --output docs/ghc_wasm_jsffi.js && \
			echo "✅ WASM build complete: docs/"'; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

serve: ## Serve WASM build locally (port 8080)
	@if [ -f "docs/app.wasm" ]; then \
		echo "🌐 Serving at http://localhost:$(PORT)"; \
		cd docs && python3 -m http.server $(PORT); \
	else \
		echo "❌ No WASM build found. Run 'make build' first"; \
		echo "For development, you can run 'make dev' instead"; \
	fi

clean: ## Clean build artifacts and WASM files
	@echo "🧹 Cleaning build artifacts..."
	@rm -rf docs/app.wasm docs/ghc_wasm_jsffi.js dist-newstyle .ghc.environment.*
	@if command -v nix > /dev/null; then \
		nix develop .#wasm --command wasm32-wasi-cabal clean 2>/dev/null || true; \
	fi
	@cabal clean 2>/dev/null || true
	@echo "✅ Cleaned"

lint: ## Lint code with hlint (in Nix shell)
	@if command -v nix > /dev/null; then \
		nix develop --command hlint src/; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

format: ## Format code with ormolu (in Nix shell)
	@if command -v nix > /dev/null; then \
		nix develop --command sh -c 'find src -name "*.hs" -exec ormolu --mode inplace {} \;'; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

format-check: ## Check if code is formatted (in Nix shell)
	@if command -v nix > /dev/null; then \
		nix develop --command sh -c 'find src -name "*.hs" -exec ormolu --mode check {} \;'; \
	else \
		echo "❌ Nix is required. Install from: https://nixos.org/download.html"; \
		exit 1; \
	fi

.SILENT: help