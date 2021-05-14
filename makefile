DEV_VERSION=0.1.0
STABLE_VERSION=0.1.0

build-dev-docs:
	gleam docs build --version=$(DEV_VERSION) . && rm -rf ./docs && mv --force gen/docs ./docs

build-stable-docs:
	gleam docs build --version=$(STABLE_VERSION) . && rm -rf ./docs && mv --force gen/docs ./doc