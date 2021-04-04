VERSION=1.0.0

build-docs:
	gleam docs build --version=$(VERSION) . && rm -rf ./docs && mv --force gen/docs ./docs