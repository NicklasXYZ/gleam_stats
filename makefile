VERSION=0.1.0

build-docs:
	gleam docs build --version=$(VERSION) . && rm -rf ./docs && mv --force gen/docs ./docs