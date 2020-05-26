
testwatch:
	ghcid -T :main -c 'stack repl exsqlain:lib exsqlain:test:exsqlain-test' --restart="package.yaml" --restart="stack.yaml"
