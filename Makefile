.PHONY: build run clean

build:
	lake build

run: build
	./.lake/build/bin/liq-lean-policy

clean:
	lake clean