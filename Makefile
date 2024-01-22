.PHONE: test build

README.md: src/lib.rs
	cargo readme > $@

test:
	cargo test

build: test README.md
	cargo build
