.PHONY: macos clean

macos:
	cargo rustc -- -C link-args=-Wl,-undefined,dynamic_lookup

clean:
	cargo clean
