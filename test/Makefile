default:
	$(shell vessel bin)/moc $(shell vessel sources) -wasi-system-api -o ArrayTest.wasm ArrayTest.mo && wasmtime ArrayTest.wasm
	rm -f ArrayTest.wasm
