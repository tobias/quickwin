%.lua: %.fnl
	fennel --compile $< > $@

quickwin.bin: quickwin.lua main.lua
	luac -o quickwin.bin quickwin.lua main.lua

.PHONY: kill
kill:
	kill `ps ux | grep quickwin.bin | grep -v grep | cut -f2 -d" "`

.PHONY: build
build: quickwin.bin kill

.PHONY: clean
clean:
	rm -f *.lua *.bin

.PHONY: autobuild
autobuild:
	filewatcher main.fnl quickwin.fnl "echo '-----------'; make build"

