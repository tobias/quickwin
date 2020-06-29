%.lua: %.fnl
	fennel --compile $< > $@

.PHONY: kill
kill:
	kill `ps ux | grep quickwin-main.lua | grep -v grep | cut -f2 -d" "`

.PHONY: build
build: quickwin.lua quickwin-main.lua kill

.PHONY: clean
clean:
	rm -f *.lua

.PHONY: autobuild
autobuild:
	filewatcher quickwin-main.fnl quickwin.fnl "echo '-----------'; make build"

