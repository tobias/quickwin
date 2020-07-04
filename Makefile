%.lua: %.fnl
	fennel --compile $< > $@

.PHONY: kill
kill:
	kill `ps ux | grep quickwin-main.lua | grep -v grep | tr -s ' ()' '\t' |  cut -f2`

.PHONY: build
build: quickwin.lua quickwin-main.lua kill

.PHONY: clean
clean:
	rm -f *.lua

.PHONY: autobuild
autobuild:
	filewatcher quickwin-main.fnl quickwin.fnl "echo '-----------'; make build"

