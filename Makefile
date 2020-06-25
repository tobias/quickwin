autotest:
	filewatcher test.fnl quickwin.fnl "echo '-----------'; ./test.fnl"

autorun:
	filewatcher main.fnl quickwin.fnl "echo '-----------'; ./main.fnl"

autobuild:
	filewatcher main.fnl quickwin.fnl "echo '-----------'; make build"

%.lua: %.fnl
	fennel --compile $< > $@

quickwin.bin: quickwin.lua main.lua
	luac -o quickwin.bin quickwin.lua main.lua

build: quickwin.bin

clean:
	rm -f *.lua *.bin

