#!/usr/bin/env fennel

(local fennel (require :fennel))
(table.insert (or package.loaders package.searchers) fennel.searcher)
(local qw (require :quickwin))
(local lu (require :luaunit))

(each [_  test (ipairs qw.tests)]
  (test lu))
