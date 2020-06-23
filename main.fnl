#!/usr/bin/env fennel

(local fennel (require :fennel))
(table.insert (or package.loaders package.searchers) fennel.searcher)
(local qw (require :quickwin))

(qw.run)
