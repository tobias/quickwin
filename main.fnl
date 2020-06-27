#!/usr/bin/env fennel

(local fennel (require :fennel))
(table.insert (or package.loaders package.searchers) fennel.searcher)
(local qw (require :quickwin))
(local dbus (require :ldbus))
(local socket (require :socket))

(local primary-bus (dbus.bus.get :session))

(local dbus-name "org.tcrawley.quickwin")
(local dbus-activate-path "/org/tcrawley/quickwin")

(lambda already-running? [bus]
  (let [res (dbus.bus.request_name bus dbus-name)]
    (print res)
    (not= "primary_owner" res)))

(lambda listen-for-activate-signal [bus activate]
  (print "listening")
  (dbus.bus.add_match bus (.. "type='signal',interface='" dbus-name "'"))
  (bus:flush)
  (while (bus:read_write 0)
    (let [msg (bus:pop_message)]
      (when (and msg
                 (= dbus-activate-path (msg:get_path)))
        (print "activating")
        (activate))
      (socket.sleep 0.1))))

(lambda send-activate-signal []
  (print "signaling")
  (let [bus (dbus.bus.get :session)
        msg (dbus.message.new_signal dbus-activate-path dbus-name "activate")]
    (dbus.bus.request_name bus (.. dbus-name ".activator"))
    (bus:send msg)
    (bus:flush)))

(lambda run []
  (if (already-running? primary-bus)
      (send-activate-signal)
      (do
        (qw.activate) ;; load qw on process start
        (listen-for-activate-signal primary-bus qw.activate))))

(run)
