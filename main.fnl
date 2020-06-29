#!/usr/bin/env fennel

(local dbus (require :ldbus))

(local dbus-name "org.tcrawley.quickwin")
(local dbus-activate-path "/org/tcrawley/quickwin")

(fn already-running? [bus]
  (not= "primary_owner" (dbus.bus.request_name bus dbus-name)))

(fn listen-for-activate-signal [bus activate]
  (local socket (require :socket))

  (dbus.bus.add_match bus (.. "type='signal',interface='" dbus-name "'"))
  (bus:flush)
  (while (bus:read_write 0)
    (let [msg (bus:pop_message)]
      (when (and msg
                 (= dbus-activate-path (msg:get_path)))
        (activate))
      (socket.sleep 0.1))))

(fn init [bus]
  (local qw (require :quickwin))
  (qw.init)
  (qw.activate) ;; activate qw on process start
  (listen-for-activate-signal bus qw.activate))

(fn send-activate-signal []
  (let [bus (dbus.bus.get :session)
        msg (dbus.message.new_signal dbus-activate-path dbus-name "activate")]
    (dbus.bus.request_name bus (.. dbus-name ".activator"))
    (bus:send msg)
    (bus:flush)))

(fn run []
  (let [primary-bus (dbus.bus.get :session)]
    (if (already-running? primary-bus)
        (send-activate-signal)
        (init primary-bus))))

(run)
