#!/usr/bin/env fennel

;;  This file is part of Quickwin.
;;
;;  Quickwin is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License as published by
;;  the Free Software Foundation, either version 3 of the License, or
;;  (at your option) any later version.
;;
;;  Quickwin is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.
;;
;;  You should have received a copy of the GNU General Public License
;;  along with Quickwin.  If not, see <https://www.gnu.org/licenses/>.

(local dbus (require :ldbus))

(local dbus-name "org.tcrawley.quickwin")
(local dbus-activate-path "/org/tcrawley/quickwin")

(fn already-running? [bus]
  (not= "primary_owner" (dbus.bus.request_name bus dbus-name)))

(fn init [bus]
  (local qw (require :quickwin))
  (qw.init bus))

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
