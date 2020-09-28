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

;; lanes has to be initialized before anything else, otherwise it
;; fails with a type assertion
(local lanes
       (let [lanes (require :lanes)]
         (lanes.configure {})))

(local dbus (require :ldbus))
(local lgi (require :lgi))
(local os (require :os))
(local sfun (require :std.functional))
(local sio (require :std.io))
(local socket (require :socket))
(local stable (require :std.table))
(local std (require :std))
(local view (require :fennelview))
(local xctrl (require :xctrl))

(local gdk (lgi.require "Gdk" "3.0"))
(local gtk (lgi.require "Gtk" "3.0"))

(local dbus-name "org.tcrawley.quickwin")
(local dbus-activate-path "/org/tcrawley/quickwin")

;; quick and dirty multimethods
(local _mutlifn-dispatch-table {})

(macro multifn [name dispatch-fn]
  `(fn ,name [...]
     (let [dispatch-v# (,dispatch-fn ...)
           f-table# (. _mutlifn-dispatch-table ,name)
           f# (or (. f-table# dispatch-v#)
                  (. f-table# :default))]
       (f# ...))))

(fn multi [name dispatch-v f]
  (let [f-table (or (. _mutlifn-dispatch-table name)
                    {})]
    (tset f-table dispatch-v f)
    (tset _mutlifn-dispatch-table name f-table)
    nil))

(lambda pp [...] (print (view [...])))

(lambda preserve-meta
  [f xs]
  (let [mt (getmetatable xs)
        res (f xs)]
    (setmetatable res mt)
    res))

(lambda map [f xs]
  (sfun.map f std.elems xs))

(lambda map-preserve-meta [f xs]
  (preserve-meta (partial map f) xs))

(lambda filter [f xs]
  (preserve-meta
   #(sfun.filter f std.elems $)
   xs))

(lambda reduce [f init xs]
  (sfun.reduce f init std.elems xs))

(lambda reduce-kv [f init xs]
  (sfun.reduce f init pairs xs))

(lambda assoc [t k v]
  (tset t k v)
  t)

(lambda first [xs]
  (. xs 1))

(lambda last [xs]
  (. xs (length xs)))

(lambda str->list [s]
  (let [l []]
    (for [i 1 (length s)]
      (table.insert l (s:sub i i)))
    l))

;; was recursive, but w/o tailcall so blew the stack
(lambda fuzzy-search [text filter-s]
  (let [pattern (string.lower (table.concat (str->list filter-s) ".-"))
        text (text:lower)
        matches []]
    (var search-pos 1)
    (while (and search-pos
                (<= search-pos (length text)))
      (let [(start end) (text:find pattern search-pos)]
        (when end
          (table.insert matches [start end]))
        (set search-pos (when end
                          (if (= search-pos end)
                              (+ 1 search-pos)
                              end)))))
    matches))

(lambda range [start end]
  (let [r []]
    (for [i start end]
      (table.insert r i))
    r))

;; data types:
;;
;; tree-widget:
;; {:store list-store-object
;;  :view  tree-view-object}
;;
;; item-list - list with a metatable of:
;; {:type list-type}

(lambda text-column [text pos]
  (gtk.TreeViewColumn
   {:title text
    1 [(gtk.CellRendererText)
       {:text pos}]}))

(lambda italic-text-column [text pos]
  (let [renderer (gtk.CellRendererText)
        col (gtk.TreeViewColumn
             {:title text
              1 [renderer
                 {:text pos}]})]
    (col:set renderer
             (fn [_tree-c cell-r _model _iter]
               (let [attr-list (lgi.Pango.AttrList.new)]
                 (attr-list:insert (lgi.Pango.Attribute.weight_new lgi.Pango.Weight.BOLD))
                 (tset cell-r :attributes attr-list))))
    col))

(local xc (xctrl.new))
(local phrase-mapping {})
(local window-focus-data (lanes.linda))
;; init the linda with a value so we can read it before the thread has
;; a chance to populate it
(window-focus-data:set :data {})

(fn xevent-listener []
  (let [lanes (require :lanes)
        socket (lanes.require :socket)
        xctrl (lanes.require :xctrl)
        xc (xctrl.new)
        window-focus-times {}]
    (xc:listen (fn [ev id]
                 (when (or (= "a" ev)  ;; activated
                           (= "x" ev)) ;; closed
                   (tset window-focus-times (tostring id)
                         (if (= "a" ev)
                             (socket.gettime)
                             nil))
                   (window-focus-data:set :data window-focus-times))
                 true))))

(lambda start-xevent-listener-thread []
  (let [lane (lanes.gen "*" xevent-listener)]
    (lane)))

(lambda match-score [filter-text item]
  "Scores a match set. The score is the number of matches + a boosting
factor for the compactness of the matches related to the length of the
filter-text."
  (if item.matches
      (let [filter-len (length filter-text)
            base-score (length item.matches)
            first-match (first item.matches)
            base-score (if (= 1 (first first-match))
                           ;; the first match has the same char as the
                           ;; start of the full name, so boost it
                           ;; higher
                           (+ base-score 100)
                           base-score)
            score (reduce
                   #(+ $1 (/ 20
                             (+ 1
                                (- filter-len
                                   (- (last $2) (first $2))))))
                   base-score
                   item.matches)]
        score)
      0))

(lambda set-selection [tree-widget idx]
  (let [selection (tree-widget.view:get_selection)]
    (selection:select_path (gtk.TreePath.new_from_string (.. (- idx 1))))))

(lambda sort-by-last-active [window-list]
  (let [focus-data (window-focus-data:get :data)]
    (stable.sort window-list
                 (fn [a b]
                   (let [a-time (or (. focus-data a.id) 0)
                         b-time (or (. focus-data b.id) 0)]
                     (> a-time b-time))))))

(lambda sort-by-match-score [list filter-text]
  (stable.sort list
               #(if (and $1 $2)
                    (let [score-1 (match-score filter-text $1)
                          score-2 (match-score filter-text $2)]
                      (if (and score-1 score-2)
                          (> score-1 score-2) ;; reverse sort
                          false))
                    false)))

(lambda make-prior-window-first [window-list]
  "Makes the second window first so it can be quickly switched to. W/o
  this, the first window is the one that had the focus when qw was activated."
  (if (= 1 (length window-list))
      window-list
      (let [f (. window-list 1)
            s (. window-list 2)]
        (tset window-list 1 s)
        (tset window-list 2 f)
        window-list)))

(lambda list-type [list]
  (. (or (getmetatable list) {}) :type))

(multifn pre-sort-list list-type)

(multi pre-sort-list :window
       (fn [window-list]
         (-> window-list
             (sort-by-last-active)
             (make-prior-window-first))))

(multi pre-sort-list :action
       (fn [action-list] action-list))

(multifn format-list #(list-type $2))

(multi format-list :window
       (fn [phrase-mapping window-list]
         (map #[$.id
                (or (. phrase-mapping $.id) "")
                $.process-name
                $.title]
              window-list)))

(multi format-list :action
       (fn [phrase-mapping action-list]
         (map #[$.id
                (or (. phrase-mapping $.id) "")
                $.name
                $.title]
              action-list)))

(var active-list-idx 1)

(lambda apply-filter [filter-text tree-widget lists]
  (let [{: items} (. lists active-list-idx)
        ;; clone the list so our sorting doesn't effect the outer list
        item-list (stable.clone items)
        filtered-list (if (> (string.len filter-text) 0)
                          (->> item-list
                               (map-preserve-meta
                                #(assoc $ :matches (fuzzy-search $.full-name filter-text)))
                               (filter #(> (length $.matches) 0)))
                          item-list)
        filtered-list (-> filtered-list
                          (pre-sort-list)
                          (sort-by-match-score filter-text))
        selected-idx (reduce-kv (fn [idx item-idx item]
                                  (if (= filter-text (. phrase-mapping item.id))
                                      item-idx
                                      idx))
                                1
                                filtered-list)]
    (tree-widget.store:clear)
    (->> filtered-list
         (format-list phrase-mapping)
         (map #(tree-widget.store:append $)))
    (set-selection tree-widget selected-idx)))

(lambda string? [s]
  (= "string" (type s)))

(local columns {:id 1
                :phrase 2
                :name 3
                :description 4})

(var activated? false)
(var current-window nil)

(fn deactivate-window []
  "Destroys the window if it exists, then sets activated? to false to
alter the behavior of the main loop."
  (when current-window
    (current-window:destroy)
    (while (gtk.events_pending)
      (gtk.main_iteration)))
  (set current-window nil)
  (set activated? false))

(lambda activate-list-item [id lists]
  (let [active-list (. lists active-list-idx)
        {: activator} active-list]
    (activator id active-list)))

(lambda activate-selection [buffer selection lists]
  (let [(model iter) (selection:get_selected)]
    (when model
      (let [id (-> model (. iter) (. columns.id))]
        (activate-list-item id lists)
        (when (> (length buffer.text) 0)
          ;; support bidirectional lookup
          (tset phrase-mapping buffer.text id)
          (tset phrase-mapping id buffer.text)))
      (deactivate-window))))

(lambda rotate-selection [dir tree-widget]
  (let [selection (tree-widget.view:get_selection)
        curr-row (first (selection:get_selected_rows))
        ;; converting from gtk 0-indexing to lua 1-indexing
        row-idx (+ 1 (first (curr-row:get_indices)))
        new-row-idx (if (= dir :down)
                        (+ row-idx 1)
                        (- row-idx 1))
        total-rows (tree-widget.store:iter_n_children)
        new-row-idx (if (> new-row-idx total-rows)
                        1

                        (< new-row-idx 1)
                        total-rows

                        new-row-idx)]
    (set-selection tree-widget new-row-idx)
    ;; return true to abort further event processing
    true))

(lambda rotate-active-list [label filter-fn lists]
  (let [next-idx (+ 1 active-list-idx)
        next-idx (if (> next-idx (length lists)) 1 next-idx)
        {: name} (. lists next-idx)]
    (label:set_text name)
    (set active-list-idx next-idx)
    (filter-fn)
    ;; return true to abort further event processing
    true))

(lambda handle-key-press-fn [key-press-handlers]
  (fn [_window event]
    (let [handler (. key-press-handlers event.keyval)]
      (if handler
          (handler)
          false))))

(lambda activate-window [id _window-list]
  (xc:activate_win id))

(lambda activate-action [id action-list]
  (let [{: items} action-list
        {: action} (->> items
                        (filter #(= $.id id))
                        (first))]
    (os.execute action)))

(lambda make-window [window-list action-list]
  (let [list-store (gtk.ListStore.new {columns.id lgi.GObject.Type.STRING
                                       columns.phrase lgi.GObject.Type.STRING
                                       columns.name lgi.GObject.Type.STRING
                                       columns.description lgi.GObject.Type.STRING})
        tree-widget {:store list-store
                     :view (gtk.TreeView
                            {:id :view
                             :model list-store
                             :headers-visible false
                             1 (italic-text-column "Phrase" columns.phrase)
                             2 (text-column "Name" columns.name)
                             3 (text-column "Description" columns.description)})}
        lists [{:name "Windows"
                :activator activate-window
                :items window-list}
               {:name "Actions"
                :activator activate-action
                :items action-list}]
        buffer (gtk.EntryBuffer)
        filter-fn #(apply-filter buffer.text tree-widget lists)
        label (gtk.Label.new "Windows")
        key-press-handlers {gdk.KEY_Escape #(deactivate-window)
                            gdk.KEY_Return #(activate-selection
                                             buffer (tree-widget.view:get_selection)
                                             lists)
                            gdk.KEY_Up #(rotate-selection :up tree-widget)
                            gdk.KEY_Down #(rotate-selection :down tree-widget)
                            gdk.KEY_Tab #(rotate-active-list
                                          label
                                          filter-fn
                                          lists)}
        window (doto (gtk.Window
                      {:title "QuickWin"
                       :default_width 500
                       ;;:default_height 300
                       :decorated false
                       :window_position gtk.WindowPosition.CENTER_ALWAYS
                       :on_key_press_event (handle-key-press-fn key-press-handlers)
                       1 (gtk.Box
                          {:orientation :VERTICAL
                           :spacing 3
                           1 label 
                           2 (gtk.Entry {:id :filter
                                         : buffer})
                           3 tree-widget.view})}))]
    (set buffer.on_inserted_text filter-fn)
    (set buffer.on_deleted_text filter-fn)
    (tree-widget.view:set_activate_on_single_click true)
    (set tree-widget.view.on_row_activated
         (fn [_ _ _]
           (activate-selection buffer 
                               (tree-widget.view:get_selection)
                               lists)))
    (window:set_keep_above true)
    (set active-list-idx 1)
    ;; populates the list store with everyting
    (filter-fn)
    window))

(local better-process-names
       ;; Firefox 79 reports as "MainThread" now
       {"MainThread" "firefox"
        "soffice.bin" "libreoffice"})

(lambda process-name [pid]
  "Looks up the name for a process from /proc/<pid>/stat"
  (let [(_ _ name) (-> (.. "/proc/" pid "/stat")
                       (sio.readlines)
                       (first)
                       (string.find (.. pid " %((.+)%)")))]
    (or (. better-process-names name)
        name)))

(lambda window-list []
  "Returns a list of \"normal\" windows as tables with details."
  (let [l (->> (xc:get_win_list)
               (map #{:id (tostring $) :type (xc:get_win_type $)})
               (filter #(= "normal" (. $ :type)))
               (map #(let [pid (xc:get_pid_of_win $.id)
                           process-name (process-name pid)
                           title (xc:get_win_title $.id)]
                       (stable.merge $ {: pid
                                        : process-name
                                        : title
                                        :full-name (.. process-name " " title)}))))]
    (setmetatable l {:type :window})
    l))

;; TODO: store this in a configuration file
(lambda action-list []
  "Returns a list of available actions."
  (let [l (->> [{:id :play
                 :name "play/pause"
                 :title "Play or pause active music player"
                 :action "/usr/bin/xdotool key XF86AudioPlay"}
                {:id :next-track
                 :name "next track"
                 :title "Skip to next track"
                 :action "/usr/bin/xdotool key XF86AudioNext"}
                {:id :prev-track
                 :name "previous track"
                 :title "Skip to previous track"
                 :action "/usr/bin/xdotool key XF86AudioPrev"}
                {:id :volume-up
                 :name "volume up"
                 :title "Increase output volume"
                 :action "/usr/bin/xdotool key XF86AudioRaiseVolume"}
                {:id :volume-down
                 :name "volume down"
                 :title "Decrease output volume"
                 :action "/usr/bin/xdotool key XF86AudioLowerVolume"}
                {:id :mute
                 :name "mute"
                 :title "Mute output volume"
                 :action "/usr/bin/xdotool key XF86AudioMute"}
                {:id :lock
                 :name "lock screen"
                 :title "Lock the screen"
                 :action "/usr/bin/xflock4"}]
               (map #(stable.merge $ {:full-name (.. $.name " " $.title)})))]
    (setmetatable l {:type :action})
    l))

(lambda init-dbus [bus]
  (dbus.bus.add_match bus (.. "type='signal',interface='" dbus-name "'"))
  (bus:flush))

(lambda activate-message-received? [bus]
  (let [msg (bus:pop_message)]
    (and msg
         (= dbus-activate-path (msg:get_path)))))

(fn create-and-show-window-if-needed []
  (when (not current-window)
    (set current-window (make-window (window-list) (action-list)))
    (current-window:show_all)))

(fn maybe-focus-window []
  (when current-window
    (current-window:present)))

(lambda run-app-loop [bus]
  (init-dbus bus)
  (set activated? true) ;; always show window on init
  ;; This is the main loop. It will interleave gtk event handling with
  ;; dbus event handling in order to consume any dbus activate events
  ;; that arrive while the window is visible.
  (while (bus:read_write 0)
    (when (activate-message-received? bus)
      (set activated? true)
      (maybe-focus-window))
    (if activated?
        (do
          (create-and-show-window-if-needed)
          ;; run a single iteration of the event loop, but don't block
          ;; if there are no pending events
          (gtk.main_iteration_do false))
        (socket.sleep 0.1))))

(lambda init [bus]
  (start-xevent-listener-thread)
  (run-app-loop bus))

{: init}

