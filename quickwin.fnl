(local sfun (require :std.functional))
(local sio (require :std.io))
(local lgi (require :lgi))
(local view (require :fennelview))
(local std (require :std))
(local stable (require :std.table))
(local xctrl (require :xctrl))

(local gtk (lgi.require "Gtk" "3.0"))
(local gdk (lgi.require "Gdk" "3.0"))

;; hack to work around singleton restriction. From the xctrl docs:
;; Note that only one instance of the xctrl object at a time is
;; allowed to be in use. If for some reason you need to re-initialize
;; a new object, you should set the old one to nil and call
;; collectgarbage() twice before trying to create another new
;; object. This will help insure that the first instance is properly
;; cleaned up.
(when (not (. _G :xc))
  (global xc (xctrl.new)))

(lambda pp [...] (print (view [...])))

(lambda map [f xs]
  (sfun.map f std.elems xs))

(lambda filter [f xs]
  (sfun.filter f std.elems xs))

(lambda reduce [f init xs]
  (sfun.reduce f init std.elems xs))

(lambda assoc [t k v]
  (tset t k v)
  t)

(lambda first [xs]
  (. xs 1))

(lambda last [xs]
  (. xs (length xs)))

(lambda bold [start end]
  (doto (lgi.Pango.Attribute.weight_new lgi.Pango.Weight.ULTRAHEAVY)
    (tset :start_index start)
    (tset :end_index end)))

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

(lambda fuzzy-search-re [text filter-s st]
  (let [(start end) (text:find (table.concat (str->list filter-s) ".-") st)]
    (if (not start)
        []

        (and end (< (length text) end))
        [[start end]]

        (let [more (fuzzy-search text filter-s end)]
          (table.insert more 1 [start end])
          more))))

(lambda range [start end]
  (let [r []]
    (for [i start end]
      (table.insert r i))
    r))

(lambda match->positions [text filter-s [start end]]
  (if (= (+ 1 (- end start)) (length filter-s))
      (range start end)

      ))

(lambda matches->positions [text filter-s matches]
  ;; if len matches == len filter-s, done!
  ;;
  )

(lambda text-column [text pos]
  (let [renderer (gtk.CellRendererText)
        col (gtk.TreeViewColumn
             {:title text
              1 [renderer
                 {:text pos}]})]
    (col:set renderer
             (fn [_tree-c cell-r _model _iter]
               (let [attr-list (lgi.Pango.AttrList.new)]
                 (attr-list:insert (bold 0 2))
                 (tset cell-r :attributes attr-list))))
    col))

(lambda match-score [filter-text win]
  "Scores a match set. The score is the number of matches + a boosting
factor for the compactness of the matches related to the length of the
filter-text."
  (if win.matches
      (let [filter-len (length filter-text)
            base-score (length win.matches)
            score
            (reduce #(+ $1 (/ 10 (+ 1 (- filter-len
                                         (- (last $2) (first $2))))))
                    base-score win.matches)]
        (print filter-text win.full-name score)
        score)

      0))

(lambda sort-windows [filter-text window-list]
  (print (length window-list))
  (stable.sort window-list
               #(if (and $1 $2)
                    (let [score-1 (match-score filter-text $1)
                          score-2 (match-score filter-text $2)]
                      (if (and score-1 score-2)
                          (> score-1 score-2) ;; reverse sort
                          (< $1.full-name $2.full-name)))
                    false)))

(lambda apply-filter [filter-text tree-view window-list list-store]
  (let [window-list (stable.clone window-list)
        filtered-win-list (if (> (string.len filter-text) 0)
                              (->> window-list
                                   (map #(assoc $ :matches (fuzzy-search $.full-name filter-text)))
                                   (filter #(> (length $.matches) 0)))
                              window-list)
        selection (tree-view:get_selection)]
    (list-store:clear)
    (->> filtered-win-list
         (sort-windows filter-text)
         (map #(list-store:append [$.id $.full-name])))
    (selection:select_path (gtk.TreePath.new_from_string "0"))))

(local columns {:id 1
                :title 2})

(lambda activate-selection [window selection]
  (let [(model iter) (selection:get_selected)]
    (when model
      (xc:activate_win (. (. model iter) columns.id))
      (window:destroy))))

(lambda handle-key-press [tree-view window event]
  (if (= gdk.KEY_Escape event.keyval)
      (window:destroy)
      (= gdk.KEY_Return event.keyval)
      (activate-selection window (tree-view:get_selection))
      false))

(lambda window [window-list]
  (let [list-store (gtk.ListStore.new {columns.id lgi.GObject.Type.INT64
                                       columns.title lgi.GObject.Type.STRING})
        buffer (gtk.EntryBuffer)
        tree-view (gtk.TreeView
                   {:id :view
                    :model list-store
                    1 (text-column "Window" columns.title)})
        filter-fn #(apply-filter buffer.text tree-view window-list list-store)]
    (set buffer.on_inserted_text filter-fn)
    (set buffer.on_deleted_text filter-fn)
    ;; populates the list store with everyting
    (apply-filter "" tree-view window-list list-store)

    (gtk.Window
     {:title "QuickWin"
      :default_width 400
      :default_height 300
      :on_destroy gtk.main_quit
      :on_key_press_event (partial handle-key-press tree-view)
      1 (gtk.Box
         {:orientation :VERTICAL
          :spacing 5
          1 (gtk.Entry {:id :filter
                        :buffer buffer})
          2 tree-view})})))

(lambda process-name [pid]
  "Looks up the name for a process from /proc/<pid>/stat"
  (let [(_ _ name) (-> (.. "/proc/" pid "/stat")
                       (sio.readlines)
                       (first)
                       (string.find (.. pid " %((.+)%)")))]
    name))

(lambda window-list []
  "Returns a list of \"normal\" windows as tables with details."
  (->> (xc:get_win_list)
       (map #{:id $ :type (xc:get_win_type $)})
       (filter #(= "normal" (. $ :type)))
       (map #(let [pid (xc:get_pid_of_win $.id)
                   process-name (process-name pid)
                   win-title (xc:get_win_title $.id)]
               (stable.merge $ {:pid pid
                                :process-name process-name
                                :title win-title
                                :full-name (.. process-name " | " win-title)})))))

(lambda run []
  (let [w (window (window-list))]
    (w:show_all))
  (gtk.main))

;; --- tests ---

(lambda fuzzy-search-test [lu]
  (pp nil)
  (lu.assertEquals true false))

{: run
 :tests [fuzzy-search-test]}

