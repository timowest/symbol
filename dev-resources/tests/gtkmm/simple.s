(ns simple
  (include "gtkmm.h"))

(defn main [^int argc ^char** argv]
  (let [app (.create Gtk/Application argc argv
                                    "org.gtkmm.examples.base")
        window (Gtk/ApplicationWindow.)]
    (.run app (pref window))))