(ns monk.main
  (:require
   [clojure.tools.cli :as cli]
   [monk.tool :as tool]))

(def ^:const VERSION "0.0.1")
(def ^:dynamic *command* "monk")

(def ^:private default-cli-options
  {:parallel? false
   :color? true})

(defn- cli-options [defaults]
  [["-h" "--help"]
   [nil "--version"]
   ["-q" "--quiet"
    :id :quiet?]
   [nil "--[no-]color"
    :default (:color? defaults)
    :id :color?]
   [nil "--[no-]parallel"
    :id :parallel?
    :default (:parallel? defaults)]])

(defn- abort [& msg]
  (binding [*out* *err*]
    (when (seq msg)
      (apply println msg))
    (System/exit 1)))

(defn- print-help [summary]
  (println "Usage:")
  (println (str \tab *command* " (check | fix) [PATHS...]"))
  (println "Options:")
  (println summary))

(defn -main [& args]
  (let [parsed-opts (cli/parse-opts args (cli-options default-cli-options))
        [cmd & paths] (:arguments parsed-opts)
        options (:options parsed-opts)]
    (when (:errors parsed-opts)
      (abort (:errors parsed-opts)))
    (cond
      (:version options) (println *command* VERSION)

      (or (nil? cmd)
          (:help options)) (print-help (:summary parsed-opts))

      :else (let [action (case cmd
                           "check" tool/check
                           "fix" tool/fix
                           (abort "Unknown command:" cmd))]
              (binding [tool/*no-output* (:quiet? options)
                        tool/*verbose* (:verbose? options)]
                (action paths options))
              (when (:parallel? options)
                (shutdown-agents))))))
