(ns monk.main
  (:require
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.tools.cli :as cli]
   [monk.config :as config]
   [monk.tool :as tool])
  (:gen-class))

(def ^:const VERSION
  (-> (slurp (io/resource "VERSION"))
      str/trim
      (str/split #"\n")
      first))

(def ^:dynamic *command*
  "monk")

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
    (when-not tool/*profiling*
      (System/exit 1))))

(defn- print-help [summary]
  (println "Usage:")
  (println (str \tab *command* " (check | fix) [PATHS...]"))
  (println "Options:")
  (println summary))

(defn -main [& args]
  (let [parsed-opts (cli/parse-opts args (cli-options default-cli-options))
        config (config/load-from-file)
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
              (binding [tool/*no-output* (:quiet? options)]
                (action paths (assoc options :symbol-mapping (:format-as config))))
              (when (:parallel? options)
                (shutdown-agents))))))
