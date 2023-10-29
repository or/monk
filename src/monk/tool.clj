(ns monk.tool
  (:require
   [clojure.stacktrace :as st]
   [monk.core :as monk]
   [monk.diff :as diff]
   [monk.io :as io]))

(def ^:dynamic *no-output*
  false)

(def ^:dynamic *verbose*
  false)

(defn- warn [& args]
  (when-not *no-output*
    (binding [*out* *err*]
      (apply println args))))

(defn- trace [& args]
  (when *verbose*
    (apply warn args)))

(defn- grep [re dir]
  (filter #(re-find re (io/relative-path % dir))
          (io/list-files dir)))

(defn- find-files [f]
  (let [f (io/file-entity f)]
    (when (io/exists? f)
      (if (io/directory? f)
        (grep #"\.clj[csx]?$" f)
        (list f)))))

(defn- format-diff
  [options file original revised]
  (let [diff (diff/unified-diff file original revised)]
    (if (:color? options)
      (diff/colorize-diff diff)
      diff)))

(def ^:private zero-counts
  {:okay 0
   :incorrect 0
   :error 0})

(defn- check-one [options file]
  (trace "Processing file:" file)
  (let [status {:counts zero-counts
                :file file}]
    (try
      (let [original (io/read-file file)
            revised (str (monk/format-string original options) "\n")]
        (if (= original revised)
          (assoc-in status [:counts :okay] 1)
          (-> status
              (assoc-in [:counts :incorrect] 1)
              (assoc :diff (format-diff options file original revised)))))
      (catch Exception ex
        (-> status
            (assoc-in [:counts :error] 1)
            (assoc :exception ex))))))

(defn- print-stack-trace [ex]
  (when-not *no-output*
    (binding [*out* *err*]
      (st/print-stack-trace ex))))

(defn- print-file-status [status]
  (let [path (:file status)]
    (when-let [ex (:exception status)]
      (warn "Failed to format file:" path)
      (print-stack-trace ex))
    (when (:reformatted status)
      (warn "Reformatted" path))
    (when-let [diff (:diff status)]
      (warn path "has incorrect formatting")
      (println diff))))

(defn- exit [counts]
  (when-not (zero? (:error counts 0))
    (System/exit 2))
  (when-not (zero? (:incorrect counts 0))
    (System/exit 1)))

(defn- print-final-count [counts]
  (let [error (:error counts 0)
        incorrect (:incorrect counts 0)]
    (when-not (zero? error)
      (warn error "file(s) could not be parsed for formatting"))
    (when-not (zero? incorrect)
      (warn incorrect "file(s) formatted incorrectly"))
    (when (and (zero? incorrect)
               (zero? error))
      (warn "All source files formatted correctly"))))

(defn- merge-counts
  ([]
   zero-counts)

  ([a]
   a)

  ([a b]
   (merge-with + a b)))

(defn check
  "Checks that the Clojure paths specified by the :paths option are correctly formatted."
  [paths options]
  (let [map* (if (:parallel? options)
               pmap
               map)
        counts (->> (set paths)
                    (mapcat find-files)
                    (map* (partial check-one options))
                    (map (fn [status]
                           (print-file-status status)
                           (:counts status)))
                    (reduce merge-counts))]
    (print-final-count counts)
    (exit counts)))

(defn- fix-one [options file]
  (trace "Processing file:" file)
  (try
    (let [original (io/read-file file)
          revised (str (monk/format-string original options) "\n")
          changed? (not= original revised)]
      (io/update-file file revised changed?)
      (cond-> {:file file}
        changed? (assoc :reformatted true)))
    (catch Exception e
      {:file file
       :exception e})))

(defn- recursively-find-files [paths]
  (mapcat find-files (set paths)))

(defn fix
  "Fixes the formatting for all files specified by the :paths option."
  [paths options]
  (let [files (recursively-find-files paths)
        map* (if (:parallel? options)
               pmap
               map)]
    (dorun
     (->> files
          (map* (partial fix-one options))
          (map print-file-status)))))
