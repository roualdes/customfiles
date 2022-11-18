#!/usr/bin/env bb

(require '[clojure.java.shell :refer [sh]]
         '[clojure.tools.cli :refer [parse-opts]]
         '[clojure.string :as string]
         '[clojure.java.io :as io])

(def cli-options
  [;; First three strings describe a short-option, long-option with optional
   ;; example argument description, and a description. All three are optional
   ;; and positional.
   ;; If no required argument description is given, the option is assumed to
   ;; be a boolean option defaulting to nil
   ["-n" "--dry-run" "Do a trial run with no permanent changes."]
   ["-l" "--log-file" "Log all of rclone's output to FILE."
    :default "~/rclone_logfile.txt"]
   ["-v" "--verbose" "Verbosity level; may be specified multiple times to increase value"
    :default 0
    :update-fn inc]
   ["-h" "--help"]])


(defn usage [options-summary]
  (->> ["This is my program. There are many like it, but this one is mine."
        ""
        "Usage: rc [options] path"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  sync    Sync up to DigitalOcean storage"
        "  cp      Copy down from DigitalOcean storage"
        ""]
       (string/join \newline)))

(defn error-msg [errors]
  (str "The following errors occurred while parsing your command:\n\n"
       (string/join \newline errors)))

(defn validate-args
  "Validate command line arguments. Either return a map indicating the program
  should exit (with an error message, and optional ok status), or a map
  indicating the action the program should take and the options provided."
  [args]
  (let [{:keys [options arguments errors summary]} args]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      (.exists (io/file (first arguments)))
      {:path (first arguments) :options options}
      :else ; failed  custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn run [args]
  (let [{:keys [path options exit-message ok?]} (validate-args args)]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (sh "ls" path))))

(run (parse-opts *command-line-args* cli-options))
