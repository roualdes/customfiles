#!/usr/bin/env bb

(require '[clojure.java.shell :refer [sh]]
         '[clojure.tools.cli :refer [parse-opts]]
         '[clojure.string :as string]
         '[clojure.java.io :as io]
         '[clojure.pprint :refer [pprint]])

(def cli-options
  [["-v" "--verbose" "Print on goings"]
   ["-n" "--dry-run" "Do a dry run before living dangerously"]
   ["-t" "--test" "Test script by echoing the command that would otherwise be run"]
   ["-d" "--delete-destination" "Delete files on destinaton, if they don't exist on source"]
   ["-h" "--help"]])

(defn usage [options-summary]
  (->> ["Push (local -> remote) or pull (remote -> local) files within ~/dos/"
        ""
        "Usage: rs [options] action"
        ""
        "The following example will dry run push"
        ""
        "Example: rs -n push"
        ""
        "Options:"
        options-summary
        ""
        "Actions:"
        "  push    Push to remote storage"
        "  pull    Pull from remote storage"
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
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) ; help => exit OK with usage summary
      {:exit-message (usage summary) :ok? true}
      errors ; errors => exit with description of errors
      {:exit-message (error-msg errors)}
      (and (= 1 (count arguments))
           (#{"push" "pull"} (first arguments)))
      {:action (first arguments) :command ["rsync" "--rsh=ssh"] :options options}
      :else ; failed  custom validation => exit with usage summary
      {:exit-message (usage summary)})))

(defn exit [status msg]
  (println msg)
  (System/exit status))

(defn add-action [valargs]
  (let [action (:action valargs)]
    (assoc valargs :command
           (conj (:command valargs) action))))

(defn test-flag [valargs]
  (if (:test (:options valargs))
    (assoc valargs :command
           (vec (cons "echo" (:command valargs))))
    valargs))

(defn push-flags [valargs]
  (assoc valargs :command
         (conj (:command valargs)
               "--exclude=\"node_modules/\""
               "--exclude=\".DS_Store\""
               "--exclude=\".p.kdbx.lock\""
               "--exclude=\".Rhistory\""
               "--exclude=\".juliahistory\""
               "--exclude=\"*_files\"")))

(defn dry-run-flag [valargs]
  (if (get-in valargs [:options :dry-run])
    (assoc valargs :command
           (conj (:command valargs) "--dry-run"))
  valargs))

(defn verbose-flag [valargs]
  (if (get-in valargs [:options :verbose])
    (assoc valargs :command
           (conj (:command valargs) "--verbose"))
    valargs))

(defn add-local [valargs]
  (let [home (System/getProperty "user.home")]
    (assoc valargs :command
           (conj (:command valargs) (str home "/dos/")))))

(defn add-remote [valargs]
  (assoc valargs :command
         (conj (:command valargs) "kauai:/mnt/dos/")))

(defn print-command [valargs]
  (let [cmd (:command valargs)]
    (pprint cmd))
  valargs)

(defn print-valargs [valargs]
  (pprint valargs)
  valargs)

(defn push-command [valargs]
  (-> valargs
      test-flag
      verbose-flag
      dry-run-flag
      push-flags
      add-action
      add-local
      add-remote))

(defn pull-command [valargs]
  (-> valargs
      test-flag
      verbose-flag
      dry-run-flag
      add-remote
      add-local))

(defn form-command [valargs]
  (let [action (:action valargs)]
    (case action
      "push" (push-command valargs)
      "pull" (pull-command valargs))))

(defn run-command [valargs]
  (let [cmd (:command (form-command valargs))]
    (println (:out (apply sh cmd)))))

(defn run [args]
  (let [valargs (validate-args args)
        {:keys [exit-message ok? action]} valargs]
    (if exit-message
      (exit (if ok? 0 1) exit-message)
      (run-command valargs))))

(run *command-line-args*)
