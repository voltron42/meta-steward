(ns meta-steward.core
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [meta-steward.mp3 :as mp3]
   [meta-steward.mp4 :as mp4]))

(def extensions
  {:remove-title ".mp4"
   :view-mp4-title ".mp4"
   :view-mp3-metadata ".mp3"
   :update-mp3-meta-from-path ".mp3"})

(defmulti steward #(first %&))

(defmethod steward :remove-title [_ filePath]
  (mp4/remove-title filePath))

(defmethod steward :view-mp4-title [_ filePath]
  (mp4/get-title filePath))

(defmethod steward :view-mp3-metadata [_ filePath]
  (mp3/view-mp3-metadata filePath))

(defmethod steward :update-mp3-meta-from-path [_ filePath]
  ; todo
  )

(defmulti steward-by-option #(first %&))

(defn deep-steward-recurse [file operation extension]
  (if (-> file (.getName) (.endsWith extension))
    (steward operation (.getAbsolutePath file))
    (when (.isDirectory file)
      (doseq [child (.listFiles file)]
        (deep-steward-recurse child operation extension)))))

(defmethod steward-by-option :deep [_ operation filePath]
  (let [extension (get extensions operation)]
    (deep-steward-recurse (io/file filePath) operation extension)))

(defmethod steward-by-option :default [_ operation filePath]
  (steward operation filePath))

(defn- parse-args [[operation option file-path]]
  (let [[option file-path] (if (nil? file-path) ["nil" option] [option file-path])]
    (map read-string [operation option file-path])))

(defn -main
  [& args]
  (let [[operation option file-path] (parse-args args)]
    (steward-by-option option operation file-path)))
