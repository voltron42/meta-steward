(ns meta-steward.core
  (:gen-class)
  (:import
   [java.io RandomAccessFile]
   [com.coremedia.iso IsoFile]
   [com.coremedia.iso.boxes UserDataBox MetaBox]
   [com.coremedia.iso.boxes.apple AppleItemListBox AppleStringBox]))

(defmulti steward #(first %&))

(defmethod steward :remove-title [_ filePath]
  (let [raf (RandomAccessFile. filePath "rw")
        isoFile (IsoFile. (.getChannel raf))
        moov (.getMovieBox isoFile)
        udta (-> moov (.getBoxes (.class UserDataBox)) (.get 0))
        meta (-> udta (.getBoxes (.class MetaBox)) (.get 0))
        ilst (-> meta (.getBoxes (.class AppleItemListBox)) (.get 0))
        titleBoxes (-> ilst (.getBoxes (.class AppleStringBox) "@nam"))]
    (when (not (.isEmpty titleBoxes))
      (.removeBox ilst (.get titleBoxes 0))
      (-> raf (.getChannel) (.position 0))
      (-> isoFile (.writeContainer (-> raf .getChannel))))
    (.close isoFile)))

(defmethod steward :update-mp3-meta-from-path [_ filePath]
 )

(defn -main
  [& args]
  (apply steward (map read-string args)))
