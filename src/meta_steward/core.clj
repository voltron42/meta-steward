(ns meta-steward.core
  (:gen-class)
  (:import
   [java.io RandomAccessFile]
   [com.coremedia.iso IsoFile]
   [com.coremedia.iso.boxes UserDataBox MetaBox]
   [com.coremedia.iso.boxes.apple AppleItemListBox AppleStringBox]
   [com.mpatric.mp3agic Mp3File]))

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

(defmethod steward :view-mp3-metadata [_ filePath]
  (let [mp3File (Mp3File. filePath)
        id3v2Tag (.getId3v2Tag mp3File)]
    (println "track: " (.getTrack id3v2Tag))
    (println "title: " (.getTitle id3v2Tag))
    (println "artist: " (.getArtist id3v2Tag))
    (println "album: " (.getAlbum id3v2Tag))
    (println "album artist: " (.getAlbumArtist id3v2Tag))
    (println "composer: " (.getComposer id3v2Tag))
    (println "original artist: " (.getOriginalArtist id3v2Tag))
    (println "year: " (.getYear id3v2Tag))
    (println "genre: " (.getGenre id3v2Tag) ": (" (.getGenreDescription id3v2Tag) ")")))

(defmethod steward :update-mp3-meta-from-path [_ filePath]
 )

(defn -main
  [& args]
  (apply steward (map read-string args)))
