(ns meta-steward.mp3)

(defn view-mp3-metadata [filePath]
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
