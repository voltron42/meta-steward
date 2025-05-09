(ns meta-steward.mp4 
  (:require
   [clojure.java.io :as io]) 
  (:import
   [java.io File FileNotFoundException RandomAccessFile]
   [java.nio ByteBuffer]
   [java.nio.channels Channels]
   [java.nio.file Paths]
   [meta_steward.java BetterByteArrayOutputStream]
   [org.mp4parser Container IsoFile]
   [org.mp4parser.boxes.apple AppleItemListBox AppleNameBox]
   [org.mp4parser.boxes.iso14496.part12
    FreeBox
    HandlerBox
    MetaBox
    MovieBox
    UserDataBox]))

(defn- findFreeBox [container]
  (loop [allBoxes (.getBoxes container)]
    (if (empty? allBoxes)
      nil
      (let [box (first allBoxes)]
        (if (instance? FreeBox box)
          box
          (if (instance? Container box)
            (let [temp (findFreeBox box)]
              (if-not (nil? temp)
                temp
                (recur (rest allBoxes))))
            (recur (rest allBoxes))))))))

(defn- needsOffsetCorrection [isoFile]
  (if-not (nil? (Paths/get isoFile "moov[0][/mvex[0]"))
    false
    (let [result (loop [boxes (.getBoxes isoFile)]
                   (if (empty? boxes)
                     {}
                     (if (= "moov" (.getType (first boxes)))
                       {:result true}
                       (if (= "mdat" (.getType (first boxes)))
                         {:result false}
                         (recur (rest boxes))))))]
      (if (contains? result :result)
        (:result result)
        (throw (Exception. "I need moov or mdat. Otherwise all this doesn't make sense"))))))

(defn- correctChunkOffsets [movieBox correction]
  (let [chunkOffsetBoxes (Paths/get movieBox "trak/mdia[0]/minf[0]/stbl[0]/stco[0]")
        chunkOffsetBoxes (if-not (empty? chunkOffsetBoxes)
                           chunkOffsetBoxes
                           (Paths/get movieBox "trak/mdia[0]/minf[0]/stbl[0]/st64[0]"))]
    (doseq [chunkOffsetBox chunkOffsetBoxes]
      (.setChunkOffsets
       chunkOffsetBox
       (map #(+ correction %)
            (.getChunkOffsets chunkOffsetBox))))))

(defn- splitFileAndInsert [file position length]
  (let [read (.getChannel (RandomAccessFile. file "r"))
        tmp (File/createTempFile "ChangeMetaData" "splitFileAndInsert")
        tmpWrite (.getChannel (RandomAccessFile. tmp "rw"))]
    (.position read position)
    (.transferFrom tmpWrite read 0 (- (.size read) position))
    (.close read)
    (let [write (.getChannel (RandomAccessFile. file "rw"))]
      (.position write (+ position length))
      (.position tmpWrite 0)
      (.close tmpWrite)
      (.delete tmp)
      write)))

(defn get-title-et-al [filePath]
  (let [videoFile (io/file filePath)]
    (when (not (.exists videoFile))
      (throw (FileNotFoundException. (str "File " filePath " not exists"))))
    (when (not (.canWrite videoFile))
      (throw (FileNotFoundException. (str "No write permissions to file " filePath))))
    (let [isoFile (IsoFile. filePath)
          moov (-> isoFile (.getBoxes (.class MovieBox)) (.get 0))
          freeBox (findFreeBox moov)
          correctOffset (needsOffsetCorrection isoFile)
          sizeBefore (.getSize moov)
          offset (count (filter #(= "moov" (.getType %)) (.getBoxes isoFile)))
          userDataBox (let [udtaPath (Paths/get moov "udta")]
                        (if-not (nil? udtaPath)
                          udtaPath
                          (let [temp (UserDataBox.)]
                            (.addBox moov temp)
                            temp)))
          metaBox (let [mb (Paths/get userDataBox "meta")]
                    (if-not (nil? mb)
                      mb
                      (let [temp (MetaBox.)
                            hdlr (HandlerBox.)]
                        (.setHandlerType hdlr "mdir")
                        (.addBox temp hdlr)
                        (.addBox userDataBox temp))))
          ilst (let [listBox (Paths/get metaBox "ilst")]
                 (if-not (nil? listBox)
                   listBox
                   (let [temp (AppleItemListBox.)]
                     (.addBox metaBox temp)
                     temp)))
          freeBox (if-not (nil? freeBox)
                    freeBox
                    (let [temp (FreeBox. (* 128 1024))]
                      (.addBox metaBox freeBox)
                      temp))
          nam (let [temp (Paths/get ilst "@nam")]
                (if-not (nil? temp) temp (AppleNameBox.)))]
      [videoFile isoFile moov freeBox correctOffset sizeBefore offset ilst nam])))

(defn get-title [filePath]
  (let [title (.getValue (last (get-title-et-al filePath)))
        title (if (or (nil? title) (empty? title)) "<no title>" title)]
    (printf "title: %s, filepath: %s" title filePath)))

(defn remove-title [filePath]
  (let [[videoFile isoFile moov freeBox correctOffset sizeBefore offset ilst nam] (get-title-et-al filePath)]
     (doto nam
       (.setDataCountry 0)
       (.setDataLanguage 0)
       (.setValue ""))
     (.addBox ilst nam)
     (let [sizeAfter (.getSize moov)
           diff (- sizeAfter sizeBefore)
           freeBoxLimit (int (-> freeBox (.getData) (.limit)))
           limitDiff (- freeBoxLimit diff)
           [sizeAfter diff] (if-not (pos? limitDiff)
                              [sizeAfter diff]
                              (do
                                (-> freeBox (.setData (ByteBuffer/allocate limitDiff)))
                                (let [sizeAfter (.getSize moov)
                                      diff (- sizeAfter sizeBefore)]
                                  [sizeAfter diff])))]
       (when (and correctOffset (not= 0 diff))
         (correctChunkOffsets moov diff))
       (let [baos (BetterByteArrayOutputStream.)]
         (.getBox moov (Channels/newChannel baos))
         (.close isoFile)
         (let [fc (if (zero? diff)
                    (.getChannel (RandomAccessFile. videoFile "rw"))
                    (splitFileAndInsert videoFile offset (- sizeAfter sizeBefore)))]
           (doto fc
             (.position offset)
             (.write (ByteBuffer/wrap (.getBuffer baos) 0 (.size baos)))
             (.close)))))))