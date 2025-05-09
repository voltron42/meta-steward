(ns meta-steward.core-test
  (:require
   [clojure.test :as t]
   [meta-steward.core :as core]))

(t/deftest test-view-mp4-titles
  (t/testing "testing viewing mp4 titles"
    (core/steward-by-option :deep :view-mp4-title "resources/video")))

(t/deftest test-view-mp3-metadata
  (t/testing "testing viewing mp3 metadata"
    (core/steward-by-option :deep :view-mp3-metadata "resources/audio")))

