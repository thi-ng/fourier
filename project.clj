(defproject com.postspectacular/fourier "0.1.0-SNAPSHOT"
  :description "Audio analysis tools"
  :url "http://hg.postspectacular.com/fourier"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [com.googlecode.soundlibs/jlayer "1.0.1-1"]
                 [com.googlecode.soundlibs/mp3spi "1.9.5-1"]
                 [com.googlecode.soundlibs/tritonus-share "0.3.7-1"]
                 [net.sourceforge.jtransforms/jtransforms "2.4.0"]
                 [com.postspectacular/piksel "0.1.0-SNAPSHOT"]]
  :repositories {"nexus-3rd" {:url "http://50.17.220.26:8089/nexus/content/repositories/thirdparty/" :creds :gpg}
                 "nexus-releases" {:url "http://50.17.220.26:8089/nexus/content/repositories/postspectacular-releases/" :creds :gpg}
                 "nexus-snapshots" {:url "http://50.17.220.26:8089/nexus/content/repositories/postspectacular-snapshots/" :creds :gpg}}
  :jvm-opts ["-Xms512m" "-Xmx1g"])
