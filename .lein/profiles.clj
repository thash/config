{:user  {:plugins  [
                    ;; [lein-droid "0.2.0"]
                    [lein-localrepo "0.5.3"]
                    [venantius/ultra "0.5.1"]
                    [nightlight/lein-nightlight "1.6.1"]
                    ]
         ;; :android {:sdk-path "/Users/hash/work/android/sdk/"
         ;;           ;; :keystore-path "/Users/hash/.android/memerelics.keystore"
         ;;           }
         :search-page-size 30
         :jvm-opts ["-XX:+TieredCompilation" "-XX:TieredStopAtLevel=1"]}}
