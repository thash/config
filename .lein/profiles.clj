{:user  {:plugins  [
                    [lein-droid "0.2.0-preview4"] ;; latest 0.2.0 causes instant killing.
                    ]
         :android {:sdk-path "/Users/hash/work/android/sdk/"}
         :search-page-size 30
         :jvm-opts ["-XX:+TieredCompilation" "-XX:TieredStopAtLevel=1"]}}
