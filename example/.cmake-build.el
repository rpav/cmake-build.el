((cmake-build-cmake-profiles
  (release "-DCMAKE_BUILD_TYPE=Release")
  (debug "-DCMAKE_BUILD_TYPE=Debug"))
 (cmake-build-other-targets "deploy")
 (cmake-build-run-configs
  (mytest
   (:build "mytest")
   (:run "" "./mytest" "--with-cmake-build-el"))
  (mytest-with-data
   (:build "mytest")
   (:run "" "./mytest" "--with-cmake-build-el data.txt"))
  (mytest-output
   (:build "mytest")
   (:run "" "./mytest" "--with-cmake-build-el data.txt -o output.txt"))))
