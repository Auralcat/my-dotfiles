(ert-deftest addition-test ()
  (should (= (+ 1 2) 3)))

;; Run this with (ert 'elixir-module-name-test)
(ert-deftest elixir-module-name-test ()
  (should (eq
           (elixir-dir-to-module-string "/foo/bar/")
           "Foo.Bar.")))

(defun elixir-dir-to-module-string (dirpath)
  "Converts an absolute directory path name to an Elixir module name reference."
  (concat (mapconcat 'capitalize (split-string dirpath "\[_\/\]" t) ".") ".")
  )
