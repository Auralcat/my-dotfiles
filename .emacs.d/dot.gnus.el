;; Gnus configuration file.
;; Put this in your home folder.

;; You can set this info in the terminal as well, exporting the variables
;; NAME and EMAIL
(setq user-mail-address "you@your_email_address.com"
      user-full-name "Your full name")

;; Adding some servers to fetch news from
(setq gnus-select-method '(nntp "foo.bar.com"))
(add-to-list 'gnus-secondary-select-methods '(nntp "localhost"))
(add-to-list 'gnus-secondary-select-methods '(nntp "news.gnus.org"))
(add-to-list 'gnus-secondary-select-methods '(nnml ""))

(setq mail-sources '((pop :server "pop.provider.org"
                          :user "you"
                          :password "secret")))
