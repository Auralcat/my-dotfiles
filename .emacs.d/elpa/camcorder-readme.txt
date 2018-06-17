  Tool for capturing screencasts directly from Emacs.

  • To use it, simply call `M-x camcorder-record'.
  • A new smaller frame will popup and recording starts.
  • When you're finished, hit `F12' and wait for the conversion to
    finish.

  Screencasts can be generated in any format understood by
  `imagemagick''s `convert' command. You can even pause the recording
  with `F11'!

  If you want to record without a popup frame, use `M-x
  camcorder-mode'.

Dependencies
────────────

  `camcorder.el' uses the [*Names*] package, so if you're installing
  manually you need to install that too.

  For the recording, `camcorder.el' uses the following linux utilities.
  If you have these, it should work out of the box. If you use something
  else, you should still be able to configure `camcorder.el' work.

  • recordmydesktop
  • mplayer
  • imagemagick

  Do you know of a way to make it work with less dependencies? *Open an
  issue and let me know!*


  [*Names*] https://github.com/Bruce-Connor/names/

Troubleshooting
───────────────

  On my machine, I noticed that the window-id Emacs reported
  ┌────
  │ (format "%x"
  │   (string-to-number
  │    (frame-parameter (selected-frame) 'window-id)))
  └────
  differed from the id that the WM reported with the `wminfo' utility. I
  added the variable `camcorder-window-id-offset' to correct that. The
  default value is -4, but you might need to increase or decrease that
  to make those two numbers match.
