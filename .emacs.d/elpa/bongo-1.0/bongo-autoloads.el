;;; bongo-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "bongo" "bongo.el" (23026 44090 241748 571000))
;;; Generated autoloads from bongo.el

(autoload 'bongo-start "bongo" "\
Start playing the current track in the nearest playlist buffer.
If there is no current track, perform the action appropriate for the current
  playback mode (for example, for regressive playback, play the last track).
However, if something is already playing, do nothing.
When called interactively and the current track is a stop action track,
  continue playback as if the action track had finished playing.
CALLED-INTERACTIVELY-P is non-nil when called interactively.

\(fn &optional CALLED-INTERACTIVELY-P)" t nil)

(autoload 'bongo-start/stop "bongo" "\
Start or stop playback in the nearest Bongo playlist buffer.
With prefix ARGUMENT, call `bongo-stop' even if already stopped.
CALLED-INTERACTIVELY-P is non-nil when called interactively.

\(fn &optional ARGUMENT CALLED-INTERACTIVELY-P)" t nil)

(autoload 'bongo-show "bongo" "\
Display what Bongo is playing in the minibuffer.
If INSERT-FLAG (prefix argument if interactive) is non-nil,
  insert the description at point.
Return the description string.

\(fn &optional INSERT-FLAG)" t nil)

(autoload 'bongo-playlist "bongo" "\
Switch to a Bongo playlist buffer.
See the function `bongo-playlist-buffer'.

\(fn)" t nil)

(autoload 'bongo-library "bongo" "\
Switch to a Bongo library buffer.
See the function `bongo-library-buffer'.

\(fn)" t nil)

(autoload 'bongo-switch-buffers "bongo" "\
In Bongo, switch from a playlist to a library, or vice versa.
With prefix argument PROMPT, prompt for the buffer to switch to.

\(fn &optional PROMPT)" t nil)

(autoload 'bongo "bongo" "\
Switch to a Bongo buffer.
See the function `bongo-buffer'.

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("bongo-pkg.el" "lastfm-submit.el") (23026
;;;;;;  44091 455494 603000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; bongo-autoloads.el ends here
