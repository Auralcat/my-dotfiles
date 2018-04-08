* description
ampc is a controller for the Music Player Daemon (http://mpd.wikia.com/).

** installation
If you use GNU ELPA, install ampc via M-x package-list-packages RET or
(package-install 'ampc).  Otherwise, grab the files in this repository and
put the Emacs Lisp ones somewhere in your load-path or add the directory the
files are in to it, e.g.:

(add-to-list 'load-path "~/.emacs.d/ampc")
(autoload 'ampc "ampc" nil t)

Byte-compile ampc (M-x byte-compile-file RET /path/to/ampc.el RET) to improve
its performance!

*** tagger
ampc is not only a frontend to MPD but also a full-blown audio file tagger.
To use this feature you have to build the backend application, `ampc_tagger',
which in turn uses TagLib (http://taglib.github.com/), a dual-licended
(LGPL/MPL) audio meta-data library written in C++.  TagLib has no
dependencies on its own.

To build `ampc_tagger', locate ampc_tagger.cpp.  The file can be found in the
directory in which this file, ampc.el, is located.  Compile the file and
either customize `ampc-tagger-executable' to point to the binary file or move
the executable in a suitable directory so Emacs finds it via consulting
`exec-path'.

g++ -O2 ampc_tagger.cpp -oampc_tagger -ltag && sudo cp ampc_tagger /usr/local/bin && rm ampc_tagger

You have to customize `ampc-tagger-music-directories' in order to use the
tagger.  This variable should be a list of directories in which your music
files are located.  Usually this list should have only one entry, the value
of your mpd.conf's `music_directory'.

If `ampc-tagger-backup-directory' is non-nil, the tagger saved copies of all
files that are about to be modified to this directory.  Emacs's regular
numeric backup filename syntax is used for the backup file names.  By default
`ampc-tagger-backup-directory' is set to "~/.emacs.d/ampc-backups/".

** usage
To invoke ampc call the command `ampc', e.g. via M-x ampc RET.  The first
argument to `ampc' is the host, the second is the port.  Both values default
to nil.  If nil, ampc will use the value specified in `ampc-default-server',
by default localhost:6600.  To make ampc use the full frame rather than the
selected window for its window setup, customise `ampc-use-full-frame' to a
non-nil value.

ampc offers independent views which expose different parts of the user
interface.  The current playlist view, the default view at startup, may be
accessed using the `J' key (that is `S-j').  The playlist view may be
accessed using the `K' key.  The outputs view may be accessed by pressing
`L'. The search view may be accessed using the `F' key (find).

*** current playlist view
The playlist view looks like this:

.........................
. 1      . 3  . 4  . 5  .
..........    .    .    .
. 2      .    .    .    .
.        .    .    .    .
.        .    .    .    .
.        ................
.        . 6            .
.        .              .
.........................

Window one exposes basic information about the daemon, such as the current
state (stop/play/pause), the song currently playing or the volume.

All windows, except the status window, contain a tabular list of items.  Each
item may be selected/marked.  There may be multiple selections.

To mark an entry, move the point to the entry and press `m' (ampc-mark).  To
unmark an entry, press `u' (ampc-unmark).  To unmark all entries, press `U'
(ampc-unmark-all).  To toggle marks, press `t' (ampc-toggle-marks).  Pressing
`<down-mouse-1>' with the mouse mouse cursor on a list entry will move point
to the entry and toggle the mark.  To navigate to the next entry, press `n'
(ampc-next-line).  Analogous, pressing `p' (ampc-previous-line) moves the
point to the previous entry.

Window two shows the current playlist.  The song that is currently played by
the daemon, if any, is highlighted.  To delete the selected songs from the
playlist, press `d' (ampc-delete).  Pressing `<down-mouse-3>' will move the
point to the entry under cursor and delete it from the playlist.  To move the
selected songs up, press `<up>' (ampc-up).  Analogous, press `<down>'
(ampc-down) to move the selected songs down.  Pressing `RET'
(ampc-play-this) or `<down-mouse-2>' will play the song at point/cursor.

Windows three to five are tag browsers.  You use them to narrow the song
database to certain songs.  Think of tag browsers as filters, analogous to
piping `grep' outputs through additional `grep' filters.  The property of the
songs that is filtered is displayed in the header line of the window.

Window six shows the songs that match the filters defined by windows three to
five.  To add the selected song to the playlist, press `a' (ampc-add).
Pressing `<down-mouse-3>' will move the point to the entry under the cursor
and execute `ampc-add'.  These key bindings works in tag browsers as well.
Calling `ampc-add' in a tag browser adds all songs filtered up to the
selected browser to the playlist.

The tag browsers of the current playlist view (accessed via `J') are `Genre'
(window 3), `Artist' (window 4) and `Album' (window 5).  The key `M' may be
used to fire up a slightly modified current playlist view.  There is no
difference to the default current playlist view other than that the tag
browsers filter to `Genre' (window 3), `Album' (window 4) and `Artist'
(window 5).  Metaphorically speaking, the order of the `grep' filters defined
by the tag browsers is different.

*** playlist view
The playlist view resembles the current playlist view.  The window, which
exposes the playlist content, is replaced by three windows, vertically
arragned, though.  The top one still shows the current playlist.  The bottom
one shows a list of stored playlists.  The middle window exposes the content
of the selected (stored) playlist.  All commands that used to work in the
current playlist view and modify the current playlist now modify the selected
(stored) playlist unless the point is within the current playlist buffer.
The list of stored playlists is the only view in ampc that may have only one
marked entry.

To queue a playlist, press `l' (ampc-load) or `<down-mouse-2>'.  To delete a
playlist, press `d' (ampc-delete-playlist) or `<down-mouse-3>'.  The command
`ampc-rename-playlist', bound to `r', can be used to rename a playlist.

Again, the key `<' may be used to setup a playlist view with a different
order of tag browsers.

*** outputs view
The outputs view contains a single list which shows the configured outputs of
MPD.  To toggle the enabled property of the selected outputs, press `a'
(ampc-toggle-output-enabled) or `<mouse-3>'.

*** search view
The search view contains the result of the last performed search. You can
start a new search with the `s' key while in the search view or use M-x
ampc-start-search. Use the `a' key to add a song displayed in result list.

** tagger
To start the tagging subsystem, press `I' (ampc-tagger).  This key binding
works in every buffer associated with ampc.  First, the command tries to
determine which files you want to tag.  The files are collected using either
the selected entries within the current buffer, the file associated with the
entry at point, or, if both sources did not provide any files, the audio file
that is currently played by MPD.  Next, the tagger view is created.  On the
right there is the buffer that contain the tag data.  Each line in this
buffer represents a tag with a value.  Tag and value are separated by a
colon.  Valid tags are "Title", "Artist", "Album", "Comment", "Genre", "Year"
and "Track".  The value can be an arbitrary string.  Whitespaces in front and
at the end of the value are ignored.  If the value is "<keep>", the tag line
is ignored.

To save the specified tag values back to the files, press `C-c C-c'
(ampc-tagger-save).  To exit the tagger and restore the previous window
configuration, press `C-c C-q'.  `C-u C-c C-c' saved the tags and exits the
tagger.  Only tags that are actually specified within the tagger buffer
written back to the file.  Other tags will not be touched by ampc.  For
example, to clear the "Commentary" tag, you need to specify the line

Commentary:

In the tagger buffer.  Omitting this line will make the tagger not touch the
"Commentary" tag at all.

On the right there is the files list buffer.  The selection of this buffer
specifies which files the command `ampc-tag-save' will write to.  If no file
is selected, the file at point in the file list buffer is used.

To reset the values of the tags specified in the tagger buffer to the common
values of all selected files specified by the selection of the files list
buffer, press `C-c C-r' (ampc-tagger-reset).  With a prefix argument,
`ampc-tagger-reset' restores missing tags as well.

You can use tab-completion within the tagger buffer for both tags and tag
values.

You can also use the tagging subsystem on its own without a running ampc
instance.  To start the tagger, call `ampc-tag-files'.  This function accepts
one argument, a list of absolute file names which are the files to tag.  ampc
provides a minor mode for dired, `ampc-tagger-dired-mode'.  If this mode is
enabled within a dired buffer, pressing `C-c C-t' (ampc-tagger-dired) will
start the tagger on the current selection.

The following ampc-specific hooks are run during tagger usage:

`ampc-tagger-grab-hook': Run by the tagger before grabbing tags of a file.
Each function is called with one argument, the file name.

`ampc-tagger-grabbed-hook': Run by the tagger after grabbing tags of a file.
Each function is called with one argument, the file name.

`ampc-tagger-store-hook': Run by the tagger before writing tags back to a
file.  Each function is called with two arguments, FOUND-CHANGED and DATA.
FOUND-CHANGED is non-nil if the tags that are about to be written differ from
the ones in the file.  DATA is a cons.  The car specifies the full file name
of the file that is about to be written to, the cdr is an alist that
specifies the tags that are about to be (over-)written.  The car of each
entry in this list is a symbol specifying the tag (one of the ones in
`ampc-tagger-tags'), the cdr a string specifying the value.  The cdr of DATA
may be modified.  If FOUND-CHANGED is nil and the cdr of DATA is not modified
throughout the hook is run, the file is not touched.
`ampc-tagger-stored-hook' is still run, though.

`ampc-tagger-stored-hook': Run by the tagger after writing tags back to a
file.  Each function is called with two arguments, FOUND-CHANGED and DATA.
These are the same arguments that were already passed to
`ampc-tagger-store-hook'.  The car of DATA, the file name, may be modified.

These hooks can be used to handle vc locking and unlocking of files.  For
renaming files according to their (new) tag values, ampc provides the
function `ampc-tagger-rename-artist-title' which may be added to
`ampc-tagger-stored-hook'.  The new file name generated by this function is
"Artist"_-_"Title"."extension".  Characters within "Artist" and "Title" that
are not alphanumeric are substituted with underscores.

** global keys
Aside from `J', `M', `K', `<' and `L', which may be used to select different
views, and `I' which starts the tagger, ampc defines the following global
keys.  These binding are available in every buffer associated with ampc:

`k' (ampc-toggle-play): Toggle play state.  If MPD does not play a song,
start playing the song at point if the current buffer is the playlist buffer,
otherwise start at the beginning of the playlist.  With numeric prefix
argument 4, stop player rather than pause if applicable.

`l' (ampc-next): Play next song.
`j' (ampc-previous): Play previous song

`c' (ampc-clear): Clear playlist.
`s' (ampc-shuffle): Shuffle playlist.

`S' (ampc-store): Store playlist.
`O' (ampc-load): Load selected playlist into the current playlist.
`R' (ampc-rename-playlist): Rename selected playlist.
`D' (ampc-delete-playlist): Delete selected playlist.

`y' (ampc-increase-volume): Increase volume.
`M-y' (ampc-decrease-volume): Decrease volume.
`C-M-y' (ampc-set-volume): Set volume.
`h' (ampc-increase-crossfade): Increase crossfade.
`M-h' (ampc-decrease-crossfade): Decrease crossfade.
`C-M-h' (ampc-set-crossfade): Set crossfade.

`e' (ampc-toggle-repeat): Toggle repeat state.
`r' (ampc-toggle-random): Toggle random state.
`f' (ampc-toggle-consume): Toggle consume state.

`P' (ampc-goto-current-song): Select the current playlist window and move
point to the current song.
`G' (ampc-mini): Select song to play via `completing-read'.

`T' (ampc-trigger-update): Trigger a database update.
`Z' (ampc-suspend): Suspend ampc.
`q' (ampc-quit): Quit ampc.

The keymap of ampc is designed to fit the QWERTY United States keyboard
layout.  If you use another keyboard layout, feel free to modify
`ampc-mode-map'.  For example, I use a regular QWERTZ German keyboard
(layout), so I modify `ampc-mode-map' in my init.el like this:

(eval-after-load 'ampc
  '(flet ((substitute-ampc-key
           (from to)
           (define-key ampc-mode-map to (lookup-key ampc-mode-map from))
           (define-key ampc-mode-map from nil)))
     (substitute-ampc-key (kbd "z") (kbd "Z"))
     (substitute-ampc-key (kbd "y") (kbd "z"))
     (substitute-ampc-key (kbd "M-y") (kbd "M-z"))
     (substitute-ampc-key (kbd "C-M-y") (kbd "C-M-z"))
     (substitute-ampc-key (kbd "<") (kbd ";"))))

If ampc is suspended, you can still use every interactive command that does
not directly operate on or with the user interace of ampc.  For example it is
perfectly fine to call `ampc-increase-volume' or `ampc-toggle-play' via M-x
RET.  Especially the commands `ampc-status' and `ampc-mini' are predesignated
to be bound in the global keymap and called when ampc is suspended.
`ampc-status' messages the information that is displayed by the status window
of ampc.  `ampc-mini' lets you select a song to play via `completing-read'.
To start ampc suspended, call `ampc' with the third argument being non-nil.
To check whether ampc is connected to the daemon and/or suspended, call
`ampc-is-on-p' or `ampc-suspended-p'.

(global-set-key (kbd "<f7>")
                (lambda ()
                  (interactive)
                  (unless (ampc-on-p)
                    (ampc nil nil t))
                  (ampc-status)))
(global-set-key (kbd "<f8>")
                (lambda ()
                  (interactive)
                  (unless (ampc-on-p)
                    (ampc nil nil t))
                  (ampc-mini)))
