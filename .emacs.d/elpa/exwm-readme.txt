Overview
--------
EXWM (Emacs X Window Manager) is a full-featured tiling X window manager
for Emacs built on top of [XELB](https://github.com/ch11ng/xelb).
It features:
+ Fully keyboard-driven operations
+ Hybrid layout modes (tiling & stacking)
+ Dynamic workspace support
+ ICCCM/EWMH compliance
+ (Optional) RandR (multi-monitor) support
+ (Optional) Built-in compositing manager
+ (Optional) Built-in system tray

Installation & configuration
----------------------------
Here are the minimal steps to get EXWM working:
1. Install XELB and EXWM, and make sure they are in `load-path'.
2. In '~/.emacs', add following lines (please modify accordingly):

   (require 'exwm)
   (require 'exwm-config)
   (exwm-config-default)

3. Link or copy the file 'xinitrc' to '~/.xinitrc'.
4. Launch EXWM in a console (e.g. tty1) with

   xinit -- vt01

You should additionally hide the menu-bar, tool-bar, etc to increase the
usable space.  Please check the wiki (https://github.com/ch11ng/exwm/wiki)
for more detailed instructions on installation, configuration, usage, etc.

References:
+ dwm (http://dwm.suckless.org/)
+ i3 wm (https://i3wm.org/)
+ Also see references within each required library.
