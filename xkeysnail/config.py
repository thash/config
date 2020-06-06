# -*- coding: utf-8 -*-

# official example: https://github.com/mooz/xkeysnail/blob/master/example/config.py

import re
from xkeysnail.transform import *

# define timeout for multipurpose_modmap
define_timeout(1)

# [Multipurpose modmap] Give a key two meanings. A normal key when pressed and
# released, and a modifier key when held down with another key. See Xcape,
# Carabiner and caps2esc for ideas and concept.
define_multipurpose_modmap({
    Key.LEFT_META:  [Key.MUHENKAN, Key.LEFT_META], # L_SUPER == LEFT_META
    Key.RIGHT_META: [Key.HENKAN, Key.RIGHT_META],
})

# NOTE: swapping colon for semicolon is achieved with xkb instead of xkeysnail,
#       due to an unintended side-effect that shift key keep pressed after hitting colon.
define_keymap(None, {
    K("Super-x"): K("C-x"),
    K("Super-a"): K("C-a"),
    K("Super-Tab"): K("M-Tab"),
    K("Super-Shift-Tab"): K("M-Shift-Tab"),
    K("C-u"): K("BACKSPACE"),
    K("C-d"): K("DELETE"),
}, "Global")

# NOT in terminal.
define_keymap(lambda wm_class: wm_class not in ("Gnome-terminal"), {
    K("C-h"): with_mark(K("left")),
    K("C-j"): with_mark(K("down")),
    K("C-k"): with_mark(K("up")),
    K("C-l"): with_mark(K("right")),
    K("C-Shift-h"): with_mark(K("Shift-left")),
    K("C-Shift-j"): with_mark(K("Shift-down")),
    K("C-Shift-k"): with_mark(K("Shift-up")),
    K("C-Shift-l"): with_mark(K("Shift-right")),
    K("C-a"): with_mark(K("home")),
    K("C-e"): with_mark(K("end")),
    K("C-Shift-a"): with_mark(K("Shift-home")),
    K("C-Shift-e"): with_mark(K("Shift-end")),
    K("C-n"): K("down"),
    K("C-p"): K("up"),
    K("C-Shift-f"): K("Shift-right"),
    K("C-Shift-b"): K("Shift-left"),
    K("C-Shift-n"): K("Shift-down"),
    # K("C-Shift-p"): K("Shift-up"), # move to 'not in VS Code' definitions

    # As Ctrl-C has special meaning on terminal. Also Gnome-terminal has a shortcut Shift+Ctrl+V to paste.
    K("Super-c"): K("C-c"),
    K("Super-v"): K("C-v"),
}, "Arrows")

# NOT in Terminal / VSCode.
define_keymap(lambda wm_class: wm_class not in ("Code", "Gnome-terminal"), {
    K("C-Shift-p"): K("Shift-up"),
    K("C-f"): K("right"),
    K("C-b"): K("left"),
}, "not in [Terminal, VSCode] keys")

# IN VSCode.
define_keymap(lambda wm_class: wm_class in ("Code"), {
    K("C-Shift-p"): K("C-Shift-p"),
    K("C-f"): K("page_down"),
    K("C-b"): K("page_up"),
    K("Super-s"): K("C-s"), # save key. Somehow VSCode gets stuck when you hit Super-s (to save).
}, "VSCode-keys")

# IN terminal.
define_keymap(lambda wm_class: wm_class in ("Gnome-terminal"), {
    # copy & paste need to pass special set of keys.
    K("Super-c"): K("C-Shift-c"),
    K("Super-v"): K("C-Shift-v"),
}, "Terminal-keys")

# Chromium specific keys.
define_keymap(lambda wm_class: wm_class in ("Chromium-browser"), {
    K("Super-a"): K("C-a"),
    K("Super-f"): K("C-f"),
    K("Super-l"): K("C-l"),
    K("Super-r"): K("C-r"),
    K("Super-t"): K("C-t"),
    K("Super-Shift-t"): K("C-Shift-t"),
    K("Super-w"): K("C-w"),
    K("Super-KEY_1"): K("C-KEY_1"),
    K("Super-KEY_2"): K("C-KEY_2"),
    K("Super-KEY_3"): K("C-KEY_3"),
    K("Super-KEY_4"): K("C-KEY_4"),
    K("Super-KEY_5"): K("C-KEY_5"),
    K("Super-KEY_6"): K("C-KEY_6"),
    K("Super-KEY_7"): K("C-KEY_7"),
    K("Super-KEY_8"): K("C-KEY_8"),
    K("Super-KEY_9"): K("C-KEY_9"),
    K("Super-MINUS"): K("C-Shift-MINUS"),
    K("Super-Shift-EQUAL"): K("C-Shift-EQUAL"),
}, "Chromium-keys")
