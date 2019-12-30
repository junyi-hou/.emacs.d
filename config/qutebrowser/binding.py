## keymap for qutebrowser ##

# NOTE: I have setup to use Meta-y to simulate Ctrl-c in exwm, so every <Ctrl-c> here are actually <Meta-y>

# ============================
# Global remap
# ============================

# This setting can be used to map keys to other keys. When the key used
# as dictionary-key is pressed, the binding for the key used as
# dictionary-value is invoked instead. This is useful for global
# remappings of keys, for example to map Ctrl-[ to Escape. Note that
# when a key is bound (via `bindings.default` or `bindings.commands`),
# the mapping is ignored.
## Type: Dict
c.bindings.key_mappings = {
    "<Alt-j>": "<Down>",
    "<Alt-k>": "<Up>",
    "<Alt-h>": "<Left>",
    "<Alt-l>": "<Right>"
}

# ============================
# Bindings for normal mode
# ============================

# vim stuff
config.bind("<Space>", 'set-cmd-text :')
# config.bind('<Meta-Space>', 'enter-mode passthrough ;; ')
config.bind('.', 'repeat-command')

config.bind('q', 'record-macro')
config.bind('@', 'run-macro')

config.bind('r', 'reload')
config.bind('R', 'reload -f')
config.bind('<Ctrl-s>', 'stop')

config.bind('<Return>', 'follow-selected')
config.bind('<Ctrl-Return>', 'follow-selected -t')
config.bind('<Escape>', 'clear-keychain ;; search ;; fullscreen --leave')
config.bind("w", 'tab-close')
config.bind('<Alt-m>', 'tab-mute')
config.bind('<Ctrl-Q>', 'quit')

# zooming
config.bind('+', 'zoom-in')
config.bind('zi', 'zoom-in')
config.bind('-', 'zoom-out')
config.bind('zo', 'zoom-out')
config.bind('=', 'zoom')

# search
config.bind('/', 'set-cmd-text /')
config.bind('?', 'set-cmd-text ?')
config.bind('N', 'search-prev')
config.bind('n', 'search-next')

# movement
config.bind('h', 'scroll left')
config.bind('j', 'scroll down')
config.bind('k', 'scroll up')
config.bind('l', 'scroll right')
config.bind('d', 'scroll-page 0 0.5')
config.bind('u', 'scroll-page 0 -0.5')
config.bind('<Ctrl-y>', 'scroll-page 0 -0.3')
config.bind('<Ctrl-e>', 'scroll-page 0 0.3')

config.bind('p', 'tab-pin')

config.bind('H', 'back')
config.bind('K', 'tab-next')
config.bind('J', 'tab-prev')
config.bind('L', 'forward')

config.bind('G', 'scroll-to-perc')
config.bind('gg', 'scroll-to-perc 0')

# open new tabs/new website
config.bind("t", 'set-cmd-text -s :open -t')
config.bind("T", 'open -t -- {primary}')
config.bind("o", 'set-cmd-text -s :open')
config.bind('O', 'open -- {primary}')

config.bind('gb', 'open qute://bookmarks#bookmarks')
config.bind('gh', 'open qute://history')
config.bind('gs', 'open qute://settings')
config.bind('gf', 'view-source')

# start hinting
config.unbind("f", mode='normal')
config.bind('ff', 'hint')
config.bind('fi', 'hint inputs --first')
config.bind('fI', 'hint inputs')
config.bind('fd', 'hint links download')
config.bind('fo', 'hint links fill :open {hint-url}')
config.bind('ft', 'hint links fill :open -t -r {hint-url}')
config.bind('fy', 'hint links yank')
config.bind('fY', 'hint links yank-primary')

# bookmarks
config.bind('m', 'quickmark-save')
config.bind('b', 'set-cmd-text -s :quickmark-load')
config.bind('B', 'set-cmd-text -s :quickmark-load -t')

# enter other modes
config.bind('<Meta-v>', 'enter-mode passthrough')
config.bind('i', 'enter-mode insert')
config.bind("'", 'enter-mode jump_mark')
config.bind("`", 'enter-mode set_mark')
config.bind('v', 'enter-mode caret')

# download
config.bind('ad', 'download-cancel')
config.bind('cd', 'download-clear')
config.bind('gd', 'download')

# yank
config.bind('yt', 'yank title')
config.bind('yy', 'yank')
config.bind("ys", 'yank selection')
config.bind('ym', 'yank inline [[{url}][{title}]]')


# ============================
# Bindings for caret mode
# ============================

config.bind('J', 'scroll-page 0 0.5', mode='caret')
config.bind('K', 'scroll-page 0 0.5', mode='caret')


# ============================
# Bindings for command and prompt mode
# ============================

# use default for now

# Bindings for command mode
# config.bind('<Alt-B>', 'rl-backward-word', mode='command')
# config.bind('<Alt-Backspace>', 'rl-backward-kill-word', mode='command')
# config.bind('<Alt-D>', 'rl-kill-word', mode='command')
# config.bind('<Alt-F>', 'rl-forward-word', mode='command')
# config.bind('<Ctrl-?>', 'rl-delete-char', mode='command')
# config.bind('<Ctrl-A>', 'rl-beginning-of-line', mode='command')
# config.bind('<Ctrl-B>', 'rl-backward-char', mode='command')
# config.bind('<Ctrl-C>', 'completion-item-yank', mode='command')
# config.bind('<Ctrl-D>', 'completion-item-del', mode='command')
# config.bind('<Ctrl-E>', 'rl-end-of-line', mode='command')
# config.bind('<Ctrl-F>', 'rl-forward-char', mode='command')
# config.bind('<Ctrl-H>', 'rl-backward-delete-char', mode='command')
# config.bind('<Ctrl-K>', 'rl-kill-line', mode='command')
# config.bind('<Ctrl-N>', 'command-history-next', mode='command')
# config.bind('<Ctrl-P>', 'command-history-prev', mode='command')
# config.bind('<Ctrl-Return>', 'command-accept --rapid', mode='command')
# config.bind('<Ctrl-Shift-C>', 'completion-item-yank --sel', mode='command')
# config.bind('<Ctrl-Shift-Tab>', 'completion-item-focus prev-category', mode='command')
# config.bind('<Ctrl-Tab>', 'completion-item-focus next-category', mode='command')
# config.bind('<Ctrl-U>', 'rl-unix-line-discard', mode='command')
# config.bind('<Ctrl-W>', 'rl-unix-word-rubout', mode='command')
# config.bind('<Ctrl-Y>', 'rl-yank', mode='command')
# config.bind('<Down>', 'completion-item-focus --history next', mode='command')
# config.bind('<Escape>', 'leave-mode', mode='command')
# config.bind('<Return>', 'command-accept', mode='command')
# config.bind('<Shift-Delete>', 'completion-item-del', mode='command')
# config.bind('<Shift-Tab>', 'completion-item-focus prev', mode='command')
# config.bind('<Tab>', 'completion-item-focus next', mode='command')
# config.bind('<Up>', 'completion-item-focus --history prev', mode='command')

# Bindings for prompt mode
# config.bind('<Alt-B>', 'rl-backward-word', mode='prompt')
# config.bind('<Alt-Backspace>', 'rl-backward-kill-word', mode='prompt')
# config.bind('<Alt-D>', 'rl-kill-word', mode='prompt')
# config.bind('<Alt-F>', 'rl-forward-word', mode='prompt')
# config.bind('<Alt-Shift-Y>', 'prompt-yank --sel', mode='prompt')
# config.bind('<Alt-Y>', 'prompt-yank', mode='prompt')
# config.bind('<Ctrl-?>', 'rl-delete-char', mode='prompt')
# config.bind('<Ctrl-A>', 'rl-beginning-of-line', mode='prompt')
# config.bind('<Ctrl-B>', 'rl-backward-char', mode='prompt')
# config.bind('<Ctrl-E>', 'rl-end-of-line', mode='prompt')
# config.bind('<Ctrl-F>', 'rl-forward-char', mode='prompt')
# config.bind('<Ctrl-H>', 'rl-backward-delete-char', mode='prompt')
# config.bind('<Ctrl-K>', 'rl-kill-line', mode='prompt')
# config.bind('<Ctrl-P>', 'prompt-open-download --pdfjs', mode='prompt')
# config.bind('<Ctrl-U>', 'rl-unix-line-discard', mode='prompt')
# config.bind('<Ctrl-W>', 'rl-unix-word-rubout', mode='prompt')
# config.bind('<Ctrl-X>', 'prompt-open-download', mode='prompt')
# config.bind('<Ctrl-Y>', 'rl-yank', mode='prompt')
# config.bind('<Down>', 'prompt-item-focus next', mode='prompt')
# config.bind('<Escape>', 'leave-mode', mode='prompt')
# config.bind('<Return>', 'prompt-accept', mode='prompt')
# config.bind('<Shift-Tab>', 'prompt-item-focus prev', mode='prompt')
# config.bind('<Tab>', 'prompt-item-focus next', mode='prompt')
# config.bind('<Up>', 'prompt-item-focus prev', mode='prompt')

# ============================
# Bindings for hint mode
# ============================

# use default

# ============================
# Bindings for insert mode
# ============================

config.bind(
    '<Escape>', 'leave-mode ;; jseval -q document.activeElement.blur()', mode='insert')
config.bind('<Meta-y>e', 'open-editor', mode='insert')
config.bind('<Meta-y>p', 'insert-text {primary}', mode='insert')
config.bind('<Meta-y>a', 'spawn --userscript qutepass', mode='insert')
config.bind('<Meta-y>u', 'spawn --userscript qutepass -u', mode='insert')
config.bind('<Meta-y>s', 'spawn --userscript qutepass -p', mode='insert')

# ============================
# Bindings for passthrouggh mode
# ============================

# use default

# ============================
# Bindings for register mode
# ============================

# use default

# ============================
# Bindings for yesno mode
# ============================

# use default
