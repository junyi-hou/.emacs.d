## general settings ##


# Aliases for commands. The keys of the given dictionary are the
# aliases, while the values are the commands they map to.
## Type: Dict
# c.aliases = {'w': 'session-save', 'q': 'close', 'qa': 'quit', 'wq': 'quit --save', 'wqa': 'quit --save'}

# Always restore open sites when qutebrowser is reopened.
## Type: Bool
c.auto_save.session = False

# Backend to use to display websites. qutebrowser supports two different
# web rendering engines / backends, QtWebKit and QtWebEngine. QtWebKit
# was discontinued by the Qt project with Qt 5.6, but picked up as a
# well maintained fork: https://github.com/annulen/webkit/wiki -
# qutebrowser only supports the fork. QtWebEngine is Qt's official
# successor to QtWebKit. It's slightly more resource hungry than
# QtWebKit and has a couple of missing features in qutebrowser, but is
# generally the preferred choice.
## Type: String
# Valid values:
# - webengine: Use QtWebEngine (based on Chromium).
# - webkit: Use QtWebKit (based on WebKit, similar to Safari).
c.backend = 'webengine'

# Height (in pixels or as percentage of the window) of the completion.
## Type: PercOrInt
c.completion.height = '30%'

# Execute the best-matching command on a partial match.
## Type: Bool
c.completion.use_best_match = True

# Shrink the completion to be smaller than the configured size if there
# are no scrollbars.
## Type: Bool
c.completion.shrink = True

# Require a confirmation before quitting the application.
## Type: ConfirmQuit
# Valid values:
# - always: Always show a confirmation.
# - multiple-tabs: Show a confirmation if multiple tabs are opened.
# - downloads: Show a confirmation if downloads are running
# - never: Never show a confirmation.
c.confirm_quit = ['always']

# Automatically start playing `<video>` elements. Note: On Qt < 5.11,
# this option needs a restart and does not support URL patterns.
## Type: Bool
c.content.autoplay = False

# Which cookies to accept.
## Type: String
# Valid values:
# - all: Accept all cookies.
# - no-3rdparty: Accept cookies from the same origin only. This is known to break some sites, such as GMail.
# - no-unknown-3rdparty: Accept cookies from the same origin only, unless a cookie is already set for the domain. On QtWebEngine, this is the same as no-3rdparty.
# - never: Don't accept cookies at all.
c.content.cookies.accept = 'no-3rdparty'

# Store cookies. Note this option needs a restart with QtWebEngine on Qt
# < 5.9.
## Type: Bool
c.content.cookies.store = True

# Default encoding to use for websites. The encoding must be a string
# describing an encoding such as _utf-8_, _iso-8859-1_, etc.
## Type: String
c.content.default_encoding = 'utf-8'

# Allow websites to share screen content. On Qt < 5.10, a dialog box is
# always displayed, even if this is set to "true".
## Type: BoolAsk
# Valid values:
##   - true
##   - false
##   - ask
c.content.desktop_capture = False

# Try to pre-fetch DNS entries to speed up browsing.
## Type: Bool
c.content.dns_prefetch = True

# Allow websites to request geolocations.
## Type: BoolAsk
# Valid values:
##   - true
##   - false
##   - ask
c.content.geolocation = 'ask'

# Custom headers for qutebrowser HTTP requests.
## Type: Dict
# c.content.headers.custom = {}

# Value to send in the `DNT` header. When this is set to true,
# qutebrowser asks websites to not track your identity. If set to null,
# the DNT header is not sent at all.
## Type: Bool
c.content.headers.do_not_track = True

# When to send the Referer header. The Referer header tells websites
# from which website you were coming from when visiting them. No restart
# is needed with QtWebKit.
## Type: String
# Valid values:
# - always: Always send the Referer.
# - never: Never send the Referer. This is not recommended, as some sites may break.
# - same-domain: Only send the Referer for the same domain. This will still protect your privacy, but shouldn't break any sites. With QtWebEngine, the referer will still be sent for other domains, but with stripped path information.
c.content.headers.referer = 'same-domain'

# Enable host blocking.
## Type: Bool
c.content.host_blocking.enabled = True

# List of URLs of lists which contain hosts to block.  The file can be
# in one of the following formats:  - An `/etc/hosts`-like file - One
# host per line - A zip-file of any of the above, with either only one
# file, or a file   named `hosts` (with any extension).  It's also
# possible to add a local file or directory via a `file://` URL. In case
# of a directory, all files in the directory are read as adblock lists.
# The file `~/.config/qutebrowser/blocked-hosts` is always read if it
# exists.
# Type: List of Url
c.content.host_blocking.lists = [
    'https://raw.githubusercontent.com/StevenBlack/hosts/master/hosts']

# A list of patterns that should always be loaded, despite being ad-
# blocked. Note this whitelists blocked hosts, not first-party URLs. As
# an example, if `example.org` loads an ad from `ads.example.org`, the
# whitelisted host should be `ads.example.org`. If you want to disable
# the adblocker on a given page, use the `content.host_blocking.enabled`
# setting with a URL pattern instead. Local domains are always exempt
# from hostblocking.
# Type: List of UrlPattern
# c.content.host_blocking.whitelist = ['piwik.org']

# Allow websites to record audio/video.
## Type: BoolAsk
# Valid values:
##   - true
##   - false
##   - ask
c.content.media_capture = False

# Allow websites to lock your mouse pointer.
## Type: BoolAsk
# Valid values:
##   - true
##   - false
##   - ask
c.content.mouse_lock = False

# Allow websites to show notifications.
## Type: BoolAsk
# Valid values:
##   - true
##   - false
##   - ask
# c.content.notifications = False

# Allow pdf.js to view PDF files in the browser. Note that the files can
# still be downloaded by clicking the download button in the pdf.js
# viewer.
## Type: Bool
c.content.pdfjs = True

# Allow websites to request persistent storage quota via
# `navigator.webkitPersistentStorage.requestQuota`.
## Type: BoolAsk
# Valid values:
##   - true
##   - false
##   - ask
c.content.persistent_storage = False

# Enable plugins in Web pages.
## Type: Bool
c.content.plugins = False

# List of user stylesheet filenames to use.
# Type: List of File, or File
# c.content.user_stylesheets = []

# Limit fullscreen to the browser window (does not expand to fill the
# screen).
## Type: Bool
c.content.windowed_fullscreen = True

# Directory to save downloads to. If unset, a sensible OS-specific
# default is used.
## Type: Directory
c.downloads.location.directory = "~/downloads"

# Prompt the user for the download location. If set to false,
# `downloads.location.directory` will be used.
## Type: Bool
c.downloads.location.prompt = False

# What to display in the download filename input.
## Type: String
# Valid values:
# - path: Show only the download path.
# - filename: Show only download filename.
# - both: Show download path and filename.
c.downloads.location.suggestion = 'both'

# Editor (and arguments) to use for the `open-editor` command. The
# following placeholders are defined: * `{file}`: Filename of the file
# to be edited. * `{line}`: Line in which the caret is found in the
# text. * `{column}`: Column in which the caret is found in the text. *
# `{line0}`: Same as `{line}`, but starting from index 0. * `{column0}`:
# Same as `{column}`, but starting from index 0.
## Type: ShellCommand
c.editor.command = ['emacsclient', '{file}']

# When a hint can be automatically followed without pressing Enter.
## Type: String
# Valid values:
# - always: Auto-follow whenever there is only a single hint on a page.
# - unique-match: Auto-follow whenever there is a unique non-empty match in either the hint string (word mode) or filter (number mode).
# - full-match: Follow the hint when the user typed the whole hint (letter, word or number mode) or the element's text (only in number mode).
# - never: The user will always need to press Enter to follow a hint.
c.hints.auto_follow = 'unique-match'

# Dictionary file to be used by the word hints.
## Type: File
# c.hints.dictionary = '/usr/share/dict/words'

# Hide unmatched hints in rapid mode.
## Type: Bool
c.hints.hide_unmatched_rapid_hints = True

# Leave hint mode when starting a new page load.
## Type: Bool
c.hints.leave_on_load = True

# Mode to use for hints.
## Type: String
# Valid values:
# - number: Use numeric hints. (In this mode you can also type letters from the hinted element to filter and reduce the number of elements that are hinted.)
# - letter: Use the characters in the `hints.chars` setting.
# - word: Use hints words based on the html elements and the extra words.
c.hints.mode = 'number'

# Comma-separated list of regular expressions to use for 'next' links.
# Type: List of Regex
# c.hints.next_regexes = ['\\bnext\\b', '\\bmore\\b', '\\bnewer\\b', '\\b[>→≫]\\b', '\\b(>>|»)\\b', '\\bcontinue\\b']

# Comma-separated list of regular expressions to use for 'prev' links.
# Type: List of Regex
# c.hints.prev_regexes = ['\\bprev(ious)?\\b', '\\bback\\b', '\\bolder\\b', '\\b[<←≪]\\b', '\\b(<<|«)\\b']

# CSS selectors used to determine which elements on a page should have
# hints.
## Type: Dict
# c.hints.selectors = {'all': ['a', 'area', 'textarea', 'select', 'input:not([type="hidden"])', 'button', 'frame', 'iframe', 'img', 'link', 'summary', '[onclick]', '[onmousedown]', '[role="link"]', '[role="option"]', '[role="button"]', '[ng-click]', '[ngClick]', '[data-ng-click]', '[x-ng-click]', '[tabindex]'], 'links': ['a[href]', 'area[href]', 'link[href]', '[role="link"][href]'], 'images': ['img'], 'media': ['audio', 'img', 'video'], 'url': ['[src]', '[href]'], 'inputs': ['input[type="text"]', 'input[type="date"]', 'input[type="datetime-local"]', 'input[type="email"]', 'input[type="month"]', 'input[type="number"]', 'input[type="password"]', 'input[type="search"]', 'input[type="tel"]', 'input[type="time"]', 'input[type="url"]', 'input[type="week"]', 'input:not([type])', 'textarea']}

# Allow Escape to quit the crash reporter.
## Type: Bool
# c.input.escape_quits_reporter = True

# Which unbound keys to forward to the webview in normal mode.
## Type: String
# Valid values:
# - all: Forward all unbound keys.
# - auto: Forward unbound non-alphanumeric keys.
# - none: Don't forward any keys.
# c.input.forward_unbound_keys = 'auto'

# Enter insert mode if an editable element is clicked.
## Type: Bool
c.input.insert_mode.auto_enter = True

# Leave insert mode if a non-editable element is clicked.
## Type: Bool
c.input.insert_mode.auto_leave = True

# Automatically enter insert mode if an editable element is focused
# after loading the page.
## Type: Bool
c.input.insert_mode.auto_load = True

# Include hyperlinks in the keyboard focus chain when tabbing.
## Type: Bool
# c.input.links_included_in_focus_chain = True

# Timeout (in milliseconds) for partially typed key bindings. If the
# current input forms only partial matches, the keystring will be
# cleared after this time.
## Type: Int
# c.input.partial_timeout = 5000

# Keychains that shouldn't be shown in the keyhint dialog. Globs are
# supported, so `;*` will blacklist all keychains starting with `;`. Use
# `*` to disable keyhints.
# Type: List of String
# c.keyhint.blacklist = []

# Time (in milliseconds) from pressing a key to seeing the keyhint
# dialog.
## Type: Int
# c.keyhint.delay = 500

# Rounding radius (in pixels) for the edges of the keyhint dialog.
## Type: Int
# c.keyhint.radius = 6

# Duration (in milliseconds) to show messages in the statusbar for. Set
# to 0 to never clear messages.
## Type: Int
c.messages.timeout = 2000

# Show a filebrowser in upload/download prompts.
## Type: Bool
c.prompt.filebrowser = True

# When to use Chromium's low-end device mode. This improves the RAM
# usage of renderer processes, at the expense of performance.
## Type: String
# Valid values:
# - always: Always use low-end device mode.
# - auto: Decide automatically (uses low-end mode with < 1 GB available RAM).
# - never: Never use low-end device mode.
c.qt.low_end_device_mode = 'auto'

# Which Chromium process model to use. Alternative process models use
# less resources, but decrease security and robustness. See the
# following pages for more details:    -
# https://www.chromium.org/developers/design-documents/process-models
# - https://doc.qt.io/qt-5/qtwebengine-features.html#process-models
## Type: String
# Valid values:
# - process-per-site-instance: Pages from separate sites are put into separate processes and separate visits to the same site are also isolated.
# - process-per-site: Pages from separate sites are put into separate processes. Unlike Process per Site Instance, all visits to the same site will share an OS process. The benefit of this model is reduced memory consumption, because more web pages will share processes. The drawbacks include reduced security, robustness, and responsiveness.
# - single-process: Run all tabs in a single process. This should be used for debugging purposes only, and it disables `:open --private`.
c.qt.process_model = 'process-per-site'

# When to show the scrollbar.
## Type: String
# Valid values:
# - always: Always show the scrollbar.
# - never: Never show the scrollbar.
# - when-searching: Show the scrollbar when searching for text in the webpage. With the QtWebKit backend, this is equal to `never`.
c.scrolling.bar = 'never'

# Enable smooth scrolling for web pages. Note smooth scrolling does not
# work with the `:scroll-px` command.
## Type: Bool
c.scrolling.smooth = False

# When to find text on a page case-insensitively.
## Type: IgnoreCase
# Valid values:
# - always: Search case-insensitively.
# - never: Search case-sensitively.
# - smart: Search case-sensitively if there are capital characters.
c.search.ignore_case = 'smart'

# Find text on a page incrementally, renewing the search for each typed
# character.
## Type: Bool
c.search.incremental = False

# Load a restored tab as soon as it takes focus.
## Type: Bool
# c.session.lazy_restore = False

# Languages to use for spell checking. You can check for available
# languages and install dictionaries using scripts/dictcli.py. Run the
# script with -h/--help for instructions.
# Type: List of String
# Valid values:
c.spellcheck.languages = []

# List of widgets displayed in the statusbar.
# Type: List of String
# Valid values:
# - url: Current page URL.
# - scroll: Percentage of the current page position like `10%`.
# - scroll_raw: Raw percentage of the current page position like `10`.
# - history: Display an arrow when possible to go back/forward in history.
# - tabs: Current active tab, e.g. `2`.
# - keypress: Display pressed keys when composing a vi command.
# - progress: Progress bar for the current page loading.
c.statusbar.widgets = ['keypress', 'history', 'progress', 'scroll']

# Hide the statusbar unless a message is shown.
## Type: Bool
c.statusbar.hide = False

# Padding (in pixels) for the statusbar.
## Type: Padding
c.statusbar.padding = {'top': 2, 'bottom': 1, 'left': 0, 'right': 0}

# Position of the status bar.
## Type: VerticalPosition
# Valid values:
##   - top
##   - bottom
c.statusbar.position = 'bottom'

# Open new tabs (middleclick/ctrl+click) in the background.
## Type: Bool
c.tabs.background = True

# Mouse button with which to close tabs.
## Type: String
# Valid values:
# - right: Close tabs on right-click.
# - middle: Close tabs on middle-click.
# - none: Don't close tabs using the mouse.
c.tabs.close_mouse_button = 'none'

# How to behave when the close mouse button is pressed on the tab bar.
## Type: String
# Valid values:
# - new-tab: Open a new tab.
# - close-current: Close the current tab.
# - close-last: Close the last tab.
# - ignore: Don't do anything.
c.tabs.close_mouse_button_on_bar = 'ignore'

# When to show favicons in the tab bar.
## Type: String
# Valid values:
# - always: Always show favicons.
# - never: Always hide favicons.
# - pinned: Show favicons only on pinned tabs.
c.tabs.favicons.show = 'pinned'

# Padding (in pixels) for tab indicators.
## Type: Padding
c.tabs.indicator.padding = {'top': 2, 'bottom': 2, 'left': 0, 'right': 4}

# How to behave when the last tab is closed.
## Type: String
# Valid values:
# - ignore: Don't do anything.
# - blank: Load a blank page.
# - startpage: Load the start page.
# - default-page: Load the default page.
# - close: Close the window.
c.tabs.last_close = 'close'

# Switch between tabs using the mouse wheel.
## Type: Bool
c.tabs.mousewheel_switching = False

# Which tab to select when the focused tab is removed.
## Type: SelectOnRemove
# Valid values:
# - prev: Select the tab which came before the closed one (left in horizontal, above in vertical).
# - next: Select the tab which came after the closed one (right in horizontal, below in vertical).
# - last-used: Select the previously selected tab.
c.tabs.select_on_remove = 'last-used'

# Format to use for the tab title. The following placeholders are
# defined:  * `{perc}`: Percentage as a string like `[10%]`. *
# `{perc_raw}`: Raw percentage, e.g. `10`. * `{current_title}`: Title of
# the current web page. * `{title_sep}`: The string ` - ` if a title is
# set, empty otherwise. * `{index}`: Index of this tab. * `{id}`:
# Internal tab ID of this tab. * `{scroll_pos}`: Page scroll position. *
# `{host}`: Host of the current web page. * `{backend}`: Either
# ''webkit'' or ''webengine'' * `{private}`: Indicates when private mode
# is enabled. * `{current_url}`: URL of the current web page. *
# `{protocol}`: Protocol (http/https/...) of the current web page. *
# `{audio}`: Indicator for audio/mute status.
## Type: FormatString
c.tabs.title.format = '{audio}{current_title}'

# Format to use for the tab title for pinned tabs. The same placeholders
# like for `tabs.title.format` are defined.
## Type: FormatString
c.tabs.title.format_pinned = ''

# Format to use for the window title. The same placeholders like for
# `tabs.title.format` are defined.
## Type: FormatString
c.window.title_format = '{current_url}'

# Page to open if :open -t/-b/-w is used without URL. Use `about:blank`
# for a blank page.
## Type: FuzzyUrl
c.url.default_page = 'https://www.google.com'

# URL segments where `:navigate increment/decrement` will search for a
# number.
## Type: FlagList
# Valid values:
##   - host
##   - port
##   - path
##   - query
##   - anchor
# c.url.incdec_segments = ['path', 'query']

# Search engines which can be used via the address bar. Maps a search
# engine name (such as `DEFAULT`, or `ddg`) to a URL with a `{}`
# placeholder. The placeholder will be replaced by the search term, use
# `{{` and `}}` for literal `{`/`}` signs. The search engine named
# `DEFAULT` is used when `url.auto_search` is turned on and something
# else than a URL was entered to be opened. Other search engines can be
# used by prepending the search engine name to the search term, e.g.
# `:open google qutebrowser`.
## Type: Dict
c.url.searchengines = {'DEFAULT': 'https://www.google.com/search?q={}'}

# Page(s) to open at the start.
# Type: List of FuzzyUrl, or FuzzyUrl
c.url.start_pages = ['https://www.google.com']
