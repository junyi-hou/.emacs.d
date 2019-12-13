# This is here so configs done via the GUI are still loaded.
# Remove it to not load settings done via the GUI.
config.load_autoconfig()

config.source("./general.py")
config.source("./font.py")
config.source("./binding.py")
