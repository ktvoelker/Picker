#!/usr/bin/env python

import curses as c
import signal
import subprocess
import string

COLUMN_WIDTH = 28
COLUMN_SPACING = 2

class Display(object):

  def __init__(self):
    self.next_color_pair = 1

  def handle_signal(self, sig, stack):
    if sig == signal.SIGTERM:
      sys.exit(0)
    else:
      sys.exit(1)

  def __enter__(self):
    # Get the whole terminal as a Window object
    self.root = c.initscr()
    c.savetty()
    # Clean up the terminal if we get killed
    signal.signal(signal.SIGTERM, lambda(sig, stack): self.handle_signal(sig, stack))
    # Enable colors
    c.start_color()
    # Get one character at a time
    c.cbreak()
    # Get the size of the terminal
    (h, w) = self.root.getmaxyx()
    # Make a window for the top bar
    self.title_bar = self.root.subwin(1, w, 0, 0)
    title_color = self.alloc_color_pair(c.COLOR_WHITE, c.COLOR_BLUE)
    self.title_bar.bkgdset(' ', title_color)
    self.title_bar.clear()
    self.title_bar.insstr(0, 1, "Picker 0.0.1")
    # TODO allow the caller of Picker to specify a title
    # Make a window for the main content area
    self.content = self.root.subpad(h - 2, w, 1, 0)
    content_color = self.alloc_color_pair(c.COLOR_BLACK, c.COLOR_WHITE)
    self.content.bkgdset(' ', content_color)
    self.content.clear()
    # Make a window for the text entry area
    self.entry_bar = self.root.subwin(1, w, h - 1, 0)
    entry_color = self.alloc_color_pair(c.COLOR_BLACK, c.COLOR_GREEN)
    self.entry_bar.bkgdset(' ', entry_color)
    self.entry_bar.clear()
    self.entry_bar.insstr(0, 1, ">")
    self.entry_bar.move(0, 3)
    # Allocate color for the selected result
    self.highlight = self.alloc_color_pair(c.COLOR_BLACK, c.COLOR_YELLOW)
    return self

  def __exit__(self, type, value, traceback):
    c.resetty()
    c.endwin()

  def alloc_color_pair(self, fg, bg):
    pair = self.next_color_pair
    self.next_color_pair += 1
    c.init_pair(pair, fg, bg)
    return c.color_pair(pair)

  def run(self, control):
    try:
      ch = self.root.getch()
      while ch >= 0:
        control.handle_key(ch, c.keyname(ch))
        ch = self.root.getch()
    except KeyboardInterrupt:
      pass

  def _format_result(self, result, max_width):
    if len(result) > max_width:
      return "..." + result[-max_width + 3, -1]
    else:
      return result

  def update(self, results, index):
    (curs_y, curs_x) = self.entry_bar.getyx()
    c.curs_set(0)
    self.content.clear()
    (rows, w) = self.content.getmaxyx()
    columns = 1 + max(0, (w - COLUMN_WIDTH) // (COLUMN_WIDTH + COLUMN_SPACING))
    column_width = min(w, COLUMN_WIDTH)
    for i in xrange(0, len(results)):
      column = i // rows
      row = i % rows
      if i == index:
        attr = self.highlight
      else:
        attr = 0
      c.insstr(row, column, self._format_result(results[i], column_width), attr)
    self.entry_bar.move(curs_y, curs_x)
    c.curs_set(2)

class Control(object):

  def __init__(self, display):
    self.extensions = ["md", "py", "scala", "hs"]
    self.display = display
    self.query = None
    self.index = None
    self.results = None
    proc = subprocess.Popen(
        ["git", "rev-parse", "--show-toplevel"],
        stdout=subprocess.PIPE,
        close_fds=True)
    self.root = proc.stdout.readline().strip()
    self._find_files()
    proc.wait()

  def _find_files(self):
    proc = subprocess.Popen(
        ["find",
          self.root,
          "(", "-type", "f", "-or", "-type", "l", ")",
          "-regex",
          r".*\.\(" + r"\|".join(self.extensions) + r"\)"],
        stdout=subprocess.PIPE,
        close_fds=True)
    root_len = len(self.root) + 1
    self.files = map(lambda xs: xs[root_len:], map(string.strip, list(proc.stdout)))
    proc.wait()

  def _refine(self, xs):
    words = self.query.split()
    def match(x):
      for word in words:
        if x.find(word) < 0:
          return False
      return True
    for x in xs:
      if match(x):
        yield x

  def handle_key(self, ch, name):
    import sys
    # TODO handle line-editing characters
    # TODO history with up and down arrows
    if ch == 10:
      # return
      if self.index is not None:
        print(self.root + "/" + self.results[self.index])
      raise KeyboardInterrupt()
    elif ch == 127:
      # delete
      pass
      # TODO backspace
    elif ch == 9:
      # tab
      pass
      # TODO increment index
    elif ch == 27:
      # shift-tab
      pass
      # TODO decrement index
    elif name in string.printable:
      if self.query:
        self.query += name
        self.results = list(self._refine(self.results))
        if self.index >= len(self.results):
          self.index = len(self.results) - 1
      else:
        self.query = name
        self.results = list(self._refine(self.files))
        if len(self.results) > 0:
          self.index = 0
      self.display.update(self.results, self.index)
    else:
      import sys
      sys.stderr.write("Unknown character code %d (name %s)\n" % (ch, name))

with Display() as display:
  control = Control(display)
  display.run(control)

