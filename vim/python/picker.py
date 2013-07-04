#!/usr/bin/env python2.7

import tempfile
import subprocess
import os
import vim

def picker():
    (fd, temp) = tempfile.mkstemp()
    try:
        if subprocess.call(["picker", temp]) == 0:
            with os.fdopen(fd) as fh:
                vim.command("edit %s" % fh.read().rstrip())
    finally:
        os.close(fd)
        os.unlink(temp)

