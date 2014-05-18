#!/usr/bin/env python

from __future__ import print_function
import subprocess
import os
import signal
import cgi


processtokill = subprocess.Popen(['ps', '-A'], stdout=subprocess.PIPE)
out, err = processtokill.communicate()
for line in out.splitlines():
  if 'swipl' in line:
    pid = int(line.split(None, 1)[0])
    os.kill(pid, signal.SIGKILL)
