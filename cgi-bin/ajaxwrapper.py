#!/usr/bin/env python

from __future__ import print_function

import os
import cgi
from subprocess import Popen, PIPE, STDOUT

# # SWI Prolog
SCRIPTDIR = 'javaprolog'
SCRIPT = ['swipl', '-q', '-g', 'main,halt', '-t', 'halt(1)', '-s']
print('Content-type:text/plain')
print()

try:

    while not os.path.isdir(SCRIPTDIR):
        SCRIPTDIR = os.path.join("..", SCRIPTDIR)
    form = cgi.FieldStorage()
    data = form.getfirst('data')
    program = form.getfirst('program')
    SCRIPT.append(program)
    script = Popen(SCRIPT, cwd=SCRIPTDIR, stdin=PIPE, stdout=PIPE, stderr=PIPE)
    out, err = script.communicate(data)

    print(out)
    if err:
        raise Exception(err)

except:
    import sys, traceback
    print(traceback.format_exc())
    sys.exit(1)
