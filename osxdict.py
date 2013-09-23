#!/usr/bin/env python
"""
osxdict
~~~~~~~

:copyright: Copyright 2013 by Leo Liu
:license: GPLv3
"""

from __future__ import absolute_import, print_function, division
# takes about 0.5s
from DictionaryServices import DCSCopyTextDefinition


# See also http://goo.gl/ZbhhkA
def get_definition(phrase):
    """Get the definition of phrase."""
    phrase = phrase.decode('utf-8')
    return DCSCopyTextDefinition(None, phrase, (0, len(phrase)))


def main():
    try:
        print("Welcome! Press `C-c' or `C-d' to exit")
        while True:
            phrase = raw_input("DICT> ")
            if len(phrase) == 0:
                continue
            definition = get_definition(phrase)
            if definition:
                print(definition)
            else:
                print("error: No definition found for `{0}'.".format(phrase))
    except (KeyboardInterrupt, EOFError):
        pass

if __name__ == '__main__':
    main()
