./: {*/ -build/ -docs/ -examples/} doc{README.md} manifest

# Don't install tests.
#
tests/: install = false
