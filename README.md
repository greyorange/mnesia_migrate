# mnesia_migrate
A tool to upgrade/downgrade schema and migrate data between different versions of mnesia.

[![Build Status](https://travis-ci.org/greyorange/mnesia_migrate.svg?branch=master)](https://travis-ci.org/greyorange/mnesia_migrate)

# Installation

* run `make deps` if you are using erlang.mk

# Usage

* Add contents of priv/sys.config to your application's sys.config or use this config in your release.
* Use forward slash in defining directory names in sys.config
* To use apply_downgrades/1, use a number in the argument which will downgrade that many revisions from currently applied head.
* To enable print statements of library, add {debug, true} in sys.config
* Run `detect_revision_sequence_conflicts()` to get a list of revision id where there is a fork in the revision tree.

# License

MIT License
