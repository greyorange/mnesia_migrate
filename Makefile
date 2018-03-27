PROJECT = mnesia_migrate
PROJECT_DESCRIPTION = An application to upgrade or downgrade mnesia database
PROJECT_VERSION = 0.0.1

DEPS = uuid erlydtl
TEST_DEPS = eunit_formatters

dep_uuid = git https://github.com/avtobiff/erlang-uuid v0.5.1
dep_erlydtl = git https://github.com/erlydtl/erlydtl 032746b
dep_eunit_formatters = git https://github.com/seancribbs/eunit_formatters v0.5.0

EUNIT_OPTS = no_tty, {report, {eunit_progress, [colored]}}

include erlang.mk
