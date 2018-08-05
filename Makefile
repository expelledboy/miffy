REBAR ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")
REBAR3_URL=https://s3.amazonaws.com/rebar3/rebar3

all: $(REBAR) build

$(REBAR):
	curl -Lo rebar3 $(REBAR3_URL) || wget $(REBAR3_URL)
	chmod a+x rebar3

clean:
	$(REBAR) clean

build:
	$(REBAR) compile

test: $(REBAR)
	$(REBAR) do eunit, dialyzer

publish:
	$(REBAR) hex publish

.PHONY: all clean build test publish
