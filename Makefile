PROJECT := pagerduty
ARCH := $(shell uname -m)
PWD := $(shell pwd)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"
UID := $(shell id -u)
GID := $(shell id -g)

default: linux-static-docker

check-root:
	@if [ "${UID}" -eq 0 ]; then \
	git config --global --add safe.directory /src; \
	fi

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps check-root
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build -R $(PROJECT)

linux-static-docker:
	docker run -t \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src \
	$(DOCKER_IMAGE) \
	make -C /src build

clean:
	rm -rf .gerbil manifest.ss

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
