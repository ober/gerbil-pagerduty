PROJECT := pagerduty
ARCH := $(shell uname -m)
PWD := $(shell pwd)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)-master"
UID := $(shell id -u)
GID := $(shell id -g)

default: linux-static-docker

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build -R $(PROJECT)

linux-static-docker:
	docker run -t \
	-u "$(UID):$(GID)" \
	-v $(PWD):/src \
	$(DOCKER_IMAGE) \
	make -C /src build

clean:
	rm -rf .gerbil

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
