PROJECT := pagerduty
ARCH := $(shell uname -m)
DOCKER_IMAGE := "gerbil/gerbilxx:$(ARCH)"

default: linux-static-docker

deps:
	/opt/gerbil/bin/gxpkg install github.com/ober/oberlib

build: deps
	/opt/gerbil/bin/gxpkg link $(PROJECT) /src || true
	/opt/gerbil/bin/gxpkg build $(PROJECT)

linux-static-docker:
	docker run -it \
	-e GERBIL_PATH=/tmp/.gerbil \
	-e USER=$(USER) \
	-v $(PWD):/src \
	$(DOCKER_IMAGE) \
	make -C /src build

clean:
	rm -rf .gerbil

install:
	mv .gerbil/bin/$(PROJECT) /usr/local/bin/$(PROJECT)
