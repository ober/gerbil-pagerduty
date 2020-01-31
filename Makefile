.PHONY: pagerduty

default: pagerduty

SYSTEM := $(shell uname -s)

ifeq ($(SYSTEM),Darwin)
SSL-BASE :=$(lastword $(wildcard /usr/local/Cellar/openssl/*/))
SED := sed
MYSQL-BASE := $(lastword $(wildcard /usr/local/Cellar/mysql/*/))
LIBYAML-BASE := $(lastword $(wildcard /usr/local/Cellar/libyaml/*/))
else
LDFLAGS := "-L/usr/lib -lssl -lyaml"
CPPFLAGS := "-I/usr/include"
LIBYAML-BASE := "/usr/include"
SED := sed
endif

pagerduty: $(eval CPPFLAGS := "-I$(SSL-BASE)include -I$(LIBYAML-BASE)include -I/usr/local/include")
pagerduty: $(eval LDFLAGS := "-L$(SSL-BASE)lib -L$(LIBYAML-BASE)lib -lz -lssl -lyaml -L/usr/local/lib")
pagerduty:
	gxc -O -o pd -static -exe -g -genv -cc-options $(CPPFLAGS) -ld-options $(LDFLAGS) -gsrc -gsc-flag -keep-c pagerduty/pd.ss

docker:
	docker run -e GERBIL_PATH=/dd/.gerbil -e PATH='/root/gerbil/bin:/usr/local/gambit/current/bin:/bin:/usr/bin:/sbin:/usr/sbin' -v $PWD:/dd -it jaimef/centos bash -c 'cd /dd && gxc -o dd -cc-options "-Bstatic -DOPENSSL_NO_KRB5 -I/usr/local/include -I/usr/local/ssl/include" -g -gsrc -genv -static -ld-options "-static -L/usr/lib64 -L/usr/local/ssl/lib -lssl -L/usr/local/lib -ldl -lyaml -lz" -exe pagerduty/pagerduty.ss'
