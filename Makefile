REPO=refuge
REFUGE_TAG=	$(shell git describe --tags --always)
REVISION?=	$(shell echo $(REFUGE_TAG) | sed -e 's/^$(REPO)-//')
PKG_VERSION?=	$(shell echo $(REVISION) | tr - .)
WITHOUT_CURL?=1
REBAR?=./rebar
SUPPORT_DIR=support
REBAR_MASTER=git://github.com/refuge/rebar.git

DESTDIR?=
DISTDIR= rel/archive


.PHONY: rebar rel stagedevrel deps

all: deps compile

bootstrap = if [ ! -d $(SUPPORT_DIR)/rebar ]; then \
			mkdir -p $(SUPPORT_DIR)/rebar && \
			git clone $(REBAR_MASTER) $(SUPPORT_DIR)/rebar; \
			fi
rebar:
	@mkdir -p $(SUPPORT_DIR)
	@echo "==> fetch rebar sources" 
	@$(call bootstrap) > /dev/null
	@echo "==> build rebar"
	@rm -rf  $(SUPPORT_DIR)/rebar/rebar
	@(cd $(SUPPORT_DIR)/rebar && ./bootstrap)
	@cp $(SUPPORT_DIR)/rebar/rebar .

compile:
	@WITHOUT_CURL=$(WITHOUT_CURL) $(REBAR) compile

deps: rebar
	@$(REBAR) get-deps

clean: devclean
	@$(REBAR) clean

distclean: clean devclean relclean
	@$(REBAR) delete-deps
	@rm -rf support/rebar

rel: relclean deps
	@WITHOUT_CURL=$(WITHOUT_CURL) $(REBAR) compile generate

relclean:
	@rm -rf rel/refuge

##
## dev targets
##

dev: devclean all devrel
	@echo "\n\
Development nodes are built, and can be started using ./dev/dev[123]/bin/refuge.\n"

devrel: dev1 dev2 dev3

dev1 dev2 dev3:
	@mkdir -p dev
	@(cd rel && WITHOUT_CURL=$(WITHOUT_CURL) ../$(REBAR) generate target_dir=../dev/$@ overlay_vars=vars/$@.config)
	# generate a custom certificate for each dev node.
	@./dev/$@/bin/refuge makecert

devclean:
	@rm -rf dev

##
## release tarballs
##

archive = git archive --format=tar --prefix=$(1)/ HEAD | (cd $(2) && tar xf -)

buildtar = mkdir distdir && \
		 git clone . distdir/refuge-clone && \
		 cd distdir/refuge-clone && \
		 git checkout $(REFUGE_TAG) && \
		 $(call archive,$(REFUGE_TAG),..) && \
		 mkdir ../$(REFUGE_TAG)/deps && \
		 make deps; \
		 for dep in deps/*; do \
                     cd $${dep} && \
                     $(call archive,$${dep},../../../$(REFUGE_TAG)) && \
                     mkdir -p ../../../$(REFUGE_TAG)/$${dep}/priv && \
                     git rev-list --max-count=1 HEAD > ../../../$(REFUGE_TAG)/$${dep}/priv/git.vsn && \
                     cd ../..; done

distdir: rebar
	$(if $(REFUGE_TAG), $(call buildtar), $(error "You can't generate a release tarball from a non-tagged revision. Run 'git checkout <tag>', then 'make dist'"))

dist $(REFUGE_TAG).tar.gz: distdir
	cd distdir; \
	tar czf ../$(REFUGE_TAG).tar.gz $(REFUGE_TAG)


package: dist
	$(MAKE) -C package package

pkgclean:
	$(MAKE) -C package pkgclean

.PHONY: package

export PKG_VERSION REPO REVISION REFUGE_TAG
