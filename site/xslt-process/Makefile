VERSION = $(shell egrep 'defconst[ ]*xslt-process-version' lisp/xslt-process.el | awk '{print $$NF}')

all clean distclean:
	for d in java etc doc; do \
	  (cd $$d; make VERSION=$(VERSION) $@); \
	done

dist:
	@(repository=`cat CVS/Root`; \
	 export CVSROOT=$$repository; \
	 rm -rf /tmp/xslt-process-$(VERSION); \
	 cd /tmp; \
	 cvs checkout -d xslt-process-$(VERSION) xslt-process; \
	 cd /tmp/xslt-process-$(VERSION); \
	 make all; make clean; \
	 find . -name 'CVS' -prune -exec rm -rf {} \; ;\
	 cd /tmp; \
	 tar zcf xslt-process-$(VERSION).tar.gz xslt-process-$(VERSION); \
	 rm -rf /tmp/xslt-process-$(VERSION); \
	 tag=release_`echo $(VERSION) | sed 's/\./_/g'`; \
	 echo "Tagging the repository with tag '$$tag'"; \
	 cvs -d $$repository rtag -F $$tag xslt-process; \
	 echo "The distribution is in /tmp/xslt-process-$(VERSION).tar.gz")
