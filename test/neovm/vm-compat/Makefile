FORMS ?= cases/core.forms
EXPECTED ?= cases/core.expected.tsv
NEOVM_OUT ?= cases/core.neovm.tsv

read_case_list = $(strip $(shell awk 'NF && $$1 !~ /^#/' $(1) | tr '\n' ' '))

CASES_LIST ?= cases/default.list
CASES ?= $(call read_case_list,$(CASES_LIST))
NEOVM_ONLY_CASES_LIST ?= cases/neovm-only.list
NEOVM_ONLY_CASES ?= $(call read_case_list,$(NEOVM_ONLY_CASES_LIST))
ALL_NEOVM_CASES ?= $(CASES) $(NEOVM_ONLY_CASES)
INTROSPECTION_CASES_LIST ?= cases/introspection.list
INTROSPECTION_CASES ?= $(call read_case_list,$(INTROSPECTION_CASES_LIST))
THREAD_CASES_LIST ?= cases/thread.list
THREAD_CASES ?= $(call read_case_list,$(THREAD_CASES_LIST))
CASE_LIST_FILES ?= $(CASES_LIST) $(NEOVM_ONLY_CASES_LIST) $(INTROSPECTION_CASES_LIST) $(THREAD_CASES_LIST)
BENCH_SOURCE ?= cases/load-policy-fixtures/vm-policy-cache-probe.el
BENCH_ITERS ?= 100
ERT_ALLOWLIST ?= cases/ert-allowlist-smoke.txt
ERT_EXPECTED ?= cases/ert-allowlist-smoke.expected.tsv
ERT_LOAD_FILES ?= cases/ert-allowlist-fixtures/smoke-tests.el

.PHONY: oracle neovm record check compare check-neovm record-all check-all check-all-neovm check-all-neovm-only check-introspection-neovm check-thread-neovm validate-case-lists ert-oracle record-ert-allowlist check-ert-allowlist bench-load-cache

oracle:
	./run-oracle.sh $(FORMS)

neovm:
	./run-neovm.sh $(FORMS)

record:
	./run-oracle.sh $(FORMS) > $(EXPECTED)
	@echo "recorded oracle baseline: $(EXPECTED)"

check:
	@set -e; \
	tmp_file="$$(mktemp)"; \
	./run-oracle.sh $(FORMS) > "$$tmp_file"; \
	diff -u $(EXPECTED) "$$tmp_file"; \
	rm -f "$$tmp_file"; \
	echo "oracle output matches expected baseline"

compare:
	./compare-results.sh $(EXPECTED) $(NEOVM_OUT)

check-neovm:
	@set -e; \
	tmp_file="$$(mktemp)"; \
	./run-neovm.sh $(FORMS) > "$$tmp_file"; \
	STRICT_FORM=1 ./compare-results.sh $(EXPECTED) "$$tmp_file"; \
	rm -f "$$tmp_file"; \
	echo "neovm output matches oracle baseline"

record-all:
	@set -e; \
	for case in $(CASES); do \
		echo "== $$case =="; \
		$(MAKE) --no-print-directory record FORMS="$$case.forms" EXPECTED="$$case.expected.tsv"; \
	done

check-all:
	@set -e; \
	for case in $(CASES); do \
		echo "== $$case =="; \
		$(MAKE) --no-print-directory check FORMS="$$case.forms" EXPECTED="$$case.expected.tsv"; \
	done

check-all-neovm:
	@set -e; \
	for case in $(ALL_NEOVM_CASES); do \
		echo "== $$case =="; \
		$(MAKE) --no-print-directory check-neovm FORMS="$$case.forms" EXPECTED="$$case.expected.tsv"; \
	done

check-all-neovm-only:
	@set -e; \
	for case in $(NEOVM_ONLY_CASES); do \
		echo "== $$case =="; \
		$(MAKE) --no-print-directory check-neovm FORMS="$$case.forms" EXPECTED="$$case.expected.tsv"; \
	done

check-introspection-neovm:
	@set -e; \
	for case in $(INTROSPECTION_CASES); do \
		echo "== $$case =="; \
		$(MAKE) --no-print-directory check-neovm FORMS="$$case.forms" EXPECTED="$$case.expected.tsv"; \
	done

check-thread-neovm:
	@set -e; \
	for case in $(THREAD_CASES); do \
		echo "== $$case =="; \
		$(MAKE) --no-print-directory check-neovm FORMS="$$case.forms" EXPECTED="$$case.expected.tsv"; \
	done

validate-case-lists:
	@set -e; \
	for list in $(CASE_LIST_FILES); do \
		echo "== $$list =="; \
		if [ ! -f "$$list" ]; then \
			echo "missing list file: $$list"; \
			exit 1; \
		fi; \
		tmp="$$(mktemp)"; \
		awk 'NF && $$1 !~ /^#/' "$$list" > "$$tmp"; \
		dups="$$(sort "$$tmp" | uniq -d)"; \
		if [ -n "$$dups" ]; then \
			echo "duplicate entries in $$list:"; \
			echo "$$dups"; \
			rm -f "$$tmp"; \
			exit 1; \
		fi; \
		while IFS= read -r case; do \
			[ -f "$$case.forms" ] || { echo "missing forms file: $$case.forms"; rm -f "$$tmp"; exit 1; }; \
			[ -f "$$case.expected.tsv" ] || { echo "missing expected file: $$case.expected.tsv"; rm -f "$$tmp"; exit 1; }; \
		done < "$$tmp"; \
		rm -f "$$tmp"; \
	done; \
	echo "case lists validated"

ert-oracle:
	./run-ert-allowlist.sh $(ERT_ALLOWLIST) $(ERT_LOAD_FILES)

record-ert-allowlist:
	./run-ert-allowlist.sh $(ERT_ALLOWLIST) $(ERT_LOAD_FILES) > $(ERT_EXPECTED)
	@echo "recorded ERT allowlist baseline: $(ERT_EXPECTED)"

check-ert-allowlist:
	@set -e; \
	tmp_file="$$(mktemp)"; \
	./run-ert-allowlist.sh $(ERT_ALLOWLIST) $(ERT_LOAD_FILES) > "$$tmp_file"; \
	diff -u $(ERT_EXPECTED) "$$tmp_file"; \
	rm -f "$$tmp_file"; \
	echo "ERT allowlist output matches expected baseline"

bench-load-cache:
	./bench-load-cache.sh $(BENCH_SOURCE) $(BENCH_ITERS)
