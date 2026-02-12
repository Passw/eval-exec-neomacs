FORMS ?= cases/core.forms
EXPECTED ?= cases/core.expected.tsv
NEOVM_OUT ?= cases/core.neovm.tsv

.PHONY: oracle record check compare

oracle:
	./run-oracle.sh $(FORMS)

record:
	./run-oracle.sh $(FORMS) > $(EXPECTED)
	@echo "recorded oracle baseline: $(EXPECTED)"

check:
	@tmp_file="$$(mktemp)"; \
	./run-oracle.sh $(FORMS) > "$$tmp_file"; \
	diff -u $(EXPECTED) "$$tmp_file"; \
	rm -f "$$tmp_file"; \
	echo "oracle output matches expected baseline"

compare:
	./compare-results.sh $(EXPECTED) $(NEOVM_OUT)
