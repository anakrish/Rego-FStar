SOURCE_FILES=$(wildcard *.fst *.fsti)
OUTPUT_DIR=./output
CACHE_DIR=$(OUTPUT_DIR)/cache
FSTAR=fstar.exe
ALREADY_CACHED=--already_cached '+Prims +FStar'
FSTAR_OPTIONS=--cache_checked_modules $(ALREADY_CACHED) --cache_dir $(CACHE_DIR) --odir $(OUTPUT_DIR)

all: prep build

test: build
	./Rego_Main.exe

.depend: $(SOURCE_FILES)
	$(FSTAR) --codegen OCaml $(FSTAR_OPTIONS) --dep full $^ --extract '* -Prims -FStar' --output_deps_to .depend

include .depend

verify: prep $(ALL_CHECKED_FILES)

extract: prep $(ALL_ML_FILES)

build: extract
	$(MAKE) -C $(OUTPUT_DIR)

$(CACHE_DIR)/%.checked:
	$(FSTAR) $(FSTAR_OPTIONS) $<

$(OUTPUT_DIR)/%.ml:
	$(FSTAR) --extract $(notdir $(subst .fst.checked,,$<)) --codegen OCaml $(FSTAR_OPTIONS) $(notdir $(subst .checked,,$<))

prep:
	mkdir -p $(CACHE_DIR)

.PHONY: prep build verify extract test