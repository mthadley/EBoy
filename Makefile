OUT = dist
ELM_MAIN = $(OUT)/index.js
ELM_FILES = $(shell find src -iname "*.elm")

.PHONY: all clean test watch format

all: $(ELM_MAIN) $(OUT)/index.html

clean:
	@rm -fr $(OUT) elm-stuff tests/elm-stuff

$(ELM_MAIN): $(ELM_FILES)
	yes | elm make src/Main.elm --output $@

$(OUT)/index.html: src/index.html
	@cp $< $@

src/index.html:
	@mkdir -p $(OUT)

test:
	@elm-test

watch:
	@find src | entr make

format:
	@elm-format --yes src/ tests/
