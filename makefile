DIST_FOLDER=dist
MAIN=$(DIST_FOLDER)/main.js

all: elm

clean:
	@rm -fr $(DIST_FOLDER)

elm:
	elm-make src/Main.elm --output $(MAIN)

test:
	@elm-test
