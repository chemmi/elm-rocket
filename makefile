SRC_FILES := src/Rocket.elm $(wildcard src/Rocket/*.elm)
MAIN := src/Rocket.elm

rocket.js: $(SRC_FILES)
	elm make $(MAIN) --output rocket.js
