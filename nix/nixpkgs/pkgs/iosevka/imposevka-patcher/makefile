SRCFILES=$(shell ls src/)
SOURCES=$(addprefix src/,$(SRCFILES))
TARGETS=$(addprefix dst/,$(subst iosevka-,imposevka-,$(SRCFILES)))
SOURCE_OTDS=$(addprefix tmp/,$(subst .ttf,.otd,$(SRCFILES)))
TARGET_OTDS=$(addprefix tmp/,$(subst iosevka-,imposevka-,$(subst .ttf,.otd,$(SRCFILES))))

all : $(TARGETS)

tmp/nerd.otd : nerd.ttf
	otfccdump $< -o $@ --glyph-name-prefix .2. --ignore-hints

$(TARGETS) : dst/imposevka-%.ttf : tmp/imposevka-%.otd
	@otfccbuild $< -o $@ --keep-average-char-width --ship
	@echo Patched $@
$(TARGET_OTDS) : tmp/imposevka-%.otd : merge.js tmp/iosevka-%.otd tmp/nerd.otd
	@node $^ $@
$(SOURCE_OTDS) : tmp/iosevka-%.otd : src/iosevka-%.ttf
	@otfccdump $< -o $@ --glyph-name-prefix .1.