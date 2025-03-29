CPPL := ../../../../../build/cppl
EXAMPLES := baysian-parameter-estimation.json baysian-parameter-estimation-sensitivities.json ode-sensitivites.json rode-faster.json

.PHONY: all
all: $(EXAMPLES)

.PHONY: plot
plot:
	python plot.py

%.exe: %.mc
	$(CPPL) --seed 1 --test --dppl-typecheck --output $@ $<

%.json: %.exe
	./$<

.PHONY: clean
clean:
	rm -rf *.json *.exe
