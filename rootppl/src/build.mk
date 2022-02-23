#!/usr/bin/make -f

# NOTE: config.mk in $(BUILD) must exist prior to running this script

include $(BUILD)config.mk

srcs=$(shell find $(SRC) -name "*.cu")
objs=$(srcs:$(SRC)%.cu=$(BUILD)%.o)
deps=$(objs:.o=.d)

all: $(objs)

$(BUILD)%.o: $(SRC)%.cu $(BUILD)config.mk
	mkdir -p $(dir $@)
	$(CXX) $(FLAGS) -MMD -MP -c $< -o $@

-include $(DEPS)
