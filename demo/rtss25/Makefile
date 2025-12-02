# Compiler and tool definitions
CXX := g++
CC := gcc
AR := ar
PYTHON := python3


# Directories
TFLITE_MICRO_DIR := tflite-micro
TFLITE_SRC := tflite-sources
LAST_TFLITE_SRC_FILE := $(TFLITE_SRC)/third_party/ruy/ruy/profiler/instrumentation.h
EXTERNAL_DIR := src
GENDIR := ./gen
OBJDIR := $(GENDIR)/obj
BINDIR := $(GENDIR)/bin
LIB := $(GENDIR)/libtflm.a

# Flags
COMMON_FLAGS := -DTF_LITE_STATIC_MEMORY -ffunction-sections -fdata-sections -fmessage-length=0 -fno-strict-aliasing
CXXFLAGS := -std=c++11 -O3 -ftree-vectorize -march=native -fopt-info-vec -fno-rtti -fno-exceptions -fno-threadsafe-statics $(COMMON_FLAGS)
CCFLAGS := -std=c11 $(COMMON_FLAGS)
ARFLAGS := -r
GTK4FLAGS := `pkg-config --cflags --libs opencv4`

# Include directories
INCLUDES := -I$(TFLITE_SRC) \
            -I$(TFLITE_SRC)/third_party/gemmlowp \
            -I$(TFLITE_SRC)/third_party/flatbuffers/include \
            -I$(TFLITE_SRC)/third_party/kissfft \
            -I$(TFLITE_SRC)/third_party/kissfft/tools \
            -I$(TFLITE_SRC)/third_party/ruy \
            -I$(GENDIR)/include \
            -Itestdata \
            -I$(EXTERNAL_DIR)

.PHONY: all clean libtflm person_detection tflite-sources

all: rtss25

# get the tflite sources
$(TFLITE_MICRO_DIR)/tensorflow:
	git submodule update --init $(TFLITE_MICRO_DIR)

# generate the files needed for the tflite library
$(LAST_TFLITE_SRC_FILE): $(TFLITE_MICRO_DIR)/tensorflow
	@echo "Creating Tensorflow-micro sources folder..."
	@# required or the script will fail, because for some reason Make's Entering directory message messes with the copy function in the python script
	@$(MAKE) -s tflite-silent 

tflite-silent:
	@cd $(TFLITE_MICRO_DIR) && \
	# fix shebangs so that shellscripts work on non-fhs systems \
	find . -name "*.sh" -type f -exec sed -i 's/#!\/bin\/bash/#!\/usr\/bin\/env bash/g' {} \; && \
	$(PYTHON) $$(find . -name "create_tflm_tree.py") ../$(TFLITE_SRC)

PERSON_DETECTION_INCLUDES := $(INCLUDES) \
                           -I$(EXTERNAL_DIR) \
                           -I$(GENDIR) \
                           -I$(GENDIR)/include

# Object file compilation
$(OBJDIR)/%.cc.o: %.cc
	@mkdir -p $(@D)
	$(CXX) $(CXXFLAGS) $(INCLUDES) -c $< -o $@

$(OBJDIR)/%.c.o: %.c
	@mkdir -p $(@D)
	$(CC) $(CCFLAGS) $(INCLUDES) -c $< -o $@

# Library creation
$(LIB): $(LAST_TFLITE_SRC_FILE)
	@$(eval TFLM_SRCS := $(shell find $(TFLITE_SRC)/tensorflow $(TFLITE_SRC)/signal -name "*.cc" -o -name "*.c"))
	@$(eval OBJS := $(patsubst %,$(OBJDIR)/%.o,$(TFLM_SRCS)))
	$(MAKE) $(OBJS)
	@mkdir -p $(@D)
	$(AR) $(ARFLAGS) $@ $(OBJS)

libtflm: $(LIB)

# Person detection specific targets
$(GENDIR)/include/gen_micro_mutable_op_resolver.h: $(TFLITE_MICRO_DIR)/tensorflow
	@mkdir -p $(@D)
	@echo "Searching for generate_micro_mutable_op_resolver_from_model.py..."
	@$(eval SCRIPT= $(shell find $(TFLITE_MICRO_DIR) -name "generate_micro_mutable_op_resolver_from_model.py"))
	$(PYTHON) $(SCRIPT) \
		--input_tflite_files person_detect.tflite \
		--common_tflite_path . \
		--output_dir $(GENDIR)/include

$(GENDIR)/include/person_detect_model_data.h: person_detect.tflite
	@mkdir -p $(@D)
	xxd -i $< > $@

UNENCRYPTED_SRCS := src/model_settings.cc rtss25.cpp

# Base flags without RTTI restrictions
BASE_CXXFLAGS := -std=c++11 \
								 -O3 -ftree-vectorize -march=native -fopt-info-vec \
                 -fno-exceptions \
                 -fno-threadsafe-statics \
                 -DTF_LITE_STATIC_MEMORY \
                 -ffunction-sections \
                 -fdata-sections \
                 -fmessage-length=0 \
                 -fno-strict-aliasing

UNENCRYPTED_CXXFLAGS := $(BASE_CXXFLAGS) -fexceptions

rtss25: libtflm $(GENDIR)/include/gen_micro_mutable_op_resolver.h $(GENDIR)/include/person_detect_model_data.h
	@mkdir -p $(BINDIR)
	$(CXX) $(UNENCRYPTED_CXXFLAGS) $(UNENCRYPTED_SRCS) $(PERSON_DETECTION_INCLUDES) $(LIB) $(GTK4FLAGS) -o $(BINDIR)/$@


# Cleanup
clean:
	rm -rf $(GENDIR) $(TFLITE_SRC)
