BIN_DIR := ../../bin

all: libdist_client.so libdist_server.so

libdist_client.so: libdist_client.c
	gcc $(GCC_FLAGS) -o libdist_client.so \
		-I$(ERLANG_LIB_DIR_erl_interface)/include/ \
		-L$(ERLANG_LIB_DIR_erl_interface)/lib/ \
		libdist_client.c -lei
	mkdir -p $(BIN_DIR) ; mv libdist_client.so $(BIN_DIR)

libdist_server.so: libdist_server.c
	gcc $(GCC_FLAGS) -o libdist_server.so \
		-I $(ERLANG_ROOT_DIR)/erts-$(ERLANG_LIB_VER_erts)/include/ \
		libdist_server.c
	mkdir -p $(BIN_DIR) ; mv libdist_server.so $(BIN_DIR)

clean:
	rm -v $(BIN_DIR)/libdist_server.so $(BIN_DIR)/libdist_client.so
	rmdir $(BIN_DIR)
