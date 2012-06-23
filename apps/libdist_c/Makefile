BIN_DIR := ../../bin

all: libdist_client.so libdist_server.so

libdist_client.so: libdist_client.c
	gcc -dynamiclib -o libdist_client.so \
		-I/usr/local/lib/erlang/lib/erl_interface-3.7.5/include/ \
		-L/usr/local/lib/erlang/lib/erl_interface-3.7.5/lib/ \
		libdist_client.c -lei
	mkdir -p $(BIN_DIR) ; mv libdist_client.so $(BIN_DIR)

libdist_server.so: libdist_server.c
	gcc -dynamiclib -o libdist_server.so \
		-undefined dynamic_lookup \
		-I /usr/local/lib/erlang/usr/include/ \
		-arch i386\
		libdist_server.c
	mkdir -p $(BIN_DIR) ; mv libdist_server.so $(BIN_DIR)


clean:
	rm -rv $(BIN_DIR)/libdist_server.so $(BIN_DIR)/libdist_client.so
	rmdir $(BIN_DIR)
