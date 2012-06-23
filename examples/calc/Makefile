all: calc_client calc_server.so

calc_client: calc_client.c
	cp ../../bin/libdist_client.so .
	gcc calc_client.c libdist_client.so -o calc_client

calc_server.so: calc_server.c
	cp ../../bin/libdist_server.so .
	gcc calc_server.c libdist_server.so \
		-I ../../apps/libdist_c/ \
		-arch i386\
		-dynamiclib -o calc_server.so

clean:
	rm -v libdist_client.so libdist_server.so
	rm -v calc_client calc_server.so
