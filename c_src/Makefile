CC=gcc
CLANG=/usr/src/llvm/Debug/bin/clang

FLAGS=-fPIC -shared
ERL_ROOT=/usr/local/lib/erlang
 
all:
	$(CC) $(FLAGS) -o ../ebin/tokyo_nif.so tokyo_nif.c -I $(ERL_ROOT)/usr/include/ -ltokyocabinet

clang:
	$(CLANG) $(FLAGS) -o ../ebin/tokyo_nif.so tokyo_nif.c -I $(ERL_ROOT)/usr/include/ -ltokyocabinet 
