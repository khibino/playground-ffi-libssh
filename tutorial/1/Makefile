
gen_hs = \
	String.hs \
	Unistd.hs \
	Libssh.hs


target = \
	session \
	options \
	connect \
	auth_server \
	auth_user \
	do_something


%.hs: %.hsc
	hsc2hs $^

%: %.hs
	ghc -lssh -Wall -j -O --make $@


all: gen $(target)

gen: $(gen_hs)

clean:
	$(RM) $(target)
	$(RM) -f *.o *.hi
	$(RM) $(gen_hs)
