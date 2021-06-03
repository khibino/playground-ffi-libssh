
%: %.hs
	ghc -lssh -Wall -j -O --make $@

%.hs: %.hsc
	hsc2hs $^


gen_hs = \
	String.hs \
	Unistd.hs \
	Libssh.hs


target = \
	session \
	options \
	connect \
	auth_server


all: gen $(target)

gen: $(gen_hs)

clean:
	$(RM) $(target)
	$(RM) -f *.o *.hi
	$(RM) $(gen_hs)
