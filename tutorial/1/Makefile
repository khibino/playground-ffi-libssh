
%: %.hs
	ghc -lssh -Wall --make $@

%.hs: %.hsc
	hsc2hs $^


gen_hs = \
	String.hs \
	Libssh.hs


target = \
	session \
	options \
	connect \
	auth_server


all: $(gen_hs) $(target)

clean:
	$(RM) $(target)
	$(RM) -f *.o *.hi
	$(RM) $(gen_hs)
