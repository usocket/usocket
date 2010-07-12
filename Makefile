# $Id$
# $URL$

clean:
	find . \( -name "*~" -o -name "*.err" -o -name "*.x86f" -o -name "*.lib" -o -name "*.fas" -o -name "*.*fasl" -o -name "*.faslmt" -o -name "*.abcl" -o -name "*.*fsl" -o -name "*.o" -o -name "*.sse2f" \) -delete

commit:
	make clean; svn up; svn ci
