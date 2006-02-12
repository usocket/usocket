# $Id$
# $URL$

clean:
	find -name -o -name "*~" -o -name "*.err" -o -name "*.x86f" -o -name "*.lib" -o -name "*.fas" -o -name "*.fasl" -o -name "*.faslmt" -o -name "*.ufsl" -o -name "*.abcl" | xargs rm

commit:
	make clean; svn up; svn ci

