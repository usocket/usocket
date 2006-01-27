# $Id$
# $Source$

clean:
	find -name -o -name "*~" -o -name "*.err" -o -name "*.x86f" -o -name "*.lib" -o -name "*.fas" -o -name "*.fasl" -o -name "*.faslmt" -o -name "*.ufsl" | xargs rm 

commit:
	make clean; cvs up; cvs ci

