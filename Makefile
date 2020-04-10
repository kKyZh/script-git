# practice of makefile
# @ not show this line but show the content of echo
# name: execute line must have a tab at beginning
#

#fdf2xyz.o: #fdf2xyz.f90
#$(FC) -c fdf2xyz.f90
#fdf2xyzf90: #fdf2xyz.f90
#$(FC) -o fdf2xyz.f90 fdf2xyz.o
	#"$<" meaning?
	#$@ $^ meaning?
	#.f source file, .o target file
	#$< source file, $@ target file
	#$< first prerequisite
	#$^ all prerequisite
	#$@ all target file
	#$@ all, $< abc.f90, $^ abc.f90 def.f90
# suffixes rules .a.b, like .f.o, when you see .f, make a .o file
.SUFFIXES: .f .f90 .F .F90 .o

all: say_hello fdf2xyzf90

FC=ifort
FC:=ifort

OBJ= fdf2xyz.o

say_hello:
	@echo "Hello man"

.f90.o: 
	$(FC) -c $<

fdf2xyzf90: $(OBJ) #fdf2xyz.o
	$(FC) -o $@ $^

clean:
	@echo "cleaning up..."
	rm -f *.o *.mod fdf2xyzf90
