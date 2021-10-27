all: BevertonHolt.so Ricker.so contHockey.so

BevertonHolt.so: BevertonHolt.cpp
	R --vanilla -e "TMB::compile('BevertonHolt.cpp')"

Ricker.so: Ricker.cpp
	R --vanilla -e "TMB::compile('Ricker.cpp')"

contHockey.so: contHockey.cpp
	R --vanilla -e "TMB::compile('contHockey.cpp')"

clean:
	rm -rf *o
