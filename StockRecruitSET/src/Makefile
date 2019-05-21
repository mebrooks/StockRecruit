all: BevertonHolt.so Ricker.so contHockey.so

BevertonHolt.so: BevertonHolt.cpp
	Rscript --vanilla -e "TMB::compile('BevertonHolt.cpp')"

Ricker.so: Ricker.cpp
	Rscript --vanilla -e "TMB::compile('Ricker.cpp')"

contHockey.so: contHockey.cpp
	Rscript --vanilla -e "TMB::compile('contHockey.cpp')"

clean:
	rm -rf *o