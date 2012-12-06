all: code

code: clean
	erl -make

clean:
	rm -rfv *.beam *.rel *.script *.boot erl_crash.dump
