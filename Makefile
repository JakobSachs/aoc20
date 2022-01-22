CC = ghci
days = day1 day2


%: src/%.hs
	$(CC) -o $@.bin $<

.PHONY: clean
clean:
	rm -f src/*.hi src/*.o *.bin
