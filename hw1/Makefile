
OBJS= creditcard hanoi

all: $(OBJS)

% : %.hs
	@echo 'Compiling $< -> [$@]'
	@ghc -O $< -o $@

clean:
	@-rm $(OBJS) *.hi *.o