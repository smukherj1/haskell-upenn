
SOURCES = LogAnalysis.hs Log.hs
TARGET = LogAnalysis

run: $(TARGET)
	@./$(TARGET)

$(TARGET): $(SOURCES)
	@ghc -O3 $(SOURCES) -o $@ -main-is LogAnalysis

clean:
	@rm -f main *.hi *.o
	@echo Clean complete
