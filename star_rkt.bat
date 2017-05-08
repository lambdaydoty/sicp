@echo off
echo.
echo -i           : enables interactive mode. (start a REPL)
echo -p neil/sicp : enables the SICP package. (inc, dec, nil, ...)
echo -l xrepl     : enables extended REPL mode. (command history, tab completion, ...)
echo.
@echo on
racket -i -p neil/sicp -l xrepl
