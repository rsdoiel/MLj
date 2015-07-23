@echo off
if "%MLJBIN%"=="" goto badbin
set RUN=%MLJBIN%\run.x86-win32.exe
if EXIST %RUN% goto haveruntime
echo SML/NJ runtime not present in MLj binaries directory
goto finish

:haveruntime
if "%1%"=="-java10" goto java10
if "%1%"=="-java11" goto java11

if EXIST %MLJBIN%\mlj-jdk1.1.1.x86-win32 goto java11
goto java10

:badbin
echo Set MLJBIN environment variable to binaries directory e.g. C:\mlj\bin
goto finish

:java11
%RUN% @SMLload=%MLJBIN%\mlj-jdk1.1.1 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto finish

:java10
%RUN% @SMLload=%MLJBIN%\mlj-jdk1.0.2 %1 %2 %3 %4 %5 %6 %7 %8 %9
goto finish

:finish

