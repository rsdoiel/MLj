@echo off

if "%MLJBIN%"=="" goto badbin
if "%CM_PATH%"=="" goto badcm
set RUN=%CM_PATH%\..\bin\.run\run.x86-win32.exe
if EXIST %RUN% goto haveruntime
echo Cannot find SML/NJ runtime file %RUN%
goto finish

:haveruntime
copy %RUN% %MLJBIN%
if "%1%"=="-java10" goto java10
if "%1%"=="-java11" goto java11

:java11
sml-cm <src/build.sml
goto finish

:java10
sml-cm <src/build102.sml
goto finish

:badbin
echo Set MLJBIN environment variable to binaries directory e.g. C:\mlj\bin
goto finish

:badcm
echo Set CM_PATH environment variable to SML/NJ library directory e.g. C:\sml\lib
goto finish

:finish


