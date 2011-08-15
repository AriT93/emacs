@echo off

rem Written by Frank Schmitt <ich@frank-schmitt.net>
rem based on the work by David Charlap (shamino@writeme.com)
rem .
rem Clear PWD so emacs doesn't get confused
set GNUS_PWD_SAVE=%PWD%
set PWD=
set ERROR=:

if "%1" == "" goto usage

rem Emacs 20.7 no longer includes emacs.bat. Use emacs.exe if the batch file is
rem not present -- this also fixes the problem about too many parameters on Win9x.
if exist %1\emacs.bat goto ebat
if exist %1\emacs.exe goto eexe
if exist %1\xemacs.exe goto xemacs
goto noemacs

:ebat
set EMACS=emacs.bat
echo.
echo ***************************************************************************
echo * Using emacs.bat (If you've got en Emacs >= 20.3 please remove Emacs.bat,
echo * it isn't needed anymore.
echo ***************************************************************************
echo.
goto emacs

:eexe
set EMACS=emacs.exe
echo.
echo ***************************************************************************
echo * Using emacs.exe
echo ***************************************************************************
echo.
goto emacs

:emacs
if not "%2" == "/copy" goto emacsnocopy
if not exist %1\..\site-lisp mkdir %1\..\site-lisp
if not exist %1\..\site-lisp\gnus mkdir %1\..\site-lisp\gnus
if not exist %1\..\site-lisp\subdirs.el set subdirwarning=yes
:emacsnocopy
set EMACS_ARGS=-batch -q -no-site-file
set GNUS_INFO_DIR=%1\..\info
set GNUS_LISP_DIR=%1\..\site-lisp\gnus\lisp
set GNUS_ETC_DIR=%1\..\site-lisp\gnus\etc
goto lisp

:xemacs
set EMACS=xemacs.exe
if not "%2" == "/copy" goto xemacsnocopy
if not exist %1\..\..\site-packages\ mkdir %1\..\..\site-packages\
if not exist %1\..\..\site-packages\info mkdir %1\..\..\site-packages\info
if not exist %1\..\..\site-packages\lisp mkdir %1\..\..\site-packages\lisp
if not exist %1\..\..\site-packages\etc mkdir %1\..\..\site-packages\etc
:xemacsnocopy
set EMACS_ARGS=-batch -no-autoloads
set GNUS_INFO_DIR=%1\..\..\site-packages\info
set GNUS_LISP_DIR=%1\..\..\site-packages\lisp\gnus
set GNUS_ETC_DIR=%1\..\..\site-packages\etc
echo.
echo ***************************************************************************
echo * Using xemacs.exe
echo ***************************************************************************
echo.
goto lisp

:lisp
set EMACSBATCH=call %1\%EMACS% %EMACS_ARGS%
cd lisp
if exist gnus-load.el del gnus-load.el
echo.
echo Stand by while generating autoloads.
echo.
%EMACSBATCH% -l ./dgnushack.el -f dgnushack-make-cus-load .
if ErrorLevel 1 set ERROR=make-cus-load
%EMACSBATCH% -l ./dgnushack.el -f dgnushack-make-auto-load .
if ErrorLevel 1 set ERROR=%ERROR%,make-auto-load
%EMACSBATCH% -l ./dgnushack.el -f dgnushack-make-load
if ErrorLevel 1 set ERROR=%ERROR%,make-load
echo.
echo Stand by while compiling lisp files.
echo.
%EMACSBATCH% -l ./dgnushack.el -f dgnushack-compile
if ErrorLevel 1 set ERROR=%ERROR%,compile

if not "%2" == "/copy" goto infotest
echo.
echo Stand by while copying lisp files.
echo.
if not exist %GNUS_LISP_DIR% mkdir %GNUS_LISP_DIR%
xcopy /R /Q /Y *.el* %GNUS_LISP_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-lisp
goto infotest

:infotest
cd ..\texi
if exist sieve del sieve
makeinfo sieve.texi
if exist sieve goto minfo
REM It seems that makeinfo isn't available
set EMACSINFO=%EMACSBATCH% -l infohack.el -f batch-makeinfo
echo.
echo ***************************************************************************
echo * Using infohack.el, if you've got makeinfo.exe put it in PATH.
echo ***************************************************************************
echo.
goto info

:minfo
set EMACSINFO=makeinfo
echo.
echo ***************************************************************************
echo * Using makeinfo
echo ***************************************************************************
echo.
goto info

:info
echo.
echo Stand by while generating info files.
echo.
%EMACSINFO% emacs-mime.texi
if ErrorLevel 1 set ERROR=%ERROR%,emacs-mime.texi
%EMACSINFO% gnus.texi
if ErrorLevel 1 set ERROR=%ERROR%,gnus.texi
%EMACSINFO% sieve.texi
if ErrorLevel 1 set ERROR=%ERROR%,sieve.texi
%EMACSINFO% pgg.texi
if ErrorLevel 1 set ERROR=%ERROR%,pgg.texi
%EMACSINFO% message.texi
if ErrorLevel 1 set ERROR=%ERROR%,message.texi

if not "%2" == "/copy" goto nocopy
if not exist %GNUS_INFO_DIR% mkdir %GNUS_INFO_DIR%

echo.
echo Stand by while copying info files.
echo.
xcopy /R /Q /Y gnus       %GNUS_INFO_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-gnus-info
xcopy /R /Q /Y gnus-?     %GNUS_INFO_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-gnus-x-info
xcopy /R /Q /Y gnus-??    %GNUS_INFO_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-gnus-xx-info
xcopy /R /Q /Y message    %GNUS_INFO_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-message-info
if exist message-1 xcopy /R /Q /Y message-?  %GNUS_INFO_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-message-x-info
xcopy /R /Q /Y emacs-mime %GNUS_INFO_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-emacs-mime-info
xcopy /R /Q /Y sieve      %GNUS_INFO_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-sieve-info
xcopy /R /Q /Y pgg        %GNUS_INFO_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-pgg-info

echo.
echo ***************************************************************************
echo * You should add the following lines to
echo * %GNUS_INFO_DIR%\dir
echo * if they aren't already there:
echo *
echo * * PGG: (pgg).	Emacs interface to various PGP implementations.
echo * * Sieve: (sieve).	Managing Sieve scripts in Emacs.
echo ***************************************************************************
echo.

:etc
cd ..\etc
echo.
echo Stand by while copying etc files.
echo.
if not exist %GNUS_ETC_DIR% mkdir %GNUS_ETC_DIR%
xcopy /R /Q /Y gnus-tut.txt %GNUS_ETC_DIR%
if ErrorLevel 1 set ERROR=%ERROR%,copy-etc-gnus-tut-txt
if not exist %GNUS_ETC_DIR%\gnus mkdir %GNUS_ETC_DIR%\gnus
xcopy /R /Q /Y .\gnus\* %GNUS_ETC_DIR%\gnus\
if ErrorLevel 1 set ERROR=%ERROR%,copy-etc-gnus-*
if not exist %GNUS_ETC_DIR%\smilies mkdir %GNUS_ETC_DIR%\smilies
xcopy /R /Q /Y .\smilies\* %GNUS_ETC_DIR%\smilies\
if ErrorLevel 1 set ERROR=%ERROR%,copy-etc-smilies-*
goto warnings

:nocopy
echo.
echo ***************************************************************************
echo * You chose not to copy the files, therefore you should add the
echo * following lines to the TOP of your [X]emacs customization file:
echo *
echo * (add-to-list 'load-path "/Path/to/gnus/lisp")
echo * (if (featurep 'xemacs)
echo *     (add-to-list 'Info-directory-list "c:/Path/to/gnus/texi/")
echo *   (add-to-list 'Info-default-directory-list "c:/Path/to/gnus/texi/")
echo * (require 'gnus-load)
echo *
echo * Replace c:/Path/to/gnus with the Path where your new Gnus is (that's here
echo * and yes, you've got to use forward slashes).
echo ***************************************************************************
echo.

:warnings
if not "%subdirwarning%" == "yes" goto warngnusload
echo.
echo ***************************************************************************
echo * There's no subdirs.el file in your site-lisp directory, you should
echo * therefor add the following line to the TOP of your Emacs
echo * customization file:
echo *
echo * (add-to-list 'load-path "/Path/to/emacs-site-lisp-directory/gnus/lisp")
echo * (require 'gnus-load)
echo * Yes, it must be forward slashes.
echo ***************************************************************************
echo.
goto warnerrors

:warngnusload
echo.
echo ***************************************************************************
echo * You should add the following line to the TOP of your Emacs
echo * customization file:
echo *
echo * (require 'gnus-load)
echo ***************************************************************************
echo.

:warnerrors
if "%ERROR%"==":" goto noerrors
set errorlevel=1
echo.
echo ***************************************************************************
echo * WARNING ERRORS OCCURRED!
echo * You should look for error messages in the output of the called programs
echo * and try to find out what exactly went wrong.
echo * Errors occured in the following modules:
echo * %ERROR%
echo ***************************************************************************
echo.
goto done

:noerrors
set errorlevel=0

:done
cd ..
goto end

:noemacs
echo.
echo ***************************************************************************
echo * Unable to find emacs.exe or xemacs.exe on the path you specified!
echo * STOP!
echo ***************************************************************************
echo.
goto usage

:usage
echo.
echo ***************************************************************************
echo * Usage: make.bat :[X]Emacs-exe-dir: [/copy]
echo *
echo * where: :[X]Emacs-exe-dir: is the directory your
echo *           emacs.exe respectively xemacs.exe resides in,
echo *           e.g. G:\Programme\XEmacs\XEmacs-21.4.11\i586-pc-win32\
echo *           or G:\Emacs\bin
echo *        /copy indicates that the compiled files should be copied to your
echo *           emacs lisp, info, and etc site directories.
echo *
echo * Note: If you have Emacs/w3 you should set the environment variable
echo *       W3DIR to the directory where w3 is installed eg.
echo *                set W3DIR=d:\lisp\w3-4.0pre46\lisp
echo ***************************************************************************
echo.

:end
rem Restore environment variables
set PWD=%GNUS_PWD_SAVE%
set GNUS_PWD_SAVE=
set EMACSBATCH=
set GNUS_LISP_DIR=
set GNUS_INFO_DIR=
set GNUS_ETC_DIR=
set subdirwarning=
set ERROR=
