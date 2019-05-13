@echo off
setlocal

dotnet restore WebSharper.sln
dotnet restore WebSharper.Compiler.sln
if errorlevel 1 exit /b %errorlevel%

set DOTNETSOLUTION="WebSharper.Compiler.sln;WebSharper.sln"
call paket-files\wsbuild\github.com\dotnet-websharper\build-script\update.cmd
call paket-files\wsbuild\github.com\dotnet-websharper\build-script\build.cmd %*
