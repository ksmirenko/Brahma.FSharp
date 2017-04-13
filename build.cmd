@echo off
cls

.paket\paket.bootstrapper.exe
if errorlevel 1 (
  exit /b %errorlevel%
)

.paket\paket.exe restore
if errorlevel 1 (
  exit /b %errorlevel%
)

REM Generate lexer and parser for OpenCLTranslator
packages\YC.SDK\tools\YC.FsLex.exe ^
    src\Brahma.FSharp.OpenCL.OpenCLTranslator\Lexer.fsl ^
    --unicode ^
    -o src\Brahma.FSharp.OpenCL.OpenCLTranslator\Lexer.fsl.fs
packages\YC.SDK\tools\YC.YaccConstructor.exe ^
    -i src\Brahma.FSharp.OpenCL.OpenCLTranslator\Parser.yrd

IF NOT EXIST build.fsx (
  .paket\paket.exe update
  packages\build\FAKE\tools\FAKE.exe init.fsx
)
packages\build\FAKE\tools\FAKE.exe build.fsx %*
