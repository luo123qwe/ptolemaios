@echo off

rem window编译C需要先设置环境
rem 安装vs, 把cl.exe和vcvarsall.bat放到环境变量
if not defined REBAR3_PTOLEMAIOS_VS (
    rem 改变当前编译环境
    call vcvarsall.bat x64
    rem 设置编码
    chcp 65001
)
SET REBAR3_PTOLEMAIOS_VS=true

rem 用werl开shell
if "%1"=="shell" (
    set ESCRIPT_EMULATOR=werl
) else (
    set ESCRIPT_EMULATOR=
)

setlocal
set rebarscript=%~f0
escript.exe "%rebarscript:.cmd=%" %*