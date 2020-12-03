@echo off
rem window编译C需要先设置环境
rem 安装vs, 把cl.exe和vcvarsall.bat放到环境变量
rem 改变当前编译环境
call vcvarsall.bat x64
rem 设置编码
chcp 65001


setlocal
set rebarscript=%~f0
escript.exe "%rebarscript:.cmd=%" %*