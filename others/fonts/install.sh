#/bin/sh

# 这个脚本会安装all-the-icons插件的字体

# https://wiki.debian.org/Fonts
cp *.ttf ~/.fonts
fc-cache -fv
