#!/usr/bin/env sh

infocmp xterm-256color > fancy.terminfo

sed -i 's/xterm-256color|xterm with 256 colors,/fancy|term with fancy capabilities,/' fancy.terminfo

echo '	smxx=\E[9m, rmxx=\E[29m,
	Smulx=\E[4:%p1%dm,
	Se=\E[2\sq, Ss=\E[%p1%d\sq,
	Cr=\E]112\007, Cs=\E]12;%p1%s\007,
	setrgbb=\E[48:2:%p1%d:%p2%d:%p3%dm,
	setrgbf=\E[38:2:%p1%d:%p2%d:%p3%dm,' >> fancy.terminfo

tic -x -o . fancy.terminfo
rm fancy.terminfo
mv f/fancy "$(dirname "$0")/fancy"
rm -r f
