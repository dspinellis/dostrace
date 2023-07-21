Trace: A tool for logging MS-DOS system calls

_Trace_ is a system utility that produces a listing of the MS-DOS requests
made by a process.  It is a useful debugging tool that can be used on any
executable program.
_Trace_ can also be used to provide a better
understanding of the internal workings of many programs.  A utility
with the same name and similar functionality was provided with SunOS,
and is currently available on modern versions of Unix and similar systems
with names such as _strace_, _ktrace_, and _dtrace_.

The tool is currently [distributed with](https://github.com/open-watcom/open-watcom-v2/tree/master/contrib/dostrace) the [Open Watcom v2 Project](https://github.com/open-watcom/open-watcom-v2).

More information about the tool can be found in its
[manual page](https://dspinellis.github.io/manview/?src=https%3A%2F%2Fraw.githubusercontent.com%2Fdspinellis%2Fdostrace%2Fmain%2Ftrace.1&name=trace(1)&link=https%3A%2F%2Fgithub.com%2Fdspinellis%2Fdostrace) and in the following publication.

Diomidis Spinellis. Trace: A tool for logging operating system call transactions. Operating Systems Review, 28(4):56–63, October 1994. [doi:10.1145/191525.191540](https://dx.doi.org/10.1145/191525.191540)
