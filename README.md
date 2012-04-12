Tree.hs
=======

Lightweight Haskell port of Unix `tree` command

Usage
=====
```
tree [-L level] [-adU] [--noreport] [--dirsfirst] [--version] [--help] [<directory list>]
  -L level               Descend only level directories deep.
  -d                     List directories only.
  -a                     All files are listed.
  -U                     Leave files unsorted.
            --noreport   Turn off file/directory count at end of tree listing.
            --dirsfirst  List directories before files (-U disables).
            --version    Print version and exit..
  -h, -?    --help       Prints this help message.
```