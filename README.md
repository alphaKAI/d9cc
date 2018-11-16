# d9cc - A Tiny C Compiler in Dlang.

d9cc is ported from [9cc](https://github.com/rui314/9cc).  
Original 9cc is self-hosted tiny C compiler, then I ported 9cc into d9cc with Dlang for study.  

## Requirements

- DMD >= v2.081 : D Compiler
- dub : D's package manager and build manager

Please install DMD and dub before building d9cc.  

## Build

```zsh
$ git clone https://github.com/alphaKAI/d9cc
$ cd d9cc
$ dub build --build=release
```

## License
d9cc is released under the MIT License.  
Please see LICENSE for details.  
Copyright (C) 2018 Akihiro Shoji