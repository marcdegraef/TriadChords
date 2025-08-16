# TriadChords
Basic Fortran 2018 library to compute dissonance, tension, and modality for individual triad chords as well as progressions of two or three triad chords.  

# Funding
This package is the result of unfunded research that was prompted by a series of papers by Norman Cook and co-workers, in particular "The Psychophysics of Harmony Perception: Harmony is a Three-Tone Phenomenon", *Empirical Musicology Review* Vol 1, pp. 106-126 (2006). An informal collaboration ensued over a couple of years (2006-2010), leading to a joint [poster presentation](https://kernie.materials.cmu.edu/~degraef/www/SMPC-2009-poster.pdf) at the 2009 conference of the Society for Music Perception and Cognition (Indianapolis, 8/3-6/09).  The TriadChords package contains all the source code needed to reproduce the majority of the figures in this poster.

# Requirements
The code was originally developed in the [Interactive Data Language](https://www.nv5geospatialsoftware.com/Products/IDL?__hstc=258790805.7ebae6abbe6f76e26d82791a6ff2298a.1754648070713.1754648070713.1754648070713.1&__hssc=258790805.4.1754648070713&__hsfp=364643402) on the Mac OS X platform, and was rewritten as object-oriented Fortran 2018 code contained in this repository in August of 2025.  The code has been compiled successfully on Mac OS X (Sonoma 14.3) using the gfortran compiler (gcc version 14.2.0 on aarch64-apple-darwin23, installed with Homebrew) as well as Ubuntu 22.04.5 LTS using the f95 compiler with devtoolset-8.  On both platforms, installation and building should go as follows (in some top folder):

```fortran
git clone git@github.com:marcdegraef/TriadChords.git
mkdir TriadsBuild
cd TriadsBuild
cmake ../TriadChords
make -j
```
This will produce (among other things) a TriadsBuild/Bin folder containing all executables.  Use the normal additions to the \$PATH environment variable to provide easy access to the executables from any location on the file system.

This package has not yet been built and tested on Windows (as of August 15, 2025); anyone who'd like to do so is encouraged to add instructions to this README.md file via a pull request.

Check the wiki pages for this package for information on how to configure things using the **TriadConfig.txt** file.

# License
BSD 3-Clause License

Copyright (c) 2025, Marc De Graef/Carnegie Mellon University

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, this
   list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

3. Neither the name of the copyright holder nor the names of its
   contributors may be used to endorse or promote products derived from
   this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.




