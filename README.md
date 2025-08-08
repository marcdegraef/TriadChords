# TriadChords
Basic Fortran 2018 library to compute dissonance, tension, and modaility for individual triad chords as well as progrssions of two or three triad chords.  

# Funding
This package is the result of unfunded research that was prompted by a series of papers by Norman Cook and co-workers, in particular "The Psychophysics of Harmony Perception: Harmony is a Three-Tone Phenomenon", *Empricial Musicology Review* Vol 1, pp. 106-126 (2006). An informal collaboration ensued over a couple of years (2006-2010), leading to a joint [poster presentation](https://kernie.materials.cmu.edu/~degraef/www/SMPC-2009-poster.pdf) at the 2009 conference of the Society for Music Perception and Cognition (Indianapolis, 8/3-6/09).  The TriadChords package contains all the source code needed to reproduce the majority of the figures in this poster.

# Requirements
The code was originally developed in the [Interactive Data Language](https://www.nv5geospatialsoftware.com/Products/IDL?__hstc=258790805.7ebae6abbe6f76e26d82791a6ff2298a.1754648070713.1754648070713.1754648070713.1&__hssc=258790805.4.1754648070713&__hsfp=364643402) on the Mac OS X platform, and was rewritten as object-oriented Fortran 2018 code contained in this repository in August of 2025.  The code has been successfully compiled on Mac OS X (Sonoma 14.3) using the gfortran compiler (gcc version 14.2.0 on aarch64-apple-darwin23, installed with Homebrew) as well as Ubuntu 22.04.5 LTS using the f95 compiler with devtoolset-8.  On both platforms, installation and building should go as follows (in some top folder):

```fortran
git clone git@github.com:marcdegraef/TriadChords.git
mkdir TriadsBuild
cd TriadsBuild
cmake ../TriadChords
make -j
```
This will produce (among other things) a TriadsBuild/Bin folder containing all executables.  Use the normal additions to the $PATH environment variable to provide easy access to the executables from any location on the file system.

This package has not yet been built and tested on Windows; anyone who'd like to do so is encouraged to add instructions to this README.md file via a pull request.

# Initial Configuration
All the executables in this packages use a handfuil of configuration parameters that are listed in a **TriadsConfig.txt** text file located in the folder inside which the programas are executed (good practice tip: this should not be the folder in which the package was built). To create this file, simply execute one of the programs with the -t command line option; if the configuration file does not exists, the program will create it with the following default content (formatted as a fortran name list file):

```fortran
 &TriadConfig
! number of upper partials to include
 num_partials = 6,
! timbre type (1=b^n; 2=1/n)
 timbre_type = 1,
! base factor for timbre type 1
 base = 0.88D0,
! full path to TriadChords source folder
 sourcepath = 'undefined',
 /                    
```
Set *num_partials* to the number of upper partials (harmonics) to be considered in all simulations.  Currently there are two timbre types available, an exponential type (*timbre_type*=1) of the form $base^n$ where $n$ is the number of the upper partial and *base* is a number between 0 and 1; the default of 0.88 can be thought of as representing a simple guitar string frequency spectrum.  A second timbre type (2) uses a *1/n* sequence of spectral amplitudes for the upper partials.  Finally, the *sourcepath* parameter should be set to the full path of the source code folder installed with the git clone command.  This is needed so that the proper name list template files will be found by the various executables.

Once the **TriadsConfig.txt** file has been created (and edited as needed), mosts programs should first be executed with the -t option to generate a copy of the name list template file. This file should be renamed with the .nml extension and edited as needed.

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




