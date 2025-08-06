! ###################################################################
! Copyright (c) 2008-2025, Marc De Graef/Carnegie Mellon University
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without modification, are
! permitted provided that the following conditions are met:
!
!     - Redistributions of source code must retain the above copyright notice, this list
!        of conditions and the following disclaimer.
!     - Redistributions in binary form must reproduce the above copyright notice, this
!        list of conditions and the following disclaimer in the documentation and/or
!        other materials provided with the distribution.
!     - Neither the names of Marc De Graef, Carnegie Mellon University nor the names
!        of its contributors may be used to endorse or promote products derived from
!        this software without specific prior written permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
! DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
! SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
! CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
! OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE
! USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
! ###################################################################

module mod_parameters

use mod_kinds 

IMPLICIT NONE
  public

! first a bunch of fitting parameters used for dissonance, tension, and modality functions
type fit_parameters
  real(kind=dbl)            :: alpha_tension = 2.777777778D0     ! used for interval tension computation
  real(kind=dbl)            :: b1 = 0.8D0                        ! used for dissonance computation
  real(kind=dbl)            :: b2 = 1.6D0                        ! used for dissonance computation
  real(kind=dbl)            :: b3 = 4.0D0                        ! used for dissonance computation
  real(kind=dbl)            :: gam = 1.25D0                      ! used for dissonance computation
  real(kind=dbl)            :: eps = 1.28402543D0                ! used for modality computation
  real(kind=dbl)            :: delta = 0.207D0                   ! weight factor for instability
  real(kind=dbl)            :: freq2int = 39.8631D0              ! conversion parameter from frequency to interval
end type fit_parameters

type timbre_descriptor
  real(kind=dbl)            :: base                              ! exponent base for timbre of the type base^n
  integer(kind=irg)         :: timbre_type                       ! 1 for base^n; 2 for 1/n; ...
  integer(kind=irg)         :: num_partials                      ! number of upper partials to include
end type timbre_descriptor

type scale
  character(2)              :: notes(12) = (/ ' C','C#',' D','D#',' E',' F','F#',' G','G#',' A','A#',' B' /)
end type scale 

! basic triad chords and their inversions in interval form
type triad_chords
  real(kind=dbl)            :: major(9) =       dble( (/ 0, 4, 7,  4, 7, 12,  7, 12, 16 /) )
  real(kind=dbl)            :: minor(9) =       dble( (/ 0, 3, 7,  3, 7, 12,  7, 12, 15 /) )
  real(kind=dbl)            :: augmented(9) =   dble( (/ 0, 4, 8,  4, 8, 12,  8, 12, 16 /) )
  real(kind=dbl)            :: diminished(9) =  dble( (/ 0, 3, 6,  3, 6, 12,  6, 12, 15 /) )
  real(kind=dbl)            :: suspended(9) =   dble( (/ 0, 5, 7,  5, 7, 12,  7, 12, 17 /) )
end type triad_chords

end module mod_parameters