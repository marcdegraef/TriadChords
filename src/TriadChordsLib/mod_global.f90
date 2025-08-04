! ###################################################################
! Copyright (c) 2008-2025 Marc De Graef/Carnegie Mellon University
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

module mod_global
  !! author: MDG
  !! date: 12/31/19 (original from EMsoftOO)
  !! incorporated in TriadChords on 08/04/25
  !!
  !! global constant definitions

use mod_kinds
use,intrinsic :: ISO_C_BINDING

IMPLICIT NONE
public

!> @note This module must be "use"d by every program, subroutine, and function.

!> standard string length for filenames
  integer(kind=irg),parameter           :: fnlen = 512
!DEC$ ATTRIBUTES DLLEXPORT :: fnlen

!> reserved IO unit identifiers for data (21-23)
  integer(kind=irg), parameter          :: dataunit = 21, dataunit2 = 22, dataunit3 = 23
!DEC$ ATTRIBUTES DLLEXPORT :: dataunit
!DEC$ ATTRIBUTES DLLEXPORT :: dataunit2
!DEC$ ATTRIBUTES DLLEXPORT :: dataunit3

  real(kind=dbl), parameter             :: cPi = 4.D0 * atan(1.D0)
  real(kind=dbl), parameter             :: dtor = cPi/180.D0
  real(kind=dbl), parameter             :: rtod = 180.D0/cPi
!DEC$ ATTRIBUTES DLLEXPORT :: dtor
!DEC$ ATTRIBUTES DLLEXPORT :: rtod

! source code version number
  character(8), parameter               :: scversion="3.0/2025"
!DEC$ ATTRIBUTES DLLEXPORT :: scversion

! author information
  character(13), parameter              :: authorname="Marc De Graef"
  character(26), parameter              :: authorlocation="Carnegie Mellon University"
!DEC$ ATTRIBUTES DLLEXPORT :: authorname
!DEC$ ATTRIBUTES DLLEXPORT :: authorlocation

end module mod_global
