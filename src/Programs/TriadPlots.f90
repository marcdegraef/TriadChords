! ###################################################################
! Copyright (c) 2016-2025, Marc De Graef Research Group/Carnegie Mellon University
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

program TriadPlots
  !! author: MDG
  !! date: 08/05/25
  !!
  !! Program to plot triad chord dissonance, tension, modality and instability maps

use mod_kinds
use mod_global
use mod_TriadPlots
use mod_io

IMPLICIT NONE

character(fnlen)                :: progname = 'TriadPlots.f90'
character(fnlen)                :: progdesc = 'Program to plot triad chord dissonance, tension, modality and instability maps'

type(Triadplots_T)              :: TP
type(IO_T)                      :: Message

integer(kind=irg)               :: numarg       ! number of command line arguments
integer(kind=irg)               :: iargc        ! external function for command line
integer(kind=irg)               :: io_int(1)
character(fnlen)                :: arg          ! to be read from the command line
character(fnlen)                :: cptl

! handle any command line argument (only -t is recognized)
numarg = command_argument_count()
if (numarg.gt.0) then
  if (numarg.gt.1) numarg = 1  ! only -t is possible
  io_int(1) = numarg
  call Message%WriteValue(' Number of command line arguments detected: ',io_int,1)
  call get_command_argument(1,arg)
  if (trim(arg).eq.'-t') then
    cptl = 'TriadPlots.template'
    TP = TriadPlots_T( copy_tpl = cptl )
  else
    TP = TriadPlots_T( nmlfile = arg )
  end if
else
 cptl = 'Triadplots.nml'
 TP = TriadPlots_T( nmlfile = cptl )
end if 

! perform the computations
call TP%TriadPlots( progname, progdesc )

end program TriadPlots
