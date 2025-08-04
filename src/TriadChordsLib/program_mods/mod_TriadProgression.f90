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
! ###################################################################! -*-Fortran-*-

module mod_TriadProgression

use mod_kinds
use mod_global
use mod_io
use mod_triads

IMPLICIT NONE
  private

! namelist for the EMTriadProgression program
type, public :: TriadProgressionNameListType
  integer(kind=irg)             :: triad1(3)
  integer(kind=irg)             :: triad2(3)
  integer(kind=irg)             :: triad3(3)
  integer(kind=irg)             :: numint
  character(fnlen)              :: outname
end type TriadProgressionNameListType

! class definition
type, public :: TriadProgression_T
private 
  character(fnlen)                      :: nmldeffile = 'TriadProgression.nml'
  type(TriadProgressionNameListType)    :: nml 

contains
private 
  procedure, pass(self) :: readNameList_
  procedure, pass(self) :: getNameList_
  procedure, pass(self) :: TriadProgression_

  generic, public :: getNameList => getNameList_
  generic, public :: readNameList => readNameList_
  generic, public :: TriadProgression => TriadProgression_

end type TriadProgression_T

! the constructor routine for this class 
interface TriadProgression_T
  module procedure TriadProgression_constructor
end interface TriadProgression_T

contains

!--------------------------------------------------------------------------
type(TriadProgression_T) function TriadProgression_constructor( nmlfile, copy_tpl ) result(TriadProgression)
!! author: MDG 
!! version: 1.0 
!! date: 08/04/25
!!
!! constructor for the TriadProgression_T Class; reads the name list
!! or copies the name list template file 

use mod_io 
use mod_triads

IMPLICIT NONE

character(fnlen), INTENT(INOUT), OPTIONAL   :: nmlfile 
character(fnlen), INTENT(INOUT), OPTIONAL   :: copy_tpl

type(Triad_T)                :: TT 
type(IO_T)                   :: Message 

character(fnlen)             :: filepath, outname = 'TriadProgression.template'
character(255)               :: line
integer(kind=irg)            :: ios

if (present(copy_tpl)) then ! copy the namelist template
  TT = Triad_T( no_alloc = .TRUE. )
  filepath = trim(TT%getsourcepath())//'NameListTemplates/'//trim(outname)
  open(UNIT=dataunit,FILE=trim(filepath), STATUS='old', FORM='formatted',ACCESS='sequential')
  open(UNIT=dataunit2,FILE=trim(outname), STATUS='unknown', FORM='formatted',ACCESS='sequential')
  do
    read(dataunit,'(A)',iostat=ios) line
    if (ios.ne.0) then
      exit
    end if
    write(dataunit2,'(A)') trim(line)
   end do
   close(UNIT=dataunit, STATUS='keep')
   close(UNIT=dataunit2, STATUS='keep')
   call Message%printMessage('  -> created template file '//trim(outname), frm = "(A)" )
   STOP '  -> please rename and edit the name list file'
else
  call TriadProgression%readNameList(nmlfile)
end if

end function TriadProgression_constructor

!--------------------------------------------------------------------------
subroutine TriadProgression_destructor(self) 
!! author: MDG 
!! version: 1.0 
!! date: 08/04/25
!!
!! destructor for the TriadProgression_T Class
 
IMPLICIT NONE

type(TriadProgression_T), INTENT(INOUT)  :: self 

call reportDestructor('TriadProgression_T')

end subroutine TriadProgression_destructor

!--------------------------------------------------------------------------
subroutine readNameList_(self, nmlfile)
!DEC$ ATTRIBUTES DLLEXPORT :: readNameList_
!! author: MDG 
!! version: 1.0 
!! date: 08/04/25
!!
!! read the namelist from an nml file for the TriadProgression_T Class 

use mod_io 

IMPLICIT NONE 

class(TriadProgression_T), INTENT(INOUT)        :: self
character(fnlen),INTENT(IN)                     :: nmlfile

type(IO_T)                                      :: Message       

integer(kind=irg) :: triad1(3)
integer(kind=irg) :: triad2(3)
integer(kind=irg) :: triad3(3)
integer(kind=irg) :: numint
character(fnlen)  :: outname

namelist /TriadProgression/ triad1, triad2, triad3, numint, outname

triad1 = (/ 0, 4, 7 /)
triad2 = (/ 0, 3, 7 /)
triad3 = (/ 0, 4, 7 /)
numint = 200
outname = 'undefined'

! read the name list from the config file
open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=TriadProgression)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(outname).eq.'undefined') then
  call Message%printError('TriadProgression_constructor:',' outname undefined in '//trim(nmlfile))
end if

self%nml%triad1 = triad1
self%nml%triad2 = triad2
self%nml%triad3 = triad3
self%nml%numint = numint
self%nml%outname = trim(outname)

write (*,*) ' outname = ', trim(self%nml%outname)

end subroutine readNameList_

!--------------------------------------------------------------------------
function getNameList_(self) result(nml)
!DEC$ ATTRIBUTES DLLEXPORT :: getNameList_
!! author: MDG 
!! version: 1.0 
!! date: 08/04/25
!!
!! pass the namelist for the TriadProgression_T Class to the calling program

IMPLICIT NONE 

class(TriadProgression_T), INTENT(INOUT)          :: self
type(TriadProgressionNameListType)                :: nml

nml = self%nml

end function getNameList_

!--------------------------------------------------------------------------
subroutine TriadProgression_(self, progname, progdesc)
!DEC$ ATTRIBUTES DLLEXPORT :: TriadProgression_
!! author: MDG 
!! version: 1.0 
!! date: 08/04/25
!!
!! perform the computations

use mod_io 
use mod_triads

IMPLICIT NONE 

class(TriadProgression_T), INTENT(INOUT)        :: self
character(fnlen), INTENT(INOUT)                 :: progname 
character(fnlen), INTENT(INOUT)                 :: progdesc

type(Triad_T)                                   :: TT
type(IO_T)                                      :: Message

integer(kind=sgl)                               :: i,j,k,l,iv11,iv12,iv21,iv22,iv31,iv32,dims,istat 
real(kind=dbl)                                  :: set1(3), set2(3), set3(3), fr2(3), fr3(3), tt12
real(kind=dbl)                                  :: tt13, tt23, dd12, dd13, dd23, r
real(kind=dbl),allocatable                      :: xx(:),dd(:,:),ttt(:,:)
character(len=7)                                :: id


call Message%printMessage(' ')
call Message%printMessage(' Program Name       : '//trim(progname))
call Message%printMessage(' Program Descriptor : '//trim(progdesc))
call Message%printMessage(' ')

! instantiate the Triad class
TT = Triad_T()

! define the simulation parameters and allocate arrays
dims = self%nml%numint
allocate(xx(dims),dd(dims,dims),ttt(dims,dims),stat=istat)
do i=1,dims
        xx(i) = 24.D0*dble(i)/dble(dims) - 12.D0
end do

set1 = dble(self%nml%triad1)
set2 = dble(self%nml%triad2)
set3 = dble(self%nml%triad3)

! start major computation loop
do i=1,dims
        fr2 = xx(i) + set2
        tt12 =        TT%interval_totaltension(set1(1),fr2(1),fr2(2))
        tt12 = tt12 + TT%interval_totaltension(set1(1),fr2(1),fr2(3))
        tt12 = tt12 + TT%interval_totaltension(set1(1),fr2(2),fr2(3))
        tt12 = tt12 + TT%interval_totaltension(set1(2),fr2(1),fr2(2))
        tt12 = tt12 + TT%interval_totaltension(set1(2),fr2(1),fr2(3))
        tt12 = tt12 + TT%interval_totaltension(set1(2),fr2(2),fr2(3))
        tt12 = tt12 + TT%interval_totaltension(set1(3),fr2(1),fr2(2))
        tt12 = tt12 + TT%interval_totaltension(set1(3),fr2(1),fr2(3))
        tt12 = tt12 + TT%interval_totaltension(set1(3),fr2(2),fr2(3))
        
        tt12 = tt12 + TT%interval_totaltension(fr2(1),set1(1),set1(2))
        tt12 = tt12 + TT%interval_totaltension(fr2(1),set1(1),set1(3))
        tt12 = tt12 + TT%interval_totaltension(fr2(1),set1(2),set1(3))
        tt12 = tt12 + TT%interval_totaltension(fr2(2),set1(1),set1(2))
        tt12 = tt12 + TT%interval_totaltension(fr2(2),set1(1),set1(3))
        tt12 = tt12 + TT%interval_totaltension(fr2(2),set1(2),set1(3))
        tt12 = tt12 + TT%interval_totaltension(fr2(3),set1(1),set1(2))
        tt12 = tt12 + TT%interval_totaltension(fr2(3),set1(1),set1(3))
        tt12 = tt12 + TT%interval_totaltension(fr2(3),set1(2),set1(3))
        tt12 = tt12/18.D0

        dd12 = 0.D0
        do k=1,3
                do l=1,3
                        dd12 = dd12 + TT%interval_totaldissonance(dabs(set1(k)-fr2(l)))
                end do
        end do
        dd12 = dd12/9.D0

        do j=1,dims
                fr3 = xx(j) + set3
                tt23 =        TT%interval_totaltension(fr2(1),fr3(1),fr3(2))
                tt23 = tt23 + TT%interval_totaltension(fr2(1),fr3(1),fr3(3))
                tt23 = tt23 + TT%interval_totaltension(fr2(1),fr3(2),fr3(3))
                tt23 = tt23 + TT%interval_totaltension(fr2(2),fr3(1),fr3(2))
                tt23 = tt23 + TT%interval_totaltension(fr2(2),fr3(1),fr3(3))
                tt23 = tt23 + TT%interval_totaltension(fr2(2),fr3(2),fr3(3))
                tt23 = tt23 + TT%interval_totaltension(fr2(3),fr3(1),fr3(2))
                tt23 = tt23 + TT%interval_totaltension(fr2(3),fr3(1),fr3(3))
                tt23 = tt23 + TT%interval_totaltension(fr2(3),fr3(2),fr3(3))
                
                tt23 = tt23 + TT%interval_totaltension(fr3(1),fr2(1),fr2(2))
                tt23 = tt23 + TT%interval_totaltension(fr3(1),fr2(1),fr2(3))
                tt23 = tt23 + TT%interval_totaltension(fr3(1),fr2(2),fr2(3))
                tt23 = tt23 + TT%interval_totaltension(fr3(2),fr2(1),fr2(2))
                tt23 = tt23 + TT%interval_totaltension(fr3(2),fr2(1),fr2(3))
                tt23 = tt23 + TT%interval_totaltension(fr3(2),fr2(2),fr2(3))
                tt23 = tt23 + TT%interval_totaltension(fr3(3),fr2(1),fr2(2))
                tt23 = tt23 + TT%interval_totaltension(fr3(3),fr2(1),fr2(3))
                tt23 = tt23 + TT%interval_totaltension(fr3(3),fr2(2),fr2(3))
                tt23 = tt23/18.D0

                dd23 = 0.D0
                do k=1,3
                        do l=1,3
                                dd23 = dd23 + TT%interval_totaldissonance(dabs(fr2(k)-fr3(l)))
                        end do
                end do
                dd23 = dd23/9.D0

                tt13 =        TT%interval_totaltension(fr3(1),set1(1),set1(2))
                tt13 = tt13 + TT%interval_totaltension(fr3(1),set1(1),set1(3))
                tt13 = tt13 + TT%interval_totaltension(fr3(1),set1(2),set1(3))
                tt13 = tt13 + TT%interval_totaltension(fr3(2),set1(1),set1(2))
                tt13 = tt13 + TT%interval_totaltension(fr3(2),set1(1),set1(3))
                tt13 = tt13 + TT%interval_totaltension(fr3(2),set1(2),set1(3))
                tt13 = tt13 + TT%interval_totaltension(fr3(3),set1(1),set1(2))
                tt13 = tt13 + TT%interval_totaltension(fr3(3),set1(1),set1(3))
                tt13 = tt13 + TT%interval_totaltension(fr3(3),set1(2),set1(3))
                
                tt13 = tt13 + TT%interval_totaltension(set1(1),fr3(1),fr3(2))
                tt13 = tt13 + TT%interval_totaltension(set1(1),fr3(1),fr3(3))
                tt13 = tt13 + TT%interval_totaltension(set1(1),fr3(2),fr3(3))
                tt13 = tt13 + TT%interval_totaltension(set1(2),fr3(1),fr3(2))
                tt13 = tt13 + TT%interval_totaltension(set1(2),fr3(1),fr3(3))
                tt13 = tt13 + TT%interval_totaltension(set1(2),fr3(2),fr3(3))
                tt13 = tt13 + TT%interval_totaltension(set1(3),fr3(1),fr3(2))
                tt13 = tt13 + TT%interval_totaltension(set1(3),fr3(1),fr3(3))
                tt13 = tt13 + TT%interval_totaltension(set1(3),fr3(2),fr3(3))
                tt13 = tt13/18.D0
        
                dd13 = 0.D0
                do k=1,3
                        do l=1,3
                                dd13 = dd13 + TT%interval_totaldissonance(dabs(fr3(k)-set1(l)))
                        end do
                end do
                dd13 = dd13/9.D0

                dd(i,j) = dd12+dd23+dd13
                ttt(i,j) = tt12+tt23+tt13
        end do
end do

! and save the resulting arrays in a file that can be read by IDL
open(unit=dataunit,file=trim(self%nml%outname),status='unknown',action='write',form='unformatted')
write (dataunit) dd
write (dataunit) ttt
close(unit=dataunit,status='keep')

end subroutine TriadProgression_

end module mod_TriadProgression
