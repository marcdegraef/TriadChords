! ###################################################################
! Copyright (c) 2013-2025, Marc De Graef Research Group/Carnegie Mellon University
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

module mod_TriadPlots
  !! author: MDG 
  !! version: 1.0 
  !! date: 08/06/25
  !!
  !! class definition for the TriadPlots program

use mod_kinds
use mod_global
use mod_triads
use mod_parameters

IMPLICIT NONE 

! namelist for the EMTriadPlots program
type, public :: TriadPlotsNameListType
  character(fnlen)    :: modality_file
  character(fnlen)    :: dissonance_file
  character(fnlen)    :: tension_file
  character(fnlen)    :: instability_file
  character(fnlen)    :: data_file
  integer(kind=irg)   :: scale
  integer(kind=irg)   :: interval_range
  real(kind=dbl)      :: delta
end type TriadPlotsNameListType

! class definition
type, public :: TriadPlots_T
private 
  character(fnlen)              :: nmldeffile = 'TriadPlots.nml'
  type(TriadPlotsNameListType)  :: nml 

contains
private 
  procedure, pass(self) :: readNameList_
  procedure, pass(self) :: getNameList_
  procedure, pass(self) :: TriadPlots_
  procedure, pass(self) :: setmodality_file_
  procedure, pass(self) :: getmodality_file_
  procedure, pass(self) :: setdissonance_file_
  procedure, pass(self) :: getdissonance_file_
  procedure, pass(self) :: settension_file_
  procedure, pass(self) :: gettension_file_
  procedure, pass(self) :: setinstability_file_
  procedure, pass(self) :: getinstability_file_
  procedure, pass(self) :: setdata_file_
  procedure, pass(self) :: getdata_file_
  procedure, pass(self) :: setscale_
  procedure, pass(self) :: getscale_
  procedure, pass(self) :: setinterval_range_
  procedure, pass(self) :: getinterval_range_
  procedure, pass(self) :: setdelta_
  procedure, pass(self) :: getdelta_

  generic, public :: getNameList => getNameList_
  generic, public :: readNameList => readNameList_
  generic, public :: TriadPlots => TriadPlots_
  generic, public :: setmodality_file => setmodality_file_
  generic, public :: getmodality_file => getmodality_file_
  generic, public :: setdissonance_file => setdissonance_file_
  generic, public :: getdissonance_file => getdissonance_file_
  generic, public :: settension_file => settension_file_
  generic, public :: gettension_file => gettension_file_
  generic, public :: setinstability_file => setinstability_file_
  generic, public :: getinstability_file => getinstability_file_
  generic, public :: setdata_file => setdata_file_
  generic, public :: getdata_file => getdata_file_
  generic, public :: setscale => setscale_
  generic, public :: getscale => getscale_
  generic, public :: setinterval_range => setinterval_range_
  generic, public :: getinterval_range => getinterval_range_
  generic, public :: setdelta => setdelta_
  generic, public :: getdelta => getdelta_

end type TriadPlots_T

! the constructor routine for this class 
interface TriadPlots_T
  module procedure TriadPlots_constructor
end interface TriadPlots_T

contains

!--------------------------------------------------------------------------
type(TriadPlots_T) function TriadPlots_constructor( nmlfile, copy_tpl ) result(TriadPlots)
!! author: MDG 
!! version: 1.0 
!! date: 08/06/25
!!
!! constructor for the TriadPlots_T Class; reads the name list 
!! or copies the name list template file 
 
use mod_io 
use mod_triads 

IMPLICIT NONE

character(fnlen), INTENT(INOUT), OPTIONAL   :: nmlfile 
character(fnlen), INTENT(INOUT), OPTIONAL   :: copy_tpl

type(Triad_T)                :: TT 
type(IO_T)                   :: Message 

character(fnlen)             :: filepath, outname = 'TriadPlots.template'
character(255)               :: line
integer(kind=irg)            :: ios

if (present(copy_tpl)) then ! copy the namelist template
  TT = Triad_T( no_alloc = .TRUE. )
  filepath = trim(TT%getsourcepath())//'/NameListTemplates/'//trim(outname)
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
  call TriadPlots%readNameList_(nmlfile)
end if

end function TriadPlots_constructor

!--------------------------------------------------------------------------
subroutine TriadPlots_destructor(self) 
!! author: MDG 
!! version: 1.0 
!! date: 08/06/25
!!
!! destructor for the TriadPlots_T Class
 
IMPLICIT NONE

type(TriadPlots_T), INTENT(INOUT)  :: self 

end subroutine TriadPlots_destructor

!--------------------------------------------------------------------------
subroutine readNameList_(self, nmlfile)
!DEC$ ATTRIBUTES DLLEXPORT :: readNameList_
!! author: MDG 
!! version: 1.0 
!! date: 08/06/25
!!
!! read the namelist from an nml file for the TriadPlots_T Class 

use mod_io 

IMPLICIT NONE 

class(TriadPlots_T), INTENT(INOUT)    :: self
character(fnlen),INTENT(IN)           :: nmlfile

type(IO_T)                            :: Message       
logical                               :: skipread = .FALSE.

character(fnlen)                      :: modality_file
character(fnlen)                      :: dissonance_file
character(fnlen)                      :: tension_file
character(fnlen)                      :: instability_file
character(fnlen)                      :: data_file
integer(kind=irg)                     :: scale
integer(kind=irg)                     :: interval_range
real(kind=dbl)                        :: delta

namelist / TriadPlots / modality_file, dissonance_file, tension_file, instability_file, data_file, &
                        scale, interval_range, delta 

modality_file = 'undefined'
! output file for dissonance plot (bmp or tiff)
dissonance_file = 'undefined'
! output file for tension plot (bmp or tiff)
tension_file = 'undefined'
! output file for instability plot (bmp or tiff)
instability_file = 'undefined'
! data output file 
data_file = 'undefined'
! number of pixels per interval
scale = 40
! range of plot along horizontal axis (in units of intervals)
interval_range = 24
! weight factor for tension in the instability calculation 
delta = 0.2D0

! read the name list from the config file
open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=TriadPlots)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(modality_file).eq.'undefined') then
  call Message%printError('TriadPlots_constructor:',' modality_file undefined in '//trim(nmlfile))
end if

if (trim(dissonance_file).eq.'undefined') then
  call Message%printError('TriadPlots_constructor:',' dissonance_file undefined in '//trim(nmlfile))
end if

if (trim(tension_file).eq.'undefined') then
  call Message%printError('TriadPlots_constructor:',' tension_file undefined in '//trim(nmlfile))
end if

if (trim(instability_file).eq.'undefined') then
  call Message%printError('TriadPlots_constructor:',' instability_file undefined in '//trim(nmlfile))
end if

if (trim(data_file).eq.'undefined') then
  call Message%printError('TriadPlots_constructor:',' data_file undefined in '//trim(nmlfile))
end if

self%nml%modality_file = modality_file
self%nml%dissonance_file = dissonance_file
self%nml%tension_file = tension_file
self%nml%instability_file = instability_file
self%nml%data_file = data_file
self%nml%scale = scale
self%nml%interval_range = interval_range
self%nml%delta = delta

end subroutine readNameList_

!--------------------------------------------------------------------------
function getNameList_(self) result(nml)
!DEC$ ATTRIBUTES DLLEXPORT :: getNameList_
!! author: MDG 
!! version: 1.0 
!! date: 08/06/25
!!
!! pass the namelist for the TriadPlots_T Class to the calling program

IMPLICIT NONE 

class(TriadPlots_T), INTENT(INOUT)          :: self
type(TriadPlotsNameListType)                :: nml

nml = self%nml

end function getNameList_

!--------------------------------------------------------------------------
subroutine setmodality_file_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setmodality_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set modality_file in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%modality_file = trim(inp)

end subroutine setmodality_file_

!--------------------------------------------------------------------------
function getmodality_file_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getmodality_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get modality_file from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%modality_file)

end function getmodality_file_

!--------------------------------------------------------------------------
subroutine setdissonance_file_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setdissonance_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set dissonance_file in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%dissonance_file = trim(inp)

end subroutine setdissonance_file_

!--------------------------------------------------------------------------
function getdissonance_file_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getdissonance_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get dissonance_file from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%dissonance_file)

end function getdissonance_file_

!--------------------------------------------------------------------------
subroutine settension_file_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: settension_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set tension_file in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%tension_file = trim(inp)

end subroutine settension_file_

!--------------------------------------------------------------------------
function gettension_file_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: gettension_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get tension_file from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%tension_file)

end function gettension_file_

!--------------------------------------------------------------------------
subroutine setinstability_file_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setinstability_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set instability_file in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%instability_file = trim(inp)

end subroutine setinstability_file_

!--------------------------------------------------------------------------
function getinstability_file_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getinstability_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get instability_file from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%instability_file)

end function getinstability_file_

!--------------------------------------------------------------------------
subroutine setdata_file_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setdata_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set data_file in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)       :: inp

self%nml%data_file = trim(inp)

end subroutine setdata_file_

!--------------------------------------------------------------------------
function getdata_file_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getdata_file_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get data_file from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
character(fnlen)                   :: out

out = trim(self%nml%data_file)

end function getdata_file_

!--------------------------------------------------------------------------
subroutine setscale_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setscale_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set scale in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%scale = inp

end subroutine setscale_

!--------------------------------------------------------------------------
function getscale_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getscale_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get scale from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%scale

end function getscale_

!--------------------------------------------------------------------------
subroutine setinterval_range_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setinterval_range_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set interval_range in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%interval_range = inp

end subroutine setinterval_range_

!--------------------------------------------------------------------------
function getinterval_range_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getinterval_range_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get interval_range from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%interval_range

end function getinterval_range_

!--------------------------------------------------------------------------
subroutine setdelta_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setdelta_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set delta in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
real(kind=dbl), INTENT(IN)       :: inp

self%nml%delta = inp

end subroutine setdelta_

!--------------------------------------------------------------------------
function getdelta_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getdelta_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get delta from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
real(kind=dbl)                   :: out

out = self%nml%delta

end function getdelta_

!--------------------------------------------------------------------------
subroutine TriadPlots_(self, progname, progdesc)
!DEC$ ATTRIBUTES DLLEXPORT :: TriadPlots_
!! author: MDG 
!! version: 1.0 
!! date: 08/06/25
!!
!! perform the computations

use mod_io
use mod_triads
use omp_lib
use ISO_C_BINDING
use, intrinsic :: iso_fortran_env

IMPLICIT NONE 

class(TriadPlots_T), INTENT(INOUT)        :: self
character(fnlen), INTENT(INOUT)           :: progname 
character(fnlen), INTENT(INOUT)           :: progdesc

type(Triad_T)                             :: Triad, myTT
type(IO_T)                                :: Message 

integer(kind=irg)                         :: interval_range, scl, xmax, ymax, i, j, il, iu, io_int(2), TID 
real(kind=dbl)                            :: f1, delta, fl, fu, fr, xx, io_real(2)
real(kind=dbl),allocatable                :: xline(:), yline(:), TT(:,:), DD(:,:), MM(:,:), Grid(:,:), Grid2(:,:)
character(1)                              :: bg


call Message%printMessage(' ')
call Message%printMessage(' Program Name       : '//trim(progname))
call Message%printMessage(' Program Descriptor : '//trim(progdesc))
call Message%printMessage(' ')

! set the local parameters and initialize the coordinate arrays for a hexagonal plot
interval_range = self%nml%interval_range
scl = self%nml%scale
delta = self%nml%delta

xmax = scl * interval_range + 1
ymax = nint( scl * interval_range * cos(cPi/6.D0)) + 1

allocate( xline(0:xmax-1), yline(0:ymax-1) )
xline = (/ (dble(i), i=0,xmax-1) /)
xline = (interval_range * xline/dble(xmax-1)-interval_range/2.0)
yline = (/ (dble(i), i=0,ymax-1) /)
yline = (interval_range * yline/dble(ymax-1)-interval_range/2.0)

! allocate the 2D arrays 
allocate( TT(0:xmax-1, 0:ymax-1), DD(0:xmax-1, 0:ymax-1), MM(0:xmax-1, 0:ymax-1) )
TT = 0.D0
DD = 0.D0
MM = 0.D0

Triad = Triad_T()
f1 = Triad%getf1()

fr = 1.D0/Triad%fits%freq2int

call Message%printMessage(' ')
call Message%printMessage(' Computing Dissonance, Tension, and Modality maps ... ')

! and compute the entries; this approach uses the frequencies explicitly
call OMP_SET_NUM_THREADS(OMP_GET_MAX_THREADS())
io_int(1) = OMP_GET_NUM_THREADS()
call Message%WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID, myTT, il, fl, iu, fu) 

TID = OMP_GET_THREAD_NUM()

! initialize a threadsafe copy of the Triad_T class by passing the important parameters
! to the myTT constructor
myTT = Triad_T( no_read = .TRUE., timbre = Triad%gettimbre(), spath = Triad%getsourcepath() )

!$OMP DO SCHEDULE(DYNAMIC)
do il = 0, ymax-1
  fl = f1 * 10**(yline(il)*fr)
  do iu = 0, xmax-1
    if (il.lt.ymax/2) then 
      if ( ((xline(0)-yline(il)*0.5D0).lt.xline(iu)).and.(xline(iu).lt.(xline(xmax-1)+yline(il)*0.5D0)) ) then 
        fu = fl * 10**((xline(iu)-0.5*(yline(il)))*fr)
        DD(xmax-1-iu, il) = myTT%totalquantity( f1, fl, fu, 'D' )
        TT(xmax-1-iu, il) = myTT%totalquantity( f1, fl, fu, 'T' )
        MM(xmax-1-iu, il) = myTT%totalquantity( f1, fl, fu, 'M' )
      end if
    else
      if ( ((xline(0)+yline(il)*0.5D0).lt.xline(iu)).and.(xline(iu).lt.(xline(xmax-1)-yline(il)*0.5D0)) ) then 
        fu = fl * 10**((xline(iu)-0.5*(yline(il)))*fr)
        DD(xmax-1-iu, il) = myTT%totalquantity( f1, fl, fu, 'D' )
        TT(xmax-1-iu, il) = myTT%totalquantity( f1, fl, fu, 'T' )
        MM(xmax-1-iu, il) = myTT%totalquantity( f1, fl, fu, 'M' )
      end if 
    end if 
  end do 
  if (mod(il,50).eq.0) then 
    io_int = (/ il, ymax /)
    call Message%WriteValue(' completed ',io_int, 2, frm="(I5,' of ',I5,' lines')")
  end if
end do
!$OMP END DO
!$OMP END PARALLEL

call Message%printMessage('  --->  Done ')
call Message%printMessage(' ')
io_real = (/ minval(DD), maxval(DD) /)
call Message%WriteValue(' range dissonnance : ', io_real, 2) 
io_real = (/ minval(TT), maxval(TT) /)
call Message%WriteValue(' range tension     : ', io_real, 2) 
io_real = (/ minval(MM), maxval(MM) /)
call Message%WriteValue(' range modality    : ', io_real, 2) 

! next we draw the haxagonal grid lines on all maps
call Message%printMessage(' ')
call Message%printMessage(' Creating hexagonal coordinate grid ')
call Message%printMessage(' ')

allocate(Grid(0:xmax-1,0:ymax-1), Grid2(0:xmax-1,0:ymax-1))
Grid  = 1.D0
Grid2 = 1.D0
bg = 'w'
call Triad%makeGrid(xmax, ymax, Grid2, scl, interval_range, bg)

! binarize the Grid to 1 and 0  and flip it vertically 
do i=0,xmax-1
  do j=0,ymax-1
    if (Grid2(i,j).eq.-100.D0) then 
      Grid(i,ymax-1-j) = -100.D0
    else
      if (Grid2(i,j).gt.0.5D0) then 
        Grid(i,ymax-1-j) = 1.D0
      else
        Grid(i,ymax-1-j) = 0.D0 
      end if
    end if
  end do 
end do 

deallocate( Grid2 )

! open(dataunit, file='test.data', status='unknown', form='unformatted')
! write (dataunit) real(DD)
! write (dataunit) real(TT)
! write (dataunit) real(MM)
! write (dataunit) real(Grid)
! close(dataunit,status='keep')

! next we generate color tif or bmp files ... 
call Message%printMessage(' Generating color maps and overlapping coordinate grid ')
call Message%printMessage(' ')

call Triad%saveColorMap( xmax, ymax, DD, Grid, self%nml%dissonance_file )
call Triad%saveColorMap( xmax, ymax, TT, Grid, self%nml%tension_file )
call Triad%saveColorMap( xmax, ymax, MM, Grid, self%nml%modality_file )
DD = DD + self%nml%delta * TT
call Triad%saveColorMap( xmax, ymax, DD, Grid, self%nml%instability_file )

deallocate( TT, MM, DD, Grid )

end subroutine TriadPlots_

end module mod_TriadPlots