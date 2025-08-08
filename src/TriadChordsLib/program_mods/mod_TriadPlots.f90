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
  integer(kind=irg)   :: demag
  real(kind=dbl)      :: f1
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
  procedure, pass(self) :: makeGrid_
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
  procedure, pass(self) :: setdemag_
  procedure, pass(self) :: getdemag_
  procedure, pass(self) :: setf1_
  procedure, pass(self) :: getf1_
  procedure, pass(self) :: setdelta_
  procedure, pass(self) :: getdelta_

  generic, public :: getNameList => getNameList_
  generic, public :: readNameList => readNameList_
  generic, public :: makeGrid => makeGrid_
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
  generic, public :: setdemag => setdemag_
  generic, public :: getdemag => getdemag_
  generic, public :: setf1 => setf1_
  generic, public :: getf1 => getf1_
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
integer(kind=irg)                     :: demag
real(kind=dbl)                        :: f1
real(kind=dbl)                        :: delta

namelist / TriadPlots / modality_file, dissonance_file, tension_file, instability_file, data_file, &
                        scale, interval_range, demag, f1, delta 

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
! coordinate demag factor (if interval_range=24, then actual coordinate range
! will be from -12 to +12 if demag=1, -24 to +24 if demag=2 etc.)
demag = 2
! fundamental frequency (default A4)
f1 = 440.D0
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
self%nml%demag = demag
self%nml%f1 = f1
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
subroutine setdemag_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setdemag_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set demag in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%nml%demag = inp

end subroutine setdemag_

!--------------------------------------------------------------------------
function getdemag_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getdemag_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get demag from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%nml%demag

end function getdemag_

!--------------------------------------------------------------------------
subroutine setf1_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setf1_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set f1 in the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
real(kind=dbl), INTENT(IN)       :: inp

self%nml%f1 = inp

end subroutine setf1_

!--------------------------------------------------------------------------
function getf1_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getf1_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get f1 from the TriadPlots_T class

IMPLICIT NONE

class(TriadPlots_T), INTENT(INOUT)     :: self
real(kind=dbl)                   :: out

out = self%nml%f1

end function getf1_

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

use mod_image
use mod_io
use mod_triads
use ISO_C_BINDING
use, intrinsic :: iso_fortran_env

IMPLICIT NONE 

class(TriadPlots_T), INTENT(INOUT)        :: self
character(fnlen), INTENT(INOUT)           :: progname 
character(fnlen), INTENT(INOUT)           :: progdesc

type(Triad_T)                             :: Triad
type(IO_T)                                :: Message 

integer(int8),allocatable                 :: colormap(:,:,:), rect(:,:,:)
integer(kind=irg)                         :: interval_range, scl, demag, xmax, ymax, i, j, il, iu 
integer(kind=irg)                         :: iDD, cr
integer(int8)                             :: R(0:255), G(0:255), B(0:255) 
real(kind=dbl)                            :: f1, delta, fl, fu, fr, xx, mi, ma, io_real(2), crad, cradsq, cx, cy
real(kind=dbl),allocatable                :: xline(:), yline(:), TT(:,:), DD(:,:), MM(:,:), Grid(:,:), Grid2(:,:)
character(fnlen)                          :: TIFF_filename 

! declare variables for use in object oriented image module
integer                                   :: iostat, io_int(2)
character(len=128)                        :: iomsg
logical                                   :: isInteger, OPC, PUC
type(image_t)                             :: im
integer                                   :: dim2(2), Pm
integer(c_int32_t)                        :: result


call Message%printMessage(' ')
call Message%printMessage(' Program Name       : '//trim(progname))
call Message%printMessage(' Program Descriptor : '//trim(progdesc))
call Message%printMessage(' ')

! set the local parameters and initialize the coordinate arrays for a hexagonal plot
interval_range = self%nml%interval_range
scl = self%nml%scale
demag = self%nml%demag
f1 = self%nml%f1
delta = self%nml%delta

xmax = scl * interval_range + 1
ymax = nint( scl * interval_range * cos(cPi/6.D0)) + 1

allocate( xline(0:xmax-1), yline(0:ymax-1) )
xline = (/ (dble(i), i=0,xmax-1) /)
xline = demag * (interval_range * xline/dble(xmax-1)-interval_range/2.0)
yline = (/ (dble(i), i=0,ymax-1) /)
yline = demag * (interval_range * yline/dble(ymax-1)-interval_range/2.0)

! allocate the 2D arrays 
allocate( TT(0:xmax-1, 0:ymax-1), DD(0:xmax-1, 0:ymax-1), MM(0:xmax-1, 0:ymax-1) )
TT = 0.D0
DD = 0.D0
MM = 0.D0

Triad = Triad_T()

fr = 1.D0/Triad%fits%freq2int

call Message%printMessage(' ')
call Message%printMessage(' Computing Dissonance, Tension, and Modality maps ... ')

! and compute the entries; this approach uses the frequencies explicitly
do il = 0, ymax-1
  fl = f1 * 10**(yline(il)*fr)
  do iu = 0, xmax-1
    if (il.lt.ymax/2) then 
      if ( ((xline(0)-yline(il)*0.5D0).lt.xline(iu)).and.(xline(iu).lt.(xline(xmax-1)+yline(il)*0.5D0)) ) then 
        fu = fl * 10**((xline(iu)-0.5*(yline(il)))*fr)
        DD(iu, il) = Triad%totalquantity( f1, fl, fu, 'D' )
        TT(iu, il) = Triad%totalquantity( f1, fl, fu, 'T' )
        MM(iu, il) = Triad%totalquantity( f1, fl, fu, 'M' )
      end if
    else
      if ( ((xline(0)+yline(il)*0.5D0).lt.xline(iu)).and.(xline(iu).lt.(xline(xmax-1)-yline(il)*0.5D0)) ) then 
        fu = fl * 10**((xline(iu)-0.5*(yline(il)))*fr)
        DD(iu, il) = Triad%totalquantity( f1, fl, fu, 'D' )
        TT(iu, il) = Triad%totalquantity( f1, fl, fu, 'T' )
        MM(iu, il) = Triad%totalquantity( f1, fl, fu, 'M' )
      end if 
    end if 
  end do 
end do

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
call self%makeGrid_(xmax, ymax, Grid2, scl, interval_range)

! binarize the Grid to 1 and 0  and flip it vertically 
do i=0,xmax-1
  do j=0,ymax-1
    if (Grid2(i,j).gt.0.5D0) then 
      Grid(i,ymax-1-j) = 1.D0
    else
      Grid(i,ymax-1-j) = 0.D0 
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
allocate( colormap(3, xmax, ymax) )

call Triad%getclrs(R,G,B)

! this is a test image to make sure the color table is correctly reproduced
! it should look identical to color table 33 in IDL 9.0

! allocate( rect(3, 256, 256) )

! do i=1,256
!   rect(1,i,1:256) = R(i-1)
!   rect(2,i,1:256) = G(i-1)
!   rect(3,i,1:256) = B(i-1)
! end do 
! TIFF_filename = trim('rect.tiff')
! im = image_t(rect)
! im%dims = (/ 3, 256, 256 /)
! im%samplesPerPixel = 3
! im%unsigned = .FALSE.
! if(im%empty()) call Message%printMessage("TriadPlots: failed to convert array to rgb image")

! ! create the file
! call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
! if(0.ne.iostat) then
!   call Message%printMessage(" Failed to write image to file : "//iomsg)
! else
!   call Message%printMessage(' color map written to '//trim(TIFF_filename),"(A)")
! end if

call Message%printMessage(' Generating color maps and overlapping coordinate grid ')
call Message%printMessage(' ')

! start with the dissonance map 
mi = minval(DD)
ma = maxval(DD) 
DD = 255.D0 * ((DD-mi)/(ma-mi))
do i=1,xmax
  do j=1,ymax
    iDD = nint(DD(i-1,j-1))
    colormap(1:3,i,ymax+1-j) = (/ R(iDD), G(iDD), B(iDD) /)
  end do 
end do 

! add the grid
do i=1,3
  colormap(i,1:xmax,1:ymax) = colormap(i,1:xmax,1:ymax) * Grid(0:xmax-1,0:ymax-1)
end do 

! also, add a white circle to clearly identify the origin of the grid
cr = 5
crad = dble(cr)   ! circle radius in pixels
cradsq = crad**2
cx = dble(xmax)/2.D0
cy = dble(ymax)/2.D0
do i=-cr-1,cr+1
  do j=-cr-1,cr+1
    if ( (dble(i)**2 + dble(j)**2) .le. cradsq ) then 
      colormap(1:3,xmax/2+i, ymax/2+j) = -1_int8
    end if 
  end do 
end do

TIFF_filename = trim(self%nml%dissonance_file)
! set up the image_t structure
im = image_t(colormap)
im%dims = (/ 3, xmax, ymax /)
im%samplesPerPixel = 3
im%unsigned = .TRUE.
if(im%empty()) call Message%printMessage("TriadPlots: failed to convert array to rgb image")

! create the file
call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message%printMessage(" Failed to write image to file : "//iomsg)
else
  call Message%printMessage(' dissonance map written to '//trim(TIFF_filename),"(A)")
end if

! then the tension map 
mi = minval(TT)
ma = maxval(TT) 
TT = 255.D0 * ((TT-mi)/(ma-mi))
do i=1,xmax
  do j=1,ymax
    iDD = nint(TT(i-1,j-1))
    colormap(1:3,i,ymax+1-j) = (/ R(iDD), G(iDD), B(iDD) /)
  end do 
end do 

do i=1,3
  colormap(i,1:xmax,1:ymax) = colormap(i,1:xmax,1:ymax) * Grid(0:xmax-1,0:ymax-1)
end do 

! also, add a white circle to clearly identify the origin of the grid
do i=-cr-1,cr+1
  do j=-cr-1,cr+1
    if ( (dble(i)**2 + dble(j)**2) .le. cradsq ) then 
      colormap(1:3,xmax/2+i, ymax/2+j) = -1_int8
    end if 
  end do 
end do

TIFF_filename = trim(self%nml%tension_file)

! set up the image_t structure
im = image_t(colormap)
im%dims = (/ 3, xmax, ymax /)
im%samplesPerPixel = 3
im%unsigned = .TRUE.
if(im%empty()) call Message%printMessage("TriadPlots: failed to convert array to rgb image")

! create the file
call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message%printMessage(" Failed to write image to file : "//iomsg)
else
  call Message%printMessage(' tension map written to '//trim(TIFF_filename),"(A)")
end if

! and the moadlity map 
mi = minval(MM) 
ma = maxval(MM) 
MM = 255.D0 * ((MM-mi)/(ma-mi))
do i=1,xmax
  do j=1,ymax
    iDD = nint(MM(i-1,j-1))
    colormap(1:3,i,ymax+1-j) = (/ R(iDD), G(iDD), B(iDD) /)
  end do 
end do 

do i=1,3
  colormap(i,1:xmax,1:ymax) = colormap(i,1:xmax,1:ymax) * Grid(0:xmax-1,0:ymax-1)
end do 

! also, add a white circle to clearly identify the origin of the grid
do i=-cr-1,cr+1
  do j=-cr-1,cr+1
    if ( (dble(i)**2 + dble(j)**2) .le. cradsq ) then 
      colormap(1:3,xmax/2+i, ymax/2+j) = -1_int8
    end if 
  end do 
end do

TIFF_filename = trim(self%nml%modality_file)

! set up the image_t structure
im = image_t(colormap)
im%dims = (/ 3, xmax, ymax /)
im%samplesPerPixel = 3
if(im%empty()) call Message%printMessage("TriadPlots: failed to convert array to rgb image")

! create the file
call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message%printMessage(" Failed to write image to file : "//iomsg)
else
  call Message%printMessage(' modality map written to '//trim(TIFF_filename),"(A)")
end if


deallocate( TT, MM, DD, Grid, colormap, xline, yline )


end subroutine TriadPlots_

!--------------------------------------------------------------------------
subroutine makeGrid_(self, xmax, ymax, im, scl, range)
!DEC$ ATTRIBUTES DLLEXPORT :: TriadPlots_
!! author: MDG 
!! version: 1.0 
!! date: 08/07/25
!!
!! add a hexagonal grid to an image 

IMPLICIT NONE 

class(TriadPlots_T), INTENT(INOUT)        :: self
integer(kind=irg), INTENT(IN)             :: xmax
integer(kind=irg), INTENT(IN)             :: ymax
real(kind=dbl), INTENT(INOUT)             :: im(xmax, ymax)
integer(kind=irg),INTENT(IN)              :: scl
integer(kind=irg),INTENT(IN)              :: range

real(kind=dbl)                            :: ff, x0, x1, y0, y1, c
integer(kind=irg)                         :: i, j

ff = sin(cPi/3.D0)/2.D0 
c = -1.0D0

do i=0,2*range 
  if (i.le.range) then 
    x0 = dble(i*scl)/4.D0
    x1 = dble(i*scl)/2.D0 + dble(xmax-1)/4.D0
    y0 = maxval( (/ dble(ymax-1)/2.D0-dble(i*scl)*ff, 0.D0 /) )
    y1 = dble(ymax-1)
  else
    x0 = dble(range*scl)/4.D0 + dble((i-range)*scl)/2.D0
    x1 = 3.D0*dble(range*scl)/4.D0 + dble((i-range)*scl)/4.D0
    y0 = 0.D0
    y1 = dble(ymax-1)-dble((i-range)*scl)*ff
  end if 
  call DrawLine(im, xmax, ymax, x0, y0, x1, y1, c)
end do  

do i=1,range 
  x0 = dble(xmax-1)/4.D0-dble(i*scl)/4.D0
  x1 = minval( (/ 3.D0*dble(xmax-1)/4.D0+dble(i*scl)/4.D0, dble(xmax-1) /) )
  y0 = dble(i*scl)*ff
  y1 = y0
  call DrawLine(im, xmax, ymax, x0, y0, x1, y1, c)
end do  

do i=1,range-1 
  x0 = dble(xmax-1)/4.D0-dble(i*scl)/4.D0
  x1 = minval( (/ 3.D0*dble(xmax-1)/4.D0+dble(i*scl)/4.D0, dble(xmax-1) /) )
  y0 = dble(ymax-1)-dble(i*scl)*ff
  y1 = y0
  call DrawLine(im, xmax, ymax, x0, y0, x1, y1, c)
end do  

! and complete the perimeter of the hexagon
x0 = 0.D0 
y0 = dble(ymax/2) 
x1 = dble(xmax)/4.D0 
y1 = 0.D0
call DrawLine(im, xmax, ymax, x0, y0, x1, y1, c)

x0 = 3.D0*dble(xmax)/4.D0
y0 = dble(ymax) 
x1 = dble(xmax)
y1 = dble(ymax)/2.D0
call DrawLine(im, xmax, ymax, x0, y0, x1, y1, c)

! finally, make the four triangular corners the background color 
do j=0,ymax/2
  im( 0:int(dble(xmax-1)/4.D0 - dble(j)*(dble(xmax)/dble(2*ymax)) ), j) = 0.D0
  im( int(3.D0*dble(xmax-1)/4.D0 + dble(j)*(dble(xmax)/dble(2*ymax)) ):xmax-1, j) = 0.D0
  im( 0:int(dble(xmax-1)/4.D0 - dble(j)*(dble(xmax)/dble(2*ymax)) ), ymax-1-j) = 0.D0
  im( int(3.D0*dble(xmax-1)/4.D0 + dble(j)*(dble(xmax)/dble(2*ymax)) ):xmax-1, ymax-1-j) = 0.D0
end do

end subroutine makeGrid_

!--------------------------------------------------------------------------
!
! FUNCTION:DrawLine
!
!> @author Marc De Graef, Carnegie Mellon University
!
!> @brief use Xiaolin Wu's anti-aliasing algorithm to draw a line on an image array
!
!> @param im image on which to draw lines
!> @param nx x-dimension 
!> @param ny y-dimension
!> @param xy0 first end point 
!> @param xy1 second end point 
!> @param c intensity value
!
!> @date 02/07/20  MDG 1.0 original
!--------------------------------------------------------------------------
recursive subroutine DrawLine(im, nx, ny, x0, y0, x1, y1, c)

IMPLICIT NONE 

integer(kind=irg), INTENT(IN)   :: nx 
integer(kind=irg), INTENT(IN)   :: ny 
real(kind=dbl), INTENT(INOUT)   :: im(0:nx-1,0:ny-1)
real(kind=dbl), INTENT(INOUT)   :: x0, y0, x1, y1
real(kind=dbl), INTENT(IN)      :: c 
  
real(kind=dbl)                  :: dx, dy, gradient, xend, yend, xgap,  intery, tmp
integer(kind=irg)               :: xpxl1, ypxl1, xpxl2, ypxl2, x
logical                         :: steep 

! rearrange the coordinates if necessary
steep = .FALSE.
if (abs(y1-y0) .gt. abs(x1-x0)) then 
  call swap(x0, y0)
  call swap(x1, y1)
  steep = .TRUE.
end if

if (x0 .gt. x1) then 
  call swap(x0, x1)
  call swap(y0, y1)
end if 

dx = x1-x0
dy = y1-y0
if (dx.eq.0.D0) then 
  gradient = 1.D0
else
  gradient = dy/dx 
end if

xend = round(x0)
yend = y0 + gradient*(xend-x0)
xgap = rfpart(x0+0.5D0)
xpxl1 = xend 
ypxl1 = ipart(yend)
if (steep.eqv..TRUE.) then 
  im(ypxl1, xpxl1) = im(ypxl1, xpxl1) + rfpart(yend) * xgap * c 
  im(ypxl1+1, xpxl1) = im(ypxl1+1, xpxl1) + fpart(yend) * xgap * c 
else
  im(xpxl1, ypxl1) = im(xpxl1, ypxl1) + rfpart(yend) * xgap * c 
  im(xpxl1, ypxl1+1) = im(xpxl1, ypxl1+1) + fpart(yend) * xgap * c 
end if 
intery = yend + gradient

xend = round(x1)
yend = y1 + gradient*(xend-x1)
xgap = fpart(x1+0.5D0)
xpxl2 = xend 
ypxl2 = ipart(yend)
if (steep.eqv..TRUE.) then 
  im(ypxl2, xpxl2) = im(ypxl2, xpxl2) + rfpart(yend) * xgap * c 
  im(ypxl2+1, xpxl2) = im(ypxl2+1, xpxl2) + fpart(yend) * xgap * c 
else
  im(xpxl2, ypxl2) = im(xpxl2, ypxl2) + rfpart(yend) * xgap * c 
  im(xpxl2, ypxl2+1) = im(xpxl2, ypxl2+1) + fpart(yend) * xgap * c 
end if 

if (steep.eqv..TRUE.) then 
  do x = xpxl1+1, xpxl2-1 
    im( ipart(intery), x) = im( ipart(intery), x) + rfpart(intery) * c 
    im( ipart(intery)+1, x) = im( ipart(intery)+1, x) + fpart(intery) * c 
    intery = intery + gradient
  end do 
else
  do x = xpxl1+1, xpxl2-1 
    im( x, ipart(intery)) = im( x, ipart(intery)) + rfpart(intery) * c 
    im( x, ipart(intery)+1) = im( x, ipart(intery)+1) + fpart(intery) * c 
    intery = intery + gradient
  end do 
end if 

end subroutine DrawLine

subroutine swap(x,y) 

  real(kind=dbl), INTENT(INOUT)  :: x, y 
  real(kind=dbl)                 :: tmp 

  tmp = x
  x = y 
  y = tmp

end subroutine swap

function ipart(x) result(r)

  real(kind=dbl), INTENT(IN) :: x 
  integer(kind=irg)          :: r 

  r = floor(x)

end function ipart

function round(x) result(r)

  real(kind=dbl), INTENT(IN) :: x 
  integer(kind=irg)          :: r 

  r = ipart(x+0.5D0)

end function round

function fpart(x) result(r)

  real(kind=dbl), INTENT(IN) :: x 
  real(kind=dbl)             :: r 

  r = x-floor(x)

end function fpart

function rfpart(x) result(r)

  real(kind=dbl), INTENT(IN) :: x 
  real(kind=dbl)             :: r 

  r = 1.D0 - fpart(x)

end function rfpart


end module mod_TriadPlots