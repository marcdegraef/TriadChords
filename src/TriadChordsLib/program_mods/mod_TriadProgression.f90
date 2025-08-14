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
  integer(kind=irg)             :: scale
  integer(kind=irg)             :: interval_range
  character(1)                  :: bg
  character(1)                  :: bgsubtract
  character(fnlen)              :: dissonance_file
  character(fnlen)              :: tension_file
  character(fnlen)              :: sonority_file
  character(fnlen)              :: outname
end type TriadProgressionNameListType

! class definition
type, public :: TriadProgression_T
private 
  character(fnlen)                      :: nmldeffile = 'TriadProgression.nml'
  type(TriadProgressionNameListType)    :: nml 
  integer(kind=irg)                     :: TwoOrThree

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
  call TriadProgression%readNameList_(nmlfile)
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
integer(kind=irg) :: scale
integer(kind=irg) :: interval_range
character(1)      :: bg
character(1)      :: bgsubtract
character(fnlen)  :: outname
character(fnlen)  :: dissonance_file
character(fnlen)  :: tension_file
character(fnlen)  :: sonority_file

namelist /TriadProgression/ triad1, triad2, triad3, scale, interval_range, outname, dissonance_file,  &
                            tension_file, bg, bgsubtract, sonority_file

triad1 = (/ 0, 4, 7 /)
triad2 = (/ 0, 3, 7 /)
triad3 = (/ 0, 0, 0 /)
! number of pixels per interval
scale = 40
! range of plot along horizontal axis (in units of intervals)
interval_range = 24
bg = 'w'
bgsubtract = 'y'
outname = 'undefined'
dissonance_file = 'undefined'
tension_file = 'undefined'
sonority_file = 'undefined'

! read the name list from the config file
open(UNIT=dataunit,FILE=trim(nmlfile),DELIM='apostrophe',STATUS='old')
read(UNIT=dataunit,NML=TriadProgression)
close(UNIT=dataunit,STATUS='keep')

! check for required entries
if (trim(outname).eq.'undefined') then
  call Message%printError('TriadProgression_constructor:',' outname undefined in '//trim(nmlfile))
end if

! are we doing a two or three chord progression?
self%TwoOrThree = 3
if (sum(abs(triad3)).eq.0) self%TwoOrThree = 2

self%nml%triad1 = triad1
self%nml%triad2 = triad2
self%nml%triad3 = triad3
self%nml%scale = scale
self%nml%interval_range = interval_range
self%nml%bg = bg
self%nml%bgsubtract = bgsubtract
self%nml%outname = trim(outname)
self%nml%dissonance_file = trim(dissonance_file)
self%nml%tension_file = trim(tension_file)
self%nml%sonority_file = trim(sonority_file)

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
use mod_image
use mod_math
use mod_postscript
use mod_axis
use omp_lib
use ISO_C_BINDING
use, intrinsic :: iso_fortran_env

IMPLICIT NONE 

class(TriadProgression_T), INTENT(INOUT)        :: self
character(fnlen), INTENT(INOUT)                 :: progname 
character(fnlen), INTENT(INOUT)                 :: progdesc

type(Triad_T)                                   :: TT, myTT
type(PostScript_T)                              :: PS 
type(axis_T)                                    :: AX
type(IO_T)                                      :: Message

integer(kind=sgl)                               :: i,j,k,l,iv11,iv12,iv21,iv22,iv31,iv32,dimx,dimy,numx,numy,istat, &
                                                   TID, io_int(2), imanum, mark, interval_range
real(kind=dbl)                                  :: set1(3), set2(3), set3(3), fr2(3), fr3(3), tt12, p1, p2
real(kind=dbl)                                  :: tt13, tt23, dd12, dd13, dd23, mm12, mm13, mm23, f1, fr, frat
real(kind=sgl)                                  :: xmi, xma, ymi, yma, axw, xll, yll, qx, qy, dx, fw, fh, sx, sy, q, &
                                                   dy(12), lthick
real(kind=dbl),allocatable                      :: xx(:),dd(:,:),ttt(:,:), mm(:,:), ran(:), Grid(:,:), Grid2(:,:), &
                                                   background(:,:), backgr(:), dd2(:), tt2(:), son(:) 
real(kind=sgl),allocatable                      :: xvec(:), yvec(:)
character(len=7)                                :: id
character(2)                                    :: lett
logical                                         :: xautorange, yautorange, overplot, drawborder
character(3)                                    :: xmode, ymode, pmode, scalex, scaley
character(fnlen)                                :: title, xtitle, ytitle

associate( nml => self%nml )

call Message%printMessage(' ')
call Message%printMessage(' Program Name       : '//trim(progname))
call Message%printMessage(' Program Descriptor : '//trim(progdesc))
call Message%printMessage(' ')

if (self%TwoOrThree.eq.3) then 
! instantiate the Triad class
  TT = Triad_T()
  f1 = TT%getf1()
  fr = 1.D0/TT%fits%freq2int

! define the simulation parameters and allocate arrays
  numx = nml%interval_range * nml%scale / 2
  numy = int( numx * sqrt(3.D0)/2.D0 )
  dimx = 2*numx+1
  dimy = 2*numy+1
  allocate(dd(dimx,dimy),ttt(dimx,dimy),mm(dimx,dimy),stat=istat)

  set1 = dble(nml%triad1)
  set2 = dble(nml%triad2)
  set3 = dble(nml%triad3)

  write(*,*) nml%triad1,': ', set1 
  write(*,*) nml%triad2,': ', set2 
  write(*,*) nml%triad3,': ', set3 

! start major computation loop
  p1 = dble(nml%interval_range/2)
  p2 = p1/2.D0


  call OMP_SET_NUM_THREADS(OMP_GET_MAX_THREADS())
  io_int(1) = OMP_GET_NUM_THREADS()
  call Message%WriteValue(' -> Number of threads set to ',io_int,1,"(I3)")

!$OMP PARALLEL DEFAULT(SHARED) PRIVATE(TID,fr2,fr3,tt13,tt12,tt23,dd13,dd23,dd12,i,j,k,l,myTT) 

  TID = OMP_GET_THREAD_NUM( )

! initialize a threadsafe copy of the Triad_T class by passing the important parameters
! to the myTT constructor
  myTT = Triad_T( no_read = .TRUE., timbre = TT%gettimbre(), spath = TT%getsourcepath() )


!$OMP DO SCHEDULE(DYNAMIC)
  do j=1,dimy
        fr3 = (p1 * dble(j - (numy+1))/dble(numy)) + set3 ! xx(i) + set2
        tt13 =        myTT%triad_tension(fr3(1),set1(1),set1(2))
        tt13 = tt13 + myTT%triad_tension(fr3(1),set1(1),set1(3))
        tt13 = tt13 + myTT%triad_tension(fr3(1),set1(2),set1(3))
        tt13 = tt13 + myTT%triad_tension(fr3(2),set1(1),set1(2))
        tt13 = tt13 + myTT%triad_tension(fr3(2),set1(1),set1(3))
        tt13 = tt13 + myTT%triad_tension(fr3(2),set1(2),set1(3))
        tt13 = tt13 + myTT%triad_tension(fr3(3),set1(1),set1(2))
        tt13 = tt13 + myTT%triad_tension(fr3(3),set1(1),set1(3))
        tt13 = tt13 + myTT%triad_tension(fr3(3),set1(2),set1(3))
        
        tt13 = tt13 + myTT%triad_tension(set1(1),fr3(1),fr3(2))
        tt13 = tt13 + myTT%triad_tension(set1(1),fr3(1),fr3(3))
        tt13 = tt13 + myTT%triad_tension(set1(1),fr3(2),fr3(3))
        tt13 = tt13 + myTT%triad_tension(set1(2),fr3(1),fr3(2))
        tt13 = tt13 + myTT%triad_tension(set1(2),fr3(1),fr3(3))
        tt13 = tt13 + myTT%triad_tension(set1(2),fr3(2),fr3(3))
        tt13 = tt13 + myTT%triad_tension(set1(3),fr3(1),fr3(2))
        tt13 = tt13 + myTT%triad_tension(set1(3),fr3(1),fr3(3))
        tt13 = tt13 + myTT%triad_tension(set1(3),fr3(2),fr3(3))
        tt13 = tt13/18.D0

        dd13 = 0.D0
        do k=1,3
                do l=1,3
                        dd13 = dd13 + myTT%triad_dissonance(dabs(fr3(k)-set1(l)))
                end do
        end do
        dd13 = dd13/9.D0

        do i=1,dimx

          fr2 = p1 * dble(i - (numx+1))/dble(numx) + p2 * dble(j - (numy+1))/dble(numy) + set2
          tt23 =        myTT%triad_tension(fr2(1),fr3(1),fr3(2))
          tt23 = tt23 + myTT%triad_tension(fr2(1),fr3(1),fr3(3))
          tt23 = tt23 + myTT%triad_tension(fr2(1),fr3(2),fr3(3))
          tt23 = tt23 + myTT%triad_tension(fr2(2),fr3(1),fr3(2))
          tt23 = tt23 + myTT%triad_tension(fr2(2),fr3(1),fr3(3))
          tt23 = tt23 + myTT%triad_tension(fr2(2),fr3(2),fr3(3))
          tt23 = tt23 + myTT%triad_tension(fr2(3),fr3(1),fr3(2))
          tt23 = tt23 + myTT%triad_tension(fr2(3),fr3(1),fr3(3))
          tt23 = tt23 + myTT%triad_tension(fr2(3),fr3(2),fr3(3))
          
          tt23 = tt23 + myTT%triad_tension(fr3(1),fr2(1),fr2(2))
          tt23 = tt23 + myTT%triad_tension(fr3(1),fr2(1),fr2(3))
          tt23 = tt23 + myTT%triad_tension(fr3(1),fr2(2),fr2(3))
          tt23 = tt23 + myTT%triad_tension(fr3(2),fr2(1),fr2(2))
          tt23 = tt23 + myTT%triad_tension(fr3(2),fr2(1),fr2(3))
          tt23 = tt23 + myTT%triad_tension(fr3(2),fr2(2),fr2(3))
          tt23 = tt23 + myTT%triad_tension(fr3(3),fr2(1),fr2(2))
          tt23 = tt23 + myTT%triad_tension(fr3(3),fr2(1),fr2(3))
          tt23 = tt23 + myTT%triad_tension(fr3(3),fr2(2),fr2(3))
          tt23 = tt23/18.D0

          dd23 = 0.D0
          do k=1,3
                  do l=1,3
                          dd23 = dd23 + myTT%triad_dissonance(dabs(fr2(k)-fr3(l)))
                  end do
          end do
          dd23 = dd23/9.D0

          tt12 =        myTT%triad_tension(set1(1),fr2(1),fr2(2))
          tt12 = tt12 + myTT%triad_tension(set1(1),fr2(1),fr2(3))
          tt12 = tt12 + myTT%triad_tension(set1(1),fr2(2),fr2(3))
          tt12 = tt12 + myTT%triad_tension(set1(2),fr2(1),fr2(2))
          tt12 = tt12 + myTT%triad_tension(set1(2),fr2(1),fr2(3))
          tt12 = tt12 + myTT%triad_tension(set1(2),fr2(2),fr2(3))
          tt12 = tt12 + myTT%triad_tension(set1(3),fr2(1),fr2(2))
          tt12 = tt12 + myTT%triad_tension(set1(3),fr2(1),fr2(3))
          tt12 = tt12 + myTT%triad_tension(set1(3),fr2(2),fr2(3))
          
          tt12 = tt12 + myTT%triad_tension(fr2(1),set1(1),set1(2))
          tt12 = tt12 + myTT%triad_tension(fr2(1),set1(1),set1(3))
          tt12 = tt12 + myTT%triad_tension(fr2(1),set1(2),set1(3))
          tt12 = tt12 + myTT%triad_tension(fr2(2),set1(1),set1(2))
          tt12 = tt12 + myTT%triad_tension(fr2(2),set1(1),set1(3))
          tt12 = tt12 + myTT%triad_tension(fr2(2),set1(2),set1(3))
          tt12 = tt12 + myTT%triad_tension(fr2(3),set1(1),set1(2))
          tt12 = tt12 + myTT%triad_tension(fr2(3),set1(1),set1(3))
          tt12 = tt12 + myTT%triad_tension(fr2(3),set1(2),set1(3))
          tt12 = tt12/18.D0

          dd12 = 0.D0
          do k=1,3
                  do l=1,3
                          dd12 = dd12 + myTT%triad_dissonance(dabs(set1(k)-fr2(l)))
                  end do
          end do
          dd12 = dd12/9.D0

!$OMP CRITICAL
          dd(i,j)  = dd12+dd23+dd13
          ttt(i,j) = tt12+tt23+tt13
!$OMP END CRITICAL
        end do
      if (mod(j,50).eq.0) then 
        io_int = (/ j, dimy /)
        call Message%WriteValue(' completed ',io_int, 2, frm="(I5,' of ',I5,' lines')")
      end if
  end do
!$OMP END DO
!$OMP END PARALLEL

! some corrections due to a likely bug ... they won't matter in the end...
  dd(1,1) = dd(2,1) 
  ttt(1:3,1) = ttt(4,1) 

  allocate(Grid(dimx,dimy), Grid2(0:dimx-1,0:dimy-1))
  Grid  = 1.D0
  Grid2 = 1.D0
  call TT%makeGrid(dimx, dimy, Grid2, nml%scale, nml%interval_range, nml%bg)

! binarize the Grid to 1 and 0 
  do i=1,dimx
    do j=1,dimy
      if (Grid2(i-1,j-1).eq.-100.D0) then 
        Grid(i,j) = -100.D0
      else
        if (Grid2(i-1,j-1).gt.0.5D0) then 
          Grid(i,j) = 1.D0
        else
          Grid(i,j) = 0.D0 
        end if
      end if 
    end do 
  end do 

  deallocate( Grid2 )

  if (nml%bgsubtract.eq.'y') then 
    allocate( background(dimx, dimy) )
    background = sfit(dimx, dimy, dd, 3)
    dd = dd - background
    background = sfit(dimx, dimy, ttt, 3)
    ttt = ttt - background
    deallocate( background )
  end if 

  call TT%saveColorMap( dimx, dimy, dd, Grid, nml%dissonance_file )
  call TT%saveColorMap( dimx, dimy, ttt, Grid, nml%tension_file )


! for now save the resulting arrays in a file that can be read by IDL for debugging purposes
  open(unit=dataunit,file=trim(self%nml%outname),status='unknown',action='write',form='unformatted')
  write (dataunit) real(dd)
  write (dataunit) real(ttt)
  close(unit=dataunit,status='keep')

else  ! we're doing a two-chord progression
! instantiate the Triad class
  TT = Triad_T()
  f1 = TT%getf1()
  fr = 1.D0/TT%fits%freq2int
  interval_range = 48 

! define the simulation parameters and allocate arrays
  dimx = interval_range * nml%scale 
  allocate(dd2(dimx),tt2(dimx),son(dimx),xvec(dimx),yvec(dimx),backgr(dimx),stat=istat)

  set1 = dble(nml%triad1) ! this is the fixed triad
  set2 = dble(nml%triad2) ! and this is the moving one

  write(*,*) nml%triad1,': ', set1 
  write(*,*) nml%triad2,': ', set2 

  p1 = dble(nml%interval_range/2)

  do j=1,dimx
        xvec(j) = real(p1 * dble(2*j - (dimx+1))/dble(dimx))
        fr2 =  p1 * dble(2*j - (dimx+1))/dble(dimx) + set2 
        tt12 =        TT%triad_tension(fr2(1),set1(1),set1(2))
        tt12 = tt12 + TT%triad_tension(fr2(1),set1(1),set1(3))
        tt12 = tt12 + TT%triad_tension(fr2(1),set1(2),set1(3))
        tt12 = tt12 + TT%triad_tension(fr2(2),set1(1),set1(2))
        tt12 = tt12 + TT%triad_tension(fr2(2),set1(1),set1(3))
        tt12 = tt12 + TT%triad_tension(fr2(2),set1(2),set1(3))
        tt12 = tt12 + TT%triad_tension(fr2(3),set1(1),set1(2))
        tt12 = tt12 + TT%triad_tension(fr2(3),set1(1),set1(3))
        tt12 = tt12 + TT%triad_tension(fr2(3),set1(2),set1(3))
        
        tt12 = tt12 + TT%triad_tension(set1(1),fr2(1),fr2(2))
        tt12 = tt12 + TT%triad_tension(set1(1),fr2(1),fr2(3))
        tt12 = tt12 + TT%triad_tension(set1(1),fr2(2),fr2(3))
        tt12 = tt12 + TT%triad_tension(set1(2),fr2(1),fr2(2))
        tt12 = tt12 + TT%triad_tension(set1(2),fr2(1),fr2(3))
        tt12 = tt12 + TT%triad_tension(set1(2),fr2(2),fr2(3))
        tt12 = tt12 + TT%triad_tension(set1(3),fr2(1),fr2(2))
        tt12 = tt12 + TT%triad_tension(set1(3),fr2(1),fr2(3))
        tt12 = tt12 + TT%triad_tension(set1(3),fr2(2),fr2(3))
        tt12 = tt12/18.D0

        dd12 = 0.D0
        do k=1,3
                do l=1,3
                        dd12 = dd12 + TT%triad_dissonance(dabs(fr2(k)-set1(l)))
                end do
        end do
        dd12 = dd12/9.D0

        dd2(j) = dd12
        tt2(j) = tt12
  end do 
  
! compute the sonority
  son = dd2 + TT%fits%delta * tt2  ! this is the instability
  call polyfit(dble(xvec), son, dimx, 3, backgr)
  son = son - backgr
  son = son - minval(son)
  son = son/maxval(son)
  son = 1.D0 - son

! the output for this mode is in postscript format so we need to open a file
! and then call the axis routine to plot the sonority curve
  imanum = 1
  PS = PostScript_T(progdesc, imanum, dontask=.TRUE., psname = trim(nml%sonority_file) )
  call PS%setlinewidth(0.25)
  
! initialize the axis class
  xll = 2.0
  yll = 2.25
  axw = 6.5
  AX = Axis_T( PS, axw = axw, xll = xll, yll = yll )
  yvec = real(son)
  xmi = real(minval(xvec))
  xma = real(maxval(xvec))
  ymi = real(minval(son))
  yma = real(maxval(son))
  xautorange = .TRUE.
  yautorange = .TRUE.
  xmode = 'LIN'
  ymode = 'LIN'
  pmode = 'BAR'
  lthick = 0.0075
  mark = 1
  scalex = 'NON'
  scaley = 'NON'
  overplot = .FALSE.
  drawborder = .TRUE.
  write (lett,"(I2)") nml%triad1(2)
  title = 'Fixed Chord: '//trim(adjustl(lett))//'-'
  write (lett,"(I2)") nml%triad1(3) - nml%triad1(2)
  title = trim(title)//trim(adjustl(lett))//'; Sliding Chord: '
  write (lett,"(I2)") nml%triad2(2)
  title = trim(title)//trim(adjustl(lett))//'-'
  write (lett,"(I2)") nml%triad2(3) - nml%triad2(2)
  title = trim(title)//trim(adjustl(lett))
  xtitle = 'Second chord location'
  ytitle = 'Sonority (arbitrary units)'

  call AX%axis(dimx,xvec,yvec,xmi,xma,ymi,yma,xautorange,yautorange,xmode,ymode,pmode,lthick, &
               mark,scalex,scaley,overplot,drawborder,title,xtitle,ytitle)

! determine lower left corner of plot
  call AX%initframe('start',db=.FALSE.)

! fw=0.5*(8.50-PS%psscale*PS%psfigwidth)
! fh=0.5*(11.0-PS%psscale*PS%psfigheight)
! call PS%filledcircle(fw, fh-10, 1.0, 0.5)

! go to the origin:
  qx = -xmi*100.0/(xma-xmi)
  qy = -ymi*100.0/(yma-ymi)
  dx = 0.98 * 100.0/real(nml%interval_range+1)
  write (*,*) ' line step size = ', dx, qx, qy

! vertical lines
  do i = nml%interval_range/2+1,-nml%interval_range/2+1,-1 ! 1,nml%interval_range+1
    if ( mod(i-1,12).eq.0) then 
      call PS%setlinecolor( (/ 1.0, 0.0, 0.0 /) )
      call PS%setlinewidth(0.045)
    else
      call PS%setlinecolor( (/ 0.5, 0.5, 0.5 /) )
      call PS%setlinewidth(0.015)
    end if
    call PS%move(50.0-(i-1)*dx,0.0)
    call PS%draw(50.0-(i-1)*dx,100.0)
    call PS%stroke()
  end do   

! note labels at top and bottom
  dy = (/ 0.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 1.0, 0.0, 1.0, 0.0 /)
  sy = 1.5
  call PS%setlinecolor( (/ 0.0, 0.0, 0.0 /) )
  call PS%setfont( PSfonts(4), 1.75)
  call PS%setlinewidth(0.02)
  do i=1,nml%interval_range+1
    j = mod(i,12)
    if (j.eq.0) j = 12
    call PS%text( (i-1)*dx+dx/2.0+0.25, -0.50-(1.0 + dy(j)) * sy, TT%chromatic%notes(j))
    call PS%stroke()
    call PS%text( (i-1)*dx+dx/2.0+0.25, 99.8+(1.0 + dy(j)) * sy, TT%chromatic%notes(j))
    call PS%stroke()
  end do 

! close Postscript file
  call PS%closefile()

end if 

end associate 

end subroutine TriadProgression_

end module mod_TriadProgression
