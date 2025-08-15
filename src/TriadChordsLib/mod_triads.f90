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

module mod_triads

use mod_global
use mod_kinds 
use mod_parameters     ! parameters that have to do with musical quantities

type, public :: Triad_T
  private 
    character(fnlen)            :: sourcepath
    type(fit_parameters),public :: fits    
    type(timbre_descriptor)     :: timbre_parameters
    type(triad_colors)          :: clrs
    type(triad_chords)          :: TC
    type(scale),public          :: chromatic

! then a few allocatable arrays
    real(kind=dbl),allocatable  :: timbre(:)        ! timbre amplitudes
    real(kind=dbl),allocatable  :: partials(:)      ! intervals for all upper partials 
    real(kind=dbl),allocatable  :: prods2(:)        ! precomputed pairwise products of timbre amplitudes
    real(kind=dbl),allocatable  :: prods(:)         ! precomputed triple products of timbre amplitudes
    real(kind=dbl),allocatable  :: q2(:),q3(:)      ! auxiliary arrays

contains
  private

    procedure, pass(self) :: triad_
    procedure, pass(self) :: triad_reset_
    procedure, pass(self) :: triad_dissonance_
    procedure, pass(self) :: triad_tension_
    procedure, pass(self) :: triad_modality_
    procedure, pass(self) :: totalquantity_
    procedure, pass(self) :: get_dissonance_
    procedure, pass(self) :: get_tension_
    procedure, pass(self) :: get_modality_
    procedure, pass(self) :: setbase_
    procedure, pass(self) :: getbase_
    procedure, pass(self) :: settimbre_
    procedure, pass(self) :: gettimbre_
    procedure, pass(self) :: settimbre_type_
    procedure, pass(self) :: gettimbre_type_
    procedure, pass(self) :: setnum_partials_
    procedure, pass(self) :: getnum_partials_
    procedure, pass(self) :: setsourcepath_
    procedure, pass(self) :: getsourcepath_
    procedure, pass(self) :: getclrs_
    procedure, pass(self) :: setf1_
    procedure, pass(self) :: getf1_
    procedure, pass(self) :: makeGrid_
    procedure, pass(self) :: saveColorMap_

    generic, public :: triad => triad_
    generic, public :: triad_reset => triad_reset_
    generic, public :: triad_dissonance => triad_dissonance_
    generic, public :: triad_tension => triad_tension_
    generic, public :: triad_modality => triad_modality_
    generic, public :: totalquantity => totalquantity_
    generic, public :: get_dissonance => get_dissonance_
    generic, public :: get_tension => get_tension_
    generic, public :: get_modality => get_modality_
    generic, public :: settimbre => settimbre_
    generic, public :: gettimbre => gettimbre_
    generic, public :: setbase => setbase_
    generic, public :: getbase => getbase_
    generic, public :: settimbre_type => settimbre_type_
    generic, public :: gettimbre_type => gettimbre_type_
    generic, public :: setnum_partials => setnum_partials_
    generic, public :: getnum_partials => getnum_partials_
    generic, public :: setsourcepath => setsourcepath_
    generic, public :: getsourcepath => getsourcepath_
    generic, public :: getclrs => getclrs_
    generic, public :: setf1 => setf1_
    generic, public :: getf1 => getf1_
    generic, public :: makeGrid => makeGrid_
    generic, public :: saveColorMap => saveColorMap_

end type Triad_T

interface Triad_T 
  module procedure Triad_constructor
end interface Triad_T 

contains

!--------------------------------------------------------------------------
type(Triad_T) function Triad_constructor( no_alloc, no_read, timbre, spath, nump ) result(Triad)
!DEC$ ATTRIBUTES DLLEXPORT :: Triad_constructor
  !! author: MDG
  !! date: 10/22/08
  !!
  !! constructor for the Triad_T class

use mod_io 

IMPLICIT NONE

logical,INTENT(IN),OPTIONAL                 :: no_alloc
logical,INTENT(IN),OPTIONAL                 :: no_read
type(timbre_descriptor),INTENT(IN),OPTIONAL :: timbre
character(fnlen),INTENT(IN),OPTIONAL        :: spath
integer(kind=irg),INTENT(IN),OPTIONAL       :: nump

type(IO_T)                                  :: Message 

integer(kind=irg)                           :: i,j,k,i1,i2,istat         ! loop parameters
character(fnlen)                            :: ConfigFile = 'TriadConfig.txt'
logical                                     :: fexists, alloc

integer(kind=irg)                           :: num_partials, timbre_type
real(kind=dbl)                              :: base, f1 
character(fnlen)                            :: sourcepath

namelist /TriadConfig/ num_partials, timbre_type, base, f1, sourcepath 

! use a shorthand notation
associate(Tt => Triad%timbre_parameters)

! make sure the TriadConfig.txt file is present in this folder; if it is not, 
! then create it and send a message to the user. If it does exist, read it
! and populate the class parameters.
if (.not.present(no_read)) then 
  fexists = .FALSE.
  inquire(file=trim(ConfigFile), exist=fexists)
  if (fexists.eqv..TRUE.) then  ! read the file (formatted as a namelist)
  ! read the name list from the config file
    open(UNIT=dataunit,FILE=trim(ConfigFile),DELIM='apostrophe',STATUS='old')
    read(UNIT=dataunit,NML=TriadConfig)
    close(UNIT=dataunit,STATUS='keep')

  ! check for required entries
    if (trim(sourcepath).eq.'undefined') then
      call Message%printError('Triad_constructor:',' sourcepath undefined in '//ConfigFile)
    end if

  ! and place the parameters in the class variables
    Tt%num_partials = num_partials
    Tt%base = base
    Tt%f1 = f1
    Tt%timbre_type = timbre_type
    Triad%sourcepath = trim(sourcepath)
  else  ! generate a template config file and send a message to the user
    open( dataunit, file = trim(ConfigFile), status = 'new', form='formatted')
    call Message%printMessage( (/ " &TriadConfig                            ", &
                                  "! number of upper partials to include    ", &
                                  " num_partials = 6,                       ", &
                                  "! timbre type (1=b^n; 2=1/n)             ", &
                                  " timbre_type = 1,                        ", &
                                  "! base factor for timbre type 1          ", &
                                  " base = 0.88D0,                          ", &
                                  "! fundamental frequency (Hz)             ", &
                                  " f1 = 440.0D0,                           ", &
                                  "! full path to TriadChords source folder ", &
                                  " sourcepath = 'undefined',               ", &
                                  " /                                       " /), redirect = dataunit )
    close(dataunit, status = 'keep')
    call Message%printMessage(' Configuration file did not exist... created TriadConfig.txt in current folder.')
    call Message%printError('Triad_constructor',' aborting run; please edit TriadConfig.txt file')
  end if
else ! use the input timbre to initialize the parameters (used for multi-threaded cases)
  Triad%timbre_parameters = timbre 
  Triad%sourcepath = trim(spath) 
end if 

alloc = .TRUE.
if (present(no_alloc)) then 
  if (no_alloc.eqv..TRUE.) then 
    alloc = .FALSE.
  end if
end if 

if (alloc.eqv..TRUE.) then
  if (present(nump)) then    ! we override the value of the num_partials parameter if necessary
    Tt%num_partials = nump
  end if  

! allocate the timbre variable with one extra slot for the root frequency
  if (.not.allocated(Triad%timbre)) then 
    allocate(Triad%timbre(0:Tt%num_partials),stat=istat)
  else
    deallocate(Triad%timbre)
    allocate(Triad%timbre(0:Tt%num_partials),stat=istat)
  endif

  ! fill in the timbre values
  Triad%timbre(0) = 1.0D0
  if (Tt%timbre_type.eq.1) then
    Triad%timbre(1) = Tt%base
    do i=2,Tt%num_partials
      Triad%timbre(i) = Triad%timbre(i-1)*Tt%base
    end do
  endif

  if (Tt%timbre_type.eq.2) then
    do i=1,Tt%num_partials
      Triad%timbre(i) = 1.D0/dble(i+1)
    end do
  endif

  ! we'll set the total energy equal to 1
  ! Triad%timbre = Triad%timbre / sqrt(sum(Triad%timbre**2))

  ! allocate an array for the partial intervals
  if (.not.allocated(Triad%partials)) then 
    allocate(Triad%partials(0:Tt%num_partials),stat=istat)
  else
    deallocate(Triad%partials)
    allocate(Triad%partials(0:Tt%num_partials),stat=istat)
  endif

  ! and fill the array
  Triad%partials(0) = 0.D0
  do i=1,Tt%num_partials
    ! Triad%partials(i) = Triad%fits%freq2int * dlog10(dble(i+1)/dble(i)) + Triad%partials(i-1)
    Triad%partials(i) = Triad%fits%freq2int * dlog10(dble(i+1)) 
  end do

  ! allocate an array for the precomputed products of timbre amplitudes
  if (.not.allocated(Triad%prods)) then 
    allocate(Triad%prods((Tt%num_partials+1)**3),stat=istat)
  else
    deallocate(Triad%prods)
    allocate(Triad%prods((Tt%num_partials+1)**3),stat=istat)
  endif

  if (.not.allocated(Triad%prods2)) then 
    allocate(Triad%prods2((Tt%num_partials+1)**2),stat=istat)
  else
    deallocate(Triad%prods2)
    allocate(Triad%prods2((Tt%num_partials+1)**2),stat=istat)
  endif

  ! and fill the arrays
  i1 = 1
  i2 = 1
  do i=0,Tt%num_partials
    do j=0,Tt%num_partials
      Triad%prods2(i1) = Triad%timbre(i) * Triad%timbre(j)
      do k=0,Tt%num_partials
        Triad%prods(i2) = Triad%prods2(i1) * Triad%timbre(k)
        i2 = i2 + 1
      end do
      i1 = i1 + 1
    end do
  end do

  ! finally, allocate some additional auxiliary arrays
  allocate(Triad%q2((Tt%num_partials+1)**2),stat=istat)
  allocate(Triad%q3((Tt%num_partials+1)**3),stat=istat)
end if 

end associate 

end function Triad_constructor

!--------------------------------------------------------------------------
subroutine triad_reset_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: triad_reset_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! deallocate all arrays in the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)    :: self

if (allocated(self%timbre)) deallocate(self%timbre)
if (allocated(self%partials)) deallocate(self%partials)
if (allocated(self%prods2)) deallocate(self%prods2)
if (allocated(self%prods)) deallocate(self%prods)
if (allocated(self%q2)) deallocate(self%q2)
if (allocated(self%q3)) deallocate(self%q3)

end subroutine triad_reset_

!--------------------------------------------------------------------------
subroutine setbase_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setbase_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! set base in the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)     :: self
real(kind=dbl), INTENT(IN)       :: inp

self%timbre_parameters%base = inp

end subroutine setbase_

!--------------------------------------------------------------------------
function getbase_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getbase_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! get base from the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)     :: self
real(kind=dbl)                   :: out

out = self%timbre_parameters%base

end function getbase_

!--------------------------------------------------------------------------
subroutine settimbre_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: settimbre_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! set timbre in the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)             :: self
type(timbre_descriptor), INTENT(IN)       :: inp

self%timbre_parameters = inp

end subroutine settimbre_

!--------------------------------------------------------------------------
function gettimbre_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: gettimbre_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! get timbre from the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)    :: self
type(timbre_descriptor)          :: out

out = self%timbre_parameters

end function gettimbre_

!--------------------------------------------------------------------------
subroutine settimbre_type_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: settimbre_type_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! set timbre_type in the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%timbre_parameters%timbre_type = inp

end subroutine settimbre_type_

!--------------------------------------------------------------------------
function gettimbre_type_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: gettimbre_type_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! get timbre_type from the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%timbre_parameters%timbre_type

end function gettimbre_type_

!--------------------------------------------------------------------------
subroutine setnum_partials_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setnum_partials_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! set num_partials in the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)     :: self
integer(kind=irg), INTENT(IN)       :: inp

self%timbre_parameters%num_partials = inp

end subroutine setnum_partials_

!--------------------------------------------------------------------------
function getnum_partials_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getnum_partials_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! get num_partials from the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)     :: self
integer(kind=irg)                   :: out

out = self%timbre_parameters%num_partials

end function getnum_partials_

!--------------------------------------------------------------------------
subroutine setsourcepath_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setsourcepath_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! set sourcepath in the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)     :: self
character(fnlen), INTENT(IN)      :: inp

self%sourcepath = inp

end subroutine setsourcepath_

!--------------------------------------------------------------------------
function getsourcepath_(self) result(out)
!DEC$ ATTRIBUTES DLLEXPORT :: getsourcepath_
!! author: MDG
!! version: 1.0
!! date: 08/04/25
!!
!! get sourcepath from the Triad_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)     :: self
character(fnlen)                  :: out

out = self%sourcepath

end function getsourcepath_

!--------------------------------------------------------------------------
subroutine setf1_(self,inp)
!DEC$ ATTRIBUTES DLLEXPORT :: setf1_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! set f1 in the TriadPlots_T class

IMPLICIT NONE

class(Triad_T), INTENT(INOUT)    :: self
real(kind=dbl), INTENT(IN)       :: inp

self%timbre_parameters%f1 = inp

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

class(Triad_T), INTENT(INOUT)    :: self
real(kind=dbl)                   :: out

out = self%timbre_parameters%f1

end function getf1_

!--------------------------------------------------------------------------
subroutine getclrs_(self, R, G, B )
!DEC$ ATTRIBUTES DLLEXPORT :: getclrs_
!! author: MDG
!! version: 1.0
!! date: 08/06/25
!!
!! get the color tables 

use ISO_C_BINDING
use, intrinsic :: iso_fortran_env

IMPLICIT NONE

class(Triad_T), INTENT(INOUT) :: self
integer(int8),INTENT(INOUT)   :: R(0:255)
integer(int8),INTENT(INOUT)   :: G(0:255)
integer(int8),INTENT(INOUT)   :: B(0:255)

type(triad_colors)            :: TC 

R = TC%Red
G = TC%Green
B = TC%Blue

end subroutine getclrs_

!--------------------------------------------------------------------------
recursive function triad_dissonance_(self, x) result(itd)
!DEC$ ATTRIBUTES DLLEXPORT :: triad_dissonance_
  !! author: MDG
  !! date: 10/22/08
  !!
  !! computes the total dissonance for a pair of notes taking into account the upper partials.
  !!
  !! equation 3-4 in Cook&Fujisawa (2006)

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)          :: self
real(kind=dbl),INTENT(IN)             :: x                ! the input interval (in units of Hertz)
real(kind=dbl)                        :: itd 

real(kind=dbl)                        :: d, np, xi        ! axiliary variable
integer(kind=irg)                     :: i,j,i1,istat     ! loop counters, etc...

! convert Hertz to interval units 
! xi = log10(x) * self%fits%freq2int

i1 =1 
np = dble(self%timbre_parameters%num_partials+1)
do i=0,self%timbre_parameters%num_partials
  do j=0,self%timbre_parameters%num_partials
    self%q2(i1) = dabs(self%partials(i) - (x+self%partials(j)))**self%fits%gam
    i1 = i1+1
  end do
end do

d = sum(self%prods2 * (dexp(-self%fits%b1*self%q2)-dexp(-self%fits%b2*self%q2)))
itd = self%fits%b3 * d / np

end function triad_dissonance_

!--------------------------------------------------------------------------
recursive function triad_tension_(self,i1,i2,i3) result(itt)
!DEC$ ATTRIBUTES DLLEXPORT :: triad_tension_
  !! author: MDG
  !! date: 10/22/08
  !!
  !! computes the total tension as a sum over the three partial tensions for all of the upper
  !! partials of three given tones.
  !!
  !! equation 2 in Cook&Fujisawa (2006)

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)      :: self 
real(kind=dbl),INTENT(IN)         :: i1,i2,i3              ! three input notes (in units of semi-tones)
real(kind=dbl)                    :: itt 

real(kind=dbl)                    :: x,y,tmp               ! intervals
real(kind=dbl)                    :: p1,p2,p3(3)           ! auxiliary variables
integer(kind=irg)                 :: i,j,k,ii,jj,istat,s1,s2,s3  ! loop counters etc
integer(kind=irg)                 :: iswap1,iswap(1), np

x = i2-i1
y = i3-i1
np = dble(self%timbre_parameters%num_partials+1)
jj =1 
do i=0,self%timbre_parameters%num_partials
  p1 = self%partials(i)
  do j=0,self%timbre_parameters%num_partials
    p2 = x + self%partials(j)
    do k=0,self%timbre_parameters%num_partials
      p3 = (/p1,p2,y + self%partials(k)/)
! sort p3 from large to small using a simple selection sort
      do ii=1,2
        iswap = maxloc(p3(ii:3))
        iswap1 = iswap(1)+ii-1
        if (iswap1.ne.ii) then 
                tmp = p3(ii)
                p3(ii) = p3(iswap1)
                p3(iswap1) = tmp
        endif
      end do
      self%q3(jj) = self%fits%alpha_tension * (p3(1)-2.D0*p3(2)+p3(3))**2
      jj = jj+1
    end do
  end do
end do
itt = sum(self%prods * dexp(-self%q3)) ! / np

end function triad_tension_

!--------------------------------------------------------------------------
recursive function triad_modality_(self,i1,i2,i3) result(itm)
!DEC$ ATTRIBUTES DLLEXPORT :: triad_modality_
  !! author: MDG
  !! date: 08/05/25
  !!
  !! computes the total modality
  !!
  !! equation 6 in Cook&Fujisawa (2006)

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)      :: self 
real(kind=dbl),INTENT(IN)         :: i1,i2,i3    ! three input notes (in units of semi-tones)
real(kind=dbl)                    :: itm 

real(kind=dbl)                    :: x,y,tmp               ! intervals
real(kind=dbl)                    :: p1,p2,p3(3)           ! auxiliary variables
integer(kind=irg)                 :: i,j,k,ii,jj,istat,s1,s2,s3  ! loop counters etc
integer(kind=irg)                 :: iswap1,iswap(1), np
real(kind=dbl),allocatable        :: q2(:)


x = i2-i1
y = i3-i1
np = dble(self%timbre_parameters%num_partials+1)
allocate( q2( (self%timbre_parameters%num_partials+1)**3 ))

jj =1 
do i=0,self%timbre_parameters%num_partials
  p1 = self%partials(i)
  do j=0,self%timbre_parameters%num_partials
    p2 = x + self%partials(j)
    do k=0,self%timbre_parameters%num_partials
      p3 = (/p1,p2,y + self%partials(k)/)
! sort p3 from large to small using a simple selection sort
      do ii=1,2
        iswap = maxloc(p3(ii:3))
        iswap1 = iswap(1)+ii-1
        if (iswap1.ne.ii) then 
                tmp = p3(ii)
                p3(ii) = p3(iswap1)
                p3(iswap1) = tmp
        endif
      end do
      q2(jj) = self%fits%eps* (p3(1)-2.D0*p3(2)+p3(3))
      self%q3(jj) = 0.25D0 * (p3(1)-2.D0*p3(2)+p3(3))**4
      if (self%q3(jj).gt.100.D0) then 
        self%q3(jj) = 0.D0
      else 
        self%q3(jj) = dexp(-self%q3(jj))
      end if 
      jj = jj+1
    end do
  end do
end do
itm = - sum(self%prods * q2 * self%q3) ! / np

deallocate(q2)

end function triad_modality_

!--------------------------------------------------------------------------
recursive function totalquantity_(self,f1,f2,f3,Q) result(S)
!DEC$ ATTRIBUTES DLLEXPORT :: totalquantity_
  !! author: MDG
  !! date: 08/06/25
  !!
  !! computes the total dissonance/tension/modality

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)      :: self 
real(kind=dbl),INTENT(IN)         :: f1
real(kind=dbl),INTENT(IN)         :: f2
real(kind=dbl),INTENT(IN)         :: f3
character(1),INTENT(IN)           :: Q
real(kind=dbl)                    :: S

integer(kind=irg)                 :: i1, i2, i3, num 
real(kind=dbl)                    :: ff1, ff2, ff3, vv1, vv2, vv3

num = self%timbre_parameters%num_partials

S = 0.D0
do i1=0,num
  ff1 = f1 * dble(i1+1)
  vv1 = self%timbre(i1)
  do i2=0,num
    ff2 = f2 * dble(i2+1)
    vv2 = self%timbre(i2)
    do i3=0,num
      ff3 = f3 * dble(i3+1)
      vv3 = self%timbre(i3)
      if (Q.eq.'D') S = S + self%get_dissonance_(ff1, ff2, ff3, vv1, vv2, vv3)
      if (Q.eq.'T') S = S + self%get_tension_(ff1, ff2, ff3, vv1, vv2, vv3)
      if (Q.eq.'M') S = S + self%get_modality_(ff1, ff2, ff3, vv1, vv2, vv3)
    end do 
  end do 
end do

if (Q.eq.'D') S = S / dble( 2*(num*num-num) )
if (Q.eq.'T') S = S / dble( num )

end function totalquantity_

!--------------------------------------------------------------------------
recursive function get_dissonance_(self,f1,f2,f3,v1,v2,v3) result(D)
!DEC$ ATTRIBUTES DLLEXPORT :: get_dissonance_
  !! author: MDG
  !! date: 08/06/25
  !!
  !! computes the dissonance of a triad of frequencies
  !!
  !! this function returns the dissonance based on the 
  !! relations in W.A. Sethares, J. Acoust. Soc. Am. v. 94 1218 (1993)

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)      :: self
real(kind=dbl),INTENT(IN)         :: f1
real(kind=dbl),INTENT(IN)         :: f2
real(kind=dbl),INTENT(IN)         :: f3
real(kind=dbl),INTENT(IN)         :: v1
real(kind=dbl),INTENT(IN)         :: v2
real(kind=dbl),INTENT(IN)         :: v3
real(kind=dbl)                    :: D

integer(kind=irg)                 :: iswap1,iswap(1),ii
real(kind=dbl)                    :: x, y, z, f(3), v(3), tmp 

f = (/ f1, f2, f3 /)
v = (/ v1, v2, v3 /)

! rank them large to small 
do ii=1,2
  iswap = maxloc(f(ii:3))
  iswap1 = iswap(1)+ii-1
  if (iswap1.ne.ii) then 
    tmp = f(ii)
    f(ii) = f(iswap1)
    f(iswap1) = tmp
    tmp = v(ii)
    v(ii) = v(iswap1)
    v(iswap1) = tmp
  endif
end do

x = (self%fits%freq2int*log10(f(2)/f(3)))**self%fits%gam
y = (self%fits%freq2int*log10(f(1)/f(2)))**self%fits%gam
z = (self%fits%freq2int*log10(f(1)/f(3)))**self%fits%gam

D = 0.D0 
if (x.lt.100.D0) D = D + v(2) * v(3) * ( exp(-self%fits%b1*x) - exp(-self%fits%b2*x) )
if (y.lt.100.D0) D = D + v(1) * v(2) * ( exp(-self%fits%b1*y) - exp(-self%fits%b2*y) )
if (z.lt.100.D0) D = D + v(1) * v(3) * ( exp(-self%fits%b1*z) - exp(-self%fits%b2*z) )
D = D * self%fits%b3

end function get_dissonance_

!--------------------------------------------------------------------------
recursive function get_tension_(self,f1,f2,f3,v1,v2,v3) result(T)
!DEC$ ATTRIBUTES DLLEXPORT :: get_get_tension_dissonance_
  !! author: MDG
  !! date: 08/06/25
  !!
  !! computes the tension of a triad of frequencies
  !!
  !! based on Eq. 1 in Cook and Fujisawa (2006)

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)      :: self
real(kind=dbl),INTENT(IN)         :: f1
real(kind=dbl),INTENT(IN)         :: f2
real(kind=dbl),INTENT(IN)         :: f3
real(kind=dbl),INTENT(IN)         :: v1
real(kind=dbl),INTENT(IN)         :: v2
real(kind=dbl),INTENT(IN)         :: v3
real(kind=dbl)                    :: T

integer(kind=irg)                 :: iswap1,iswap(1),ii
real(kind=dbl)                    :: x, y, q, f(3), v, tmp 

f = (/ f1, f2, f3 /)
v = v1 * v2 * v3

! rank them large to small 
do ii=1,2
  iswap = maxloc(f(ii:3))
  iswap1 = iswap(1)+ii-1
  if (iswap1.ne.ii) then 
    tmp = f(ii)
    f(ii) = f(iswap1)
    f(iswap1) = tmp
  endif
end do

! x = self%fits%freq2int*log10(f(2)/f(1))
! y = self%fits%freq2int*log10(f(3)/f(2))
x = self%fits%freq2int*log10(f(2)/f(3))
y = self%fits%freq2int*log10(f(1)/f(2))

! take the difference, scale by alpha and put in exponential
q = (y-x)**2 * self%fits%alpha_tension
if (q.gt.100.D0) then 
  T = 0.D0 
else 
  T = v * exp(-q)
end if

end function get_tension_

!--------------------------------------------------------------------------
recursive function get_modality_(self,f1,f2,f3,v1,v2,v3) result(M)
!DEC$ ATTRIBUTES DLLEXPORT :: get_modality_
  !! author: MDG
  !! date: 08/06/25
  !!
  !! computes the tension of a triad of frequencies
  !!
  !! based on Cook and Fujisawa (2006)

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)      :: self
real(kind=dbl),INTENT(IN)         :: f1
real(kind=dbl),INTENT(IN)         :: f2
real(kind=dbl),INTENT(IN)         :: f3
real(kind=dbl),INTENT(IN)         :: v1
real(kind=dbl),INTENT(IN)         :: v2
real(kind=dbl),INTENT(IN)         :: v3
real(kind=dbl)                    :: M

integer(kind=irg)                 :: iswap1,iswap(1),ii
real(kind=dbl)                    :: x, y, q, f(3), v, tmp

f = (/ f1, f2, f3 /)
v = v1 * v2 * v3

! rank them large to small 
do ii=1,2
  iswap = maxloc(f(ii:3))
  iswap1 = iswap(1)+ii-1
  if (iswap1.ne.ii) then 
    tmp = f(ii)
    f(ii) = f(iswap1)
    f(iswap1) = tmp
  endif
end do

x = self%fits%freq2int*log10(f(1)/f(2))
y = self%fits%freq2int*log10(f(2)/f(3))

! take the difference, scale by alpha and put in exponential
q = 0.25D0 * (y-x)**4
if (q.gt.100.D0) then 
  M = 0.D0 
else 
  ! M = - v * (y-x) * exp(-q) * self%fits%eps
  M =   v * (y-x) * exp(-q) * self%fits%eps
end if

end function get_modality_

!--------------------------------------------------------------------------
recursive subroutine triad_(self, progname, progdesc)
!DEC$ ATTRIBUTES DLLEXPORT :: triad_
  !! author: MDG
  !! date: 08/05/25
  !!
  !! computes the dissonance, tension, and modality for the basic
  !! triad chords and their inversions
  !!

use mod_io 

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)      :: self 
character(fnlen),INTENT(IN)       :: progname
character(fnlen),INTENT(IN)       :: progdesc

type(IO_T)                        :: Message 

real(kind=dbl)                    :: io_real(self%timbre_parameters%num_partials+1), freq(0:20)
integer(kind=irg)                 :: io_int(1), i

real(kind=dbl)                    :: tension(3), dissonance(3), modality(3), instability(3) 


call Message%printMessage(' ')
call Message%printMessage(' Program Name       : '//trim(progname))
call Message%printMessage(' Program Descriptor : '//trim(progdesc))
call Message%printMessage(' ')

io_int(1) = self%timbre_parameters%num_partials
call Message%WriteValue(' Number of partials   : ', io_int, 1, frm="(I3)")
if (self%timbre_parameters%timbre_type.eq.1) then 
  io_real(1) = self%timbre_parameters%base
  call Message%WriteValue(' Timbre type base^n with base = ', io_real, 1, frm="(F6.3)")
else
  call Message%printMessage(' Timbre type 1/n') 
end if 
io_real = self%timbre(0:self%timbre_parameters%num_partials)
call Message%WriteValue(' Timbre values = ', io_real, self%timbre_parameters%num_partials+1, frm="(20(F6.3,' '))")
io_real = self%partials(0:self%timbre_parameters%num_partials)
call Message%WriteValue(' Partial values = ', io_real, self%timbre_parameters%num_partials+1, frm="(20(F6.3,' '))")
call Message%printMessage(' ')


! determine all the frequencies needed for these computations
do i=0,20
  freq(i) = self%timbre_parameters%f1 * 10**(dble(i)/self%fits%freq2int)
end do

call Message%printMessage( (/ ' Major Chords ', & 
                              ' ------------ ', &
                              '              ' /) )

do i=0,2
  dissonance(i+1) = self%totalquantity( freq(self%TC%major(1+3*i)), freq(self%TC%major(2+3*i)), freq(self%TC%major(3+3*i)), 'D' )
  tension(i+1) = self%totalquantity( freq(self%TC%major(1+3*i)), freq(self%TC%major(2+3*i)), freq(self%TC%major(3+3*i)), 'T' )
  modality(i+1) = self%totalquantity( freq(self%TC%major(1+3*i)), freq(self%TC%major(2+3*i)), freq(self%TC%major(3+3*i)), 'M' )
end do
instability = dissonance + self%fits%delta * tension 

io_real(1:3) = dissonance
call Message%printMessage('                R        1        2')
call Message%WriteValue(' Dissonance  : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = tension
call Message%WriteValue(' Tension     : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = instability
call Message%WriteValue(' Instability : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = modality
call Message%WriteValue(' Modality    : ', io_real, 3, frm="(3(F8.3,' '))")


call Message%printMessage(' ')
call Message%printMessage( (/ ' Minor Chords ', & 
                              ' ------------ ', &
                              '              ' /) )

do i=0,2
  dissonance(i+1) = self%totalquantity( freq(self%TC%minor(1+3*i)), freq(self%TC%minor(2+3*i)), freq(self%TC%minor(3+3*i)), 'D' )
  tension(i+1) = self%totalquantity( freq(self%TC%minor(1+3*i)), freq(self%TC%minor(2+3*i)), freq(self%TC%minor(3+3*i)), 'T' )
  modality(i+1) = self%totalquantity( freq(self%TC%minor(1+3*i)), freq(self%TC%minor(2+3*i)), freq(self%TC%minor(3+3*i)), 'M' )
end do
instability = dissonance + self%fits%delta * tension 

io_real(1:3) = dissonance
call Message%printMessage('                R        1        2')
call Message%WriteValue(' Dissonance  : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = tension
call Message%WriteValue(' Tension     : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = instability
call Message%WriteValue(' Instability : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = modality
call Message%WriteValue(' Modality    : ', io_real, 3, frm="(3(F8.3,' '))")



call Message%printMessage(' ')
call Message%printMessage( (/ ' Augmented Chords ', & 
                              ' ---------------- ', &
                              '                  ' /) )

do i=0,2
  dissonance(i+1) = self%totalquantity( freq(self%TC%augmented(1+3*i)), freq(self%TC%augmented(2+3*i)), &
                                        freq(self%TC%augmented(3+3*i)), 'D' )
  tension(i+1) =    self%totalquantity( freq(self%TC%augmented(1+3*i)), freq(self%TC%augmented(2+3*i)), &
                                        freq(self%TC%augmented(3+3*i)), 'T' )
  modality(i+1) =   self%totalquantity( freq(self%TC%augmented(1+3*i)), freq(self%TC%augmented(2+3*i)), &
                                        freq(self%TC%augmented(3+3*i)), 'M' )
end do
instability = dissonance + self%fits%delta * tension 

io_real(1:3) = dissonance
call Message%printMessage('                R        1        2')
call Message%WriteValue(' Dissonance  : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = tension
call Message%WriteValue(' Tension     : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = instability
call Message%WriteValue(' Instability : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = modality
call Message%WriteValue(' Modality    : ', io_real, 3, frm="(3(F8.3,' '))")



call Message%printMessage(' ')
call Message%printMessage( (/ ' Diminished Chords ', & 
                              ' ----------------- ', &
                              '                   ' /) )

do i=0,2
  dissonance(i+1) = self%totalquantity( freq(self%TC%diminished(1+3*i)), freq(self%TC%diminished(2+3*i)), &
                                        freq(self%TC%diminished(3+3*i)), 'D' )
  tension(i+1) =    self%totalquantity( freq(self%TC%diminished(1+3*i)), freq(self%TC%diminished(2+3*i)), &
                                        freq(self%TC%diminished(3+3*i)), 'T' )
  modality(i+1) =   self%totalquantity( freq(self%TC%diminished(1+3*i)), freq(self%TC%diminished(2+3*i)), &
                                        freq(self%TC%diminished(3+3*i)), 'M' )
end do
instability = dissonance + self%fits%delta * tension 

io_real(1:3) = dissonance
call Message%printMessage('                R        1        2')
call Message%WriteValue(' Dissonance  : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = tension
call Message%WriteValue(' Tension     : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = instability
call Message%WriteValue(' Instability : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = modality
call Message%WriteValue(' Modality    : ', io_real, 3, frm="(3(F8.3,' '))")

call Message%printMessage(' ')
call Message%printMessage( (/ ' Suspended 4th Chords ', & 
                              ' -------------------- ', &
                              '                      ' /) )


do i=0,2
  dissonance(i+1) = self%totalquantity( freq(self%TC%suspended(1+3*i)), freq(self%TC%suspended(2+3*i)), &
                                        freq(self%TC%suspended(3+3*i)), 'D' )
  tension(i+1) =    self%totalquantity( freq(self%TC%suspended(1+3*i)), freq(self%TC%suspended(2+3*i)), &
                                        freq(self%TC%suspended(3+3*i)), 'T' )
  modality(i+1) =   self%totalquantity( freq(self%TC%suspended(1+3*i)), freq(self%TC%suspended(2+3*i)), &
                                        freq(self%TC%suspended(3+3*i)), 'M' )
end do
instability = dissonance + self%fits%delta * tension 

io_real(1:3) = dissonance
call Message%printMessage('                R        1        2')
call Message%WriteValue(' Dissonance  : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = tension
call Message%WriteValue(' Tension     : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = instability
call Message%WriteValue(' Instability : ', io_real, 3, frm="(3(F8.3,' '))")
io_real(1:3) = modality
call Message%WriteValue(' Modality    : ', io_real, 3, frm="(3(F8.3,' '))")

end subroutine triad_

!--------------------------------------------------------------------------
subroutine makeGrid_(self, xmax, ymax, im, scl, range, bg)
!DEC$ ATTRIBUTES DLLEXPORT :: makeGrid_
!! author: MDG 
!! version: 1.0 
!! date: 08/07/25
!!
!! add a hexagonal grid to an image 

IMPLICIT NONE 

class(Triad_T), INTENT(INOUT)             :: self
integer(kind=irg), INTENT(IN)             :: xmax
integer(kind=irg), INTENT(IN)             :: ymax
real(kind=dbl), INTENT(INOUT)             :: im(xmax, ymax)
integer(kind=irg),INTENT(IN)              :: scl
integer(kind=irg),INTENT(IN)              :: range
character(1),INTENT(IN)                   :: bg

real(kind=dbl)                            :: ff, x0, x1, y0, y1, c, bgcolor
integer(kind=irg)                         :: i, j, localscl

ff = sin(cPi/3.D0)/2.D0 
c = -1.0D0

localscl = scl*2 

! horizontal grid lines
do i=1,range/2 
  x0 = dble(xmax-1)/4.D0-dble(i*localscl)/4.D0
  x1 = minval( (/ 3.D0*dble(xmax-1)/4.D0+dble(i*localscl)/4.D0, dble(xmax-1) /) )
  y0 = dble(i*localscl)*ff
  y1 = y0
  call DrawLine(im, xmax, ymax, x0, y0, x1, y1, c)
end do  

do i=1,range/2-1 
  x0 = dble(xmax-1)/4.D0-dble(i*localscl)/4.D0
  x1 = minval( (/ 3.D0*dble(xmax-1)/4.D0+dble(i*localscl)/4.D0, dble(xmax-1) /) )
  y0 = dble(ymax-1)-dble(i*localscl)*ff
  y1 = y0
  call DrawLine(im, xmax, ymax, x0, y0, x1, y1, c)
end do  

do i=0,range 
  if (i.le.range/2) then 
    x0 = dble(i*localscl)/4.D0
    x1 = dble(i*localscl)/2.D0 + dble(xmax-1)/4.D0
    y0 = maxval( (/ dble(ymax-1)/2.D0-dble(i*localscl)*ff, 0.D0 /) )
    y1 = dble(ymax-1)
  else
    x0 = dble(range*scl)/4.D0 + dble((i-range/2)*localscl)/2.D0
    x1 = 3.D0*dble(range*scl)/4.D0 + dble((i-range/2)*localscl)/4.D0
    y0 = 0.D0
    y1 = dble(ymax-1)-dble((i-range/2)*localscl)*ff
  end if 
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
bgcolor = 0.D0
if (bg.eq.'w') bgcolor = -100.D0 
do j=0,ymax/2
  im( 0:int(dble(xmax-1)/4.D0 - dble(j)*(dble(xmax)/dble(2*ymax)) ), j) = bgcolor
  im( int(3.D0*dble(xmax-1)/4.D0 + dble(j)*(dble(xmax)/dble(2*ymax)) ):xmax-1, j) = bgcolor
  im( 0:int(dble(xmax-1)/4.D0 - dble(j)*(dble(xmax)/dble(2*ymax)) ), ymax-1-j) = bgcolor
  im( int(3.D0*dble(xmax-1)/4.D0 + dble(j)*(dble(xmax)/dble(2*ymax)) ):xmax-1, ymax-1-j) = bgcolor
end do

end subroutine makeGrid_

!--------------------------------------------------------------------------
subroutine saveColorMap_(self, xmax, ymax, ima, grid, fname)
!DEC$ ATTRIBUTES DLLEXPORT :: saveColorMap_
!! author: MDG 
!! version: 1.0 
!! date: 08/07/25
!!
!! add a hexagonal grid to an image 

use mod_image
use mod_io
use ISO_C_BINDING
use, intrinsic :: iso_fortran_env


IMPLICIT NONE 

class(Triad_T), INTENT(INOUT)             :: self
integer(kind=irg), INTENT(IN)             :: xmax
integer(kind=irg), INTENT(IN)             :: ymax
real(kind=dbl), INTENT(INOUT)             :: ima(xmax, ymax)
real(kind=dbl), INTENT(INOUT)             :: grid(xmax, ymax)
character(fnlen), INTENT(IN)              :: fname

type(IO_T)                                :: Message 

character(fnlen)                          :: TIFF_filename 
integer(int8),allocatable                 :: colormap(:,:,:)
integer(kind=irg)                         :: iDD, cr, i, j, k
integer(int8)                             :: R(0:255), G(0:255), B(0:255) 
real(kind=dbl)                            :: mi, ma, crad, cradsq, cx, cy

! declare variables for use in object oriented image module
integer                                   :: iostat, io_int(2)
character(len=128)                        :: iomsg
logical                                   :: isInteger, OPC, PUC
type(image_t)                             :: im
integer                                   :: dim2(2), Pm
integer(c_int32_t)                        :: result


call self%getclrs(R,G,B)
allocate( colormap(3, xmax, ymax) )

! start with the dissonance map 
mi = minval(ima)
ma = maxval(ima) 
ima = 255.D0 * ((ima-mi)/(ma-mi))
do i=1,xmax
  do j=1,ymax
    iDD = nint(ima(i,j))
    colormap(1:3,i,j) = (/ R(iDD), G(iDD), B(iDD) /)
  end do 
end do 

! add the grid
do j=1,xmax
  do k=1,ymax
    if (Grid(j,k).eq.-100.D0) then 
      colormap(1:3,j,k) = -1_int8
    else
      do i=1,3
        colormap(i,j,k) = colormap(i,j,k) * Grid(j,k)
      end do 
    end if 
  end do 
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

TIFF_filename = trim(fname)
! set up the image_t structure
im = image_t(colormap)
im%dims = (/ 3, xmax, ymax /)
im%samplesPerPixel = 3
im%unsigned = .TRUE.
if(im%empty()) call Message%printMessage("saveColorMap_: failed to convert array to rgb image")

! create the file
call im%write(trim(TIFF_filename), iostat, iomsg) ! format automatically detected from extension
if(0.ne.iostat) then
  call Message%printMessage(" Failed to write image to file : "//iomsg)
else
  call Message%printMessage(' Color map written to '//trim(TIFF_filename),"(A)")
end if

end subroutine saveColorMap_

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


end module
