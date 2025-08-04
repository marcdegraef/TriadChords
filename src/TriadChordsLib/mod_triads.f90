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
use mod_parameters

! parameters that have to do with musical quantities


type, public :: Triad_T
  private 
    character(fnlen)            :: sourcepath
    type(fit_parameters)        :: fits    
    type(timbre_descriptor)     :: timbre_parameters

! then a few allocatable arrays
    real(kind=dbl),allocatable  :: timbre(:)        ! timbre amplitudes
    real(kind=dbl),allocatable  :: partials(:)      ! intervals for all upper partials 
    real(kind=dbl),allocatable  :: prods2(:)        ! precomputed pairwise products of timbre amplitudes
    real(kind=dbl),allocatable  :: prods(:)         ! precomputed triple products of timbre amplitudes
    real(kind=dbl),allocatable  :: q2(:),q3(:)      ! auxiliary arrays

contains
  private

    procedure, pass(self) :: interval_totaldissonance_
    procedure, pass(self) :: interval_totaltension_
    procedure, pass(self) :: setbase_
    procedure, pass(self) :: getbase_
    procedure, pass(self) :: settimbre_type_
    procedure, pass(self) :: gettimbre_type_
    procedure, pass(self) :: setnum_partials_
    procedure, pass(self) :: getnum_partials_
    procedure, pass(self) :: setsourcepath_
    procedure, pass(self) :: getsourcepath_

    generic, public :: interval_totaldissonance => interval_totaldissonance_
    generic, public :: interval_totaltension => interval_totaltension_
    generic, public :: setbase => setbase_
    generic, public :: getbase => getbase_
    generic, public :: settimbre_type => settimbre_type_
    generic, public :: gettimbre_type => gettimbre_type_
    generic, public :: setnum_partials => setnum_partials_
    generic, public :: getnum_partials => getnum_partials_
    generic, public :: setsourcepath => setsourcepath_
    generic, public :: getsourcepath => getsourcepath_

end type Triad_T

interface Triad_T 
  module procedure Triad_constructor
end interface Triad_T 

contains

!--------------------------------------------------------------------------
type(Triad_T) function Triad_constructor( no_alloc  ) result(Triad)
!DEC$ ATTRIBUTES DLLEXPORT :: Triad_constructor
  !! author: MDG
  !! date: 10/22/08
  !!
  !! constructor for the Triad_T class

use mod_io 

IMPLICIT NONE

logical,INTENT(IN),OPTIONAL       :: no_alloc

type(IO_T)                        :: Message 

integer(kind=irg)                 :: i,j,k,i1,i2,istat         ! loop parameters
character(fnlen)                  :: ConfigFile = 'TriadConfig.txt'
logical                           :: fexists, alloc

integer(kind=irg)                 :: num_partials, timbre_type
real(kind=dbl)                    :: base 
character(fnlen)                  :: sourcepath

namelist /TriadConfig/ num_partials, timbre_type, base, sourcepath 

! use a shorthand notation
associate(Tt => Triad%timbre_parameters)

! make sure the TriadConfig.txt file is present in this folder; if it is not, 
! then create it and send a message to the user. If it does exist, read it
! and populate the class parameters.
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
                                "! full path to TriadChords source folder ", &
                                " sourcepath = 'undefined',               ", &
                                " /                                       " /), redirect = dataunit )
  close(dataunit, status = 'keep')
  call Message%printMessage(' Configuration file did not exist... created TriadConfig.txt in current folder.')
  call Message%printError('Triad_constructor',' aborting run; please edit TriadConfig.txt file')
end if

alloc = .TRUE.
if (present(no_alloc)) then 
  if (no_alloc.eqv..TRUE.) then 
    alloc = .FALSE.
  end if
end if 

if (alloc.eqv..TRUE.) then
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
    Triad%partials(i) = Triad%fits%freq2int * dlog10(dble(i+1)/dble(i)) + Triad%partials(i-1)
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
recursive function interval_totaldissonance_(self, x) result(itd)
!DEC$ ATTRIBUTES DLLEXPORT :: interval_totaldissonance_
  !! author: MDG
  !! date: 10/22/08
  !!
  !! computes the total dissonance for a pair of notes taking into account the upper partials.
  !!
  !! equation 3-4 in Cook&Fujisawa (2006)

IMPLICIT NONE

class(Triad_T),INTENT(INOUT)          :: self
real(kind=dbl),INTENT(IN)             :: x                ! the input interval (in units of semitones)
real(kind=dbl)                        :: itd 

real(kind=dbl)                        :: d                ! axiliary variable
integer(kind=irg)                     :: i,j,i1,istat     ! loop counters, etc...

i1 =1 
do i=0,self%timbre_parameters%num_partials
  do j=0,self%timbre_parameters%num_partials
    self%q2(i1) = dabs(self%partials(i) - (x+self%partials(j)))**self%fits%gam
    i1 = i1+1
  end do
end do
d = sum(self%prods2 * (dexp(-self%fits%b1*self%q2)-dexp(-self%fits%b2*self%q2)))
itd = self%fits%b3 * d

end function interval_totaldissonance_

!--------------------------------------------------------------------------
recursive function interval_totaltension_(self,i1,i2,i3) result(itt)
!DEC$ ATTRIBUTES DLLEXPORT :: interval_totaldissonance_
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
real(kind=dbl)                    :: p1,p2,p3(3)           ! axuiliary variables
integer(kind=irg)                 :: i,j,k,ii,jj,istat,s1,s2,s3  ! loop counters etc
integer(kind=irg)                 :: iswap1,iswap(1)

x = i2-i1
y = i3-i1

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
itt = sum(self%prods * dexp(-self%q3))

end function interval_totaltension_

end module
