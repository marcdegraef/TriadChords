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

module mod_math
  !! author: MDG
  !! date: 08/12/25
  !!
  !! All math routines
  !!

use mod_kinds
use mod_global

contains

!--------------------------------------------------------------------------
function sfit( dimx, dimy, inp, degree ) result(bg)
!DEC$ ATTRIBUTES DLLEXPORT :: sfit
  !! author: MDG
  !! version: 1.0
  !! date: 08/12/25
  !!
  !! simply fit of degree "degree" to a 2D data set inp
  !! loosely based on IDL sfit routine

IMPLICIT NONE

integer(kind=irg),INTENT(IN)      :: dimx
integer(kind=irg),INTENT(IN)      :: dimy
real(kind=dbl),INTENT(IN)         :: inp(dimx,dimy)
integer(kind=irg),INTENT(IN)      :: degree
real(kind=dbl)                    :: bg(dimx,dimy)
  
integer(kind=irg)                 :: n2, m, i, j, k, l, info
real(kind=dbl)                    :: x(dimx,dimy), y(dimx,dimy), xline(dimx), yline(dimy), &
                                     kx( (degree+1)**2 )
real(kind=dbl),allocatable        :: ut(:,:), kk(:,:), tmp(:,:)

n2 = (degree+1)**2
m = dimx * dimy

xline = (/ (dble(i), i=1,dimx) /)
yline = (/ (dble(j), j=1,dimy) /)

do j=1,dimy
  x(1:dimx,j) = xline
end do
do i=1,dimx
  y(i,1:dimy) = yline
end do

allocate( ut(n2, m), kk(n2, m), tmp(n2, n2) )
k=1
do i=0,degree 
  do j=0,degree
    ut(k,1:m) = reshape( x**i * y**j, (/ m /) )
    k = k+1
  end do 
end do

tmp = matmul( ut, transpose(ut) )
call invert(tmp,n2,info) ! in-place matrix inversion
if (info.ne.0) then 
  write (*,*) 'sfit matrix inversion failed '
  stop
end if
kk = matmul(tmp, ut)
kx = matmul(kk,reshape( inp, (/ m /) ) )

bg = reshape( matmul(kx,ut), (/ dimx, dimy /) )

end function sfit

!--------------------------------------------------------------------------
subroutine invert(A, M, info)
    ! Invert a non-symmetric double precision MxM matrix using
    ! Gauss-Jordan elimination with partial pivoting.
    ! No LAPACK is used.
implicit none
integer(kind=irg), INTENT(IN)         :: M
real(kind=dbl), INTENT(INOUT)         :: A(M, M)
integer(kind=irg), INTENT(OUT)        :: info   ! 0 = success, >0 = singular matrix

real(kind=dbl)                        :: aug(M, 2*M)
real(kind=dbl)                        :: factor, temp(2*M)
integer(kind=irg)                     :: i, j, k, pivot
real(kind=dbl)                        :: maxval

! Initialize augmented matrix: [A | I]
aug(:, 1:M) = A
aug(:, M+1:2*M) = 0.0d0
do i = 1, M
    aug(i, M+i) = 1.0d0
end do

info = 0

! Forward elimination
do i = 1, M
    ! Partial pivoting: find pivot row
    pivot = i
    maxval = abs(aug(i, i))
    do k = i+1, M
        if (abs(aug(k, i)) > maxval) then
            maxval = abs(aug(k, i))
            pivot = k
        end if
    end do

    ! Check for singularity
    if (maxval == 0.0d0) then
        info = i
        return
    end if

    ! Swap rows if needed
    if (pivot /= i) then
        temp = aug(i, :)
        aug(i, :) = aug(pivot, :)
        aug(pivot, :) = temp
    end if

    ! Normalize pivot row
    factor = aug(i, i)
    aug(i, :) = aug(i, :) / factor

    ! Eliminate below pivot
    do j = i+1, M
        factor = aug(j, i)
        aug(j, :) = aug(j, :) - factor * aug(i, :)
    end do
end do

! Backward elimination
do i = M, 1, -1
    do j = i-1, 1, -1
        factor = aug(j, i)
        aug(j, :) = aug(j, :) - factor * aug(i, :)
    end do
end do

! Extract inverse matrix
A = aug(:, M+1:2*M)

end subroutine invert



end module mod_math
