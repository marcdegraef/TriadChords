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

module mod_postscript
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !! last revision: 08/12/25
  !!
  !! A collection of postscript output routines used to create a variety of graphics output
  !!
  !! This class contains a number of Postscript generating subroutines
  !! which can be used to generate PS-drawings from a Fortran program.
  !! The routines are based on the Pascal version written by J. Fransaer
  !! of the Catholic University of Leuven, Belgium.
  !! Translated into Fortran-90 by M. De Graef on 12/31/92.
  !!
  !! A typical program would start as follows\n
  !!
  !!       call self%openfile(scale)\n
  !!       call self%drawframe(...)\n
  !!       ....
  !!
  !! and end with \n
  !!
  !!       call self%closefile\n
  !!
  !! The code to draw spheres (sp) is taken from the appendix of
  !! Earl J. Kirklands book on Advanced Computing in Electron Microscopy,
  !! and slightly modified to include color.
  !!
  !! All dimensions are in inches
  !! pspage      = pagenumber for multi-page files
  !! psfigwidth  = width of drawing (pagewidth - 2 inches for margins = 6.5)
  !! psfigheight = height of drawing (pageheight - 2 = 9.0)
  !! psscale     = scale factor for overall file
  !! psname      = PostScript file name
  !! psdash      = used to store a dash pattern
  !! fonts       = array with standard PostScript fonts (more may be added)


use mod_kinds
use mod_global

IMPLICIT NONE

private

! the following postscript preamble is inspired on the one from the old EMS package
! (before it was converted to JEMS) written by Pierre Stadelmann
character(55),parameter :: PSpreamble(23) = (/ &
        "%!PS-Adobe-3.0                                         ", &
        "%%Creator:                                             ", &
        "%%Title:                                               ", &
        "%%Pages: (atend)                                       ", &
        "%%EndComments                                          ", &
        "/M {moveto} def /N {newpath} def /L {lineto} def       ", &
        "/S {stroke} def /T {translate} def /R {rotate} def     ", &
        "/F {fill} def /Cl {closepath} def                      ", &
        "/circle {N 0 0 1 0 360 arc Cl F} def                   ", &
        "/sp { gsave T scale 1.0 -0.04 0 { pop 3 array astore   ", &
        "{1.02 mul} forall 3 copy setrgbcolor -0.025 0.030 T    ", &
        "circle 0.93 0.93 scale } for                           ", &
        "pop pop pop grestore } def                             ", &
        "/frame {1.0 setgray N left rad add bottom M            ", &
        "right bottom right top rad arcto L right top left top  ", &
        "rad arcto L left top left bottom rad arcto L left      ", &
        "bottom right bottom rad arcto L Cl F 0.0 setgray N     ", &
        "left rad add bottom M right bottom right top rad       ", &
        "arcto L right top left top rad arcto L left top left   ", &
        "bottom rad arcto L left bottom right bottom rad arcto  ", &
        "L Cl S } def                                           ", &
        "%%EndProlog                                            ", &
        "72 dup scale                                           " /)
!DEC$ ATTRIBUTES DLLEXPORT :: PSpreamble


! font-related stuff
character(25),public,parameter :: PSlbl = "Written by MDG, 2001-2025"
character(20),public,parameter :: PSfonts(5) = (/"Symbol              ", &
                                          "Times-Bold          ", &
                                          "Times-BoldItalic    ", &
                                          "Times-Italic        ", &
                                          "Times-Roman         "/)
!DEC$ ATTRIBUTES DLLEXPORT :: PSlbl
!DEC$ ATTRIBUTES DLLEXPORT :: PSfonts


  type, public :: PostScript_T
     integer(kind=irg)      :: pspage
     integer(kind=irg)      :: psunit = 20
     integer(kind=irg)      :: imanum
     real(kind=sgl)         :: psdash(20)
     real(kind=sgl)         :: psfigwidth
     real(kind=sgl)         :: psfigheight
     real(kind=sgl)         :: psscale
     character(fnlen)       :: psname

   contains
    private
      procedure, pass(self) :: openFile_
      procedure, pass(self) :: closeFile_
      procedure, pass(self) :: newpage_
      procedure, pass(self) :: setpspage_
      procedure, pass(self) :: clippath_
      procedure, pass(self) :: translate_
      procedure, pass(self) :: move_
      procedure, pass(self) :: draw_
      procedure, pass(self) :: line_gray_
      procedure, pass(self) :: setlinewidth_
      procedure, pass(self) :: setlinecolor_
      procedure, pass(self) :: square_
      procedure, pass(self) :: filledsquare_
      procedure, pass(self) :: cross_
      procedure, pass(self) :: sphere_
      procedure, pass(self) :: arc_
      procedure, pass(self) :: circle_
      procedure, pass(self) :: filledcircle_
      procedure, pass(self) :: drawframe_
      procedure, pass(self) :: drawrect_
      procedure, pass(self) :: line_
      procedure, pass(self) :: setdash_
      procedure, pass(self) :: closepathS_
      procedure, pass(self) :: stroke_
      procedure, pass(self) :: gsave_
      procedure, pass(self) :: grestore_
      procedure, pass(self) :: closepath_
      procedure, pass(self) :: newpath_
      procedure, pass(self) :: text_
      procedure, pass(self) :: textv_
      procedure, pass(self) :: texttitle_
      procedure, pass(self) :: textvtitle_
      procedure, pass(self) :: textint_
      procedure, pass(self) :: textvar_
      procedure, pass(self) :: textvardbl_
      procedure, pass(self) :: textballoon_
      procedure, pass(self) :: balloon_
      procedure, pass(self) :: setfont_
      procedure, pass(self) :: getpsunit_
      procedure, pass(self) :: getpsscale_
      procedure, pass(self) :: getpsfigwidth_
      procedure, pass(self) :: getpsfigheight_
      final :: PS_destructor

      generic, public :: openFile => openFile_
      generic, public :: closeFile => closeFile_
      generic, public :: newpage => newpage_
      generic, public :: setpspage => setpspage_
      generic, public :: clippath => clippath_
      generic, public :: translate => translate_
      generic, public :: move => move_
      generic, public :: draw => draw_
      generic, public :: line_gray => line_gray_
      generic, public :: setlinewidth => setlinewidth_
      generic, public :: setlinecolor => setlinecolor_
      generic, public :: square => square_
      generic, public :: filledsquare => filledsquare_
      generic, public :: cross => cross_
      generic, public :: sphere => sphere_
      generic, public :: arc => arc_
      generic, public :: circle => circle_
      generic, public :: filledcircle => filledcircle_
      generic, public :: drawframe => drawframe_
      generic, public :: drawrect => drawrect_
      generic, public :: line => line_
      generic, public :: setdash => setdash_
      generic, public :: closepathS => closepathS_
      generic, public :: stroke => stroke_
      generic, public :: gsave => gsave_
      generic, public :: grestore => grestore_
      generic, public :: closepath => closepath_
      generic, public :: newpath => newpath_
      generic, public :: text => text_
      generic, public :: textv => textv_
      generic, public :: texttitle => texttitle_
      generic, public :: textvtitle => textvtitle_
      generic, public :: textint => textint_
      generic, public :: textvar => textvar_
      generic, public :: textvardbl => textvardbl_
      generic, public :: textballoon => textballoon_
      generic, public :: balloon => balloon_
      generic, public :: setfont => setfont_
      generic, public :: getpsunit => getpsunit_
      generic, public :: getpsscale => getpsscale_
      generic, public :: getpsfigwidth => getpsfigwidth_
      generic, public :: getpsfigheight => getpsfigheight_

  end type PostScript_T

  ! the constructor routine for this class
  interface PostScript_T
    module procedure :: PS_constructor
  end interface PostScript_T

contains

!--------------------------------------------------------------------------
!--------------------------------------------------------------------------
! we begin with the functions/subroutines that are public in this class
!--------------------------------------------------------------------------
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
type(PostScript_T) function PS_constructor( progdesc, imanum, dontask, psname ) result(PS)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_constructor
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! constructor for the PostScript_T Class


IMPLICIT NONE

character(fnlen), INTENT(IN)          :: progdesc
integer(kind=irg), INTENT(IN)         :: imanum
logical,INTENT(IN),optional           :: dontask
character(fnlen),INTENT(IN),optional  :: psname

  PS%imanum = imanum
  if (present(psname)) PS%psname = trim(psname)

  if (present(dontask)) then 
    call PS%openfile(progdesc, dontask=.TRUE.)
  else
    call PS%openfile(progdesc)
  end if 

end function PS_constructor

!--------------------------------------------------------------------------
subroutine PS_destructor(self)
!DEC$ ATTRIBUTES DLLEXPORT :: PS_destructor
!! author: MDG
!! version: 1.0
!! date: 02/02/20
!!
!! destructor for the PostScript_T Class

IMPLICIT NONE

type(PostScript_T), INTENT(INOUT)         :: self

end subroutine PS_destructor

!--------------------------------------------------------------------------
recursive subroutine openfile_(self, progdesc, dontask)
!DEC$ ATTRIBUTES DLLEXPORT :: openFile_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! open postscript file and dump the preamble to the file

use mod_io

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
character(fnlen),INTENT(IN)           :: progdesc
logical,INTENT(IN),optional           :: dontask

type(IO_T)                            :: Message

real(kind=sgl)                        :: fw, fh        !< page format parameters
integer(kind=irg)                     :: i            !< loop counter
character(fnlen)                      :: gname

! define the writeable portion of the page (should be made more user-friendly by adding A4 format...)
 self%psfigwidth=6.5
 self%psfigheight=9.0
 self%psscale=1.0

! open file and dump Prolog and Comments sections
 if (present(dontask)) then
! don't ask for a file name
   open(unit=self%psunit,file=trim(self%psname),status='unknown',action='write',form='formatted')
   call Message%printMessage('Opening temporary file for PostScript output', frm = "(A)")
 else
! do ask for a file name
   call Message%ReadValue(' Enter Postscript file name : ', gname,"(A)")
   self%psname = trim(gname)
   open(unit=self%psunit,file=trim(self%psname),status='unknown',form='formatted')
 end if

! write the preamble
 write (self%psunit,"(A)") PSpreamble(1)
 write (self%psunit,"(A,' ',A)") trim(PSpreamble(2)), 'TriadChords Package'
 write (self%psunit,"(A,' ',A)") trim(PSpreamble(3)), trim(progdesc)
 do i=4,23
  write (self%psunit,"(A)") PSpreamble(i)
 end do

! determine lower left corner and translate to that point
 fw=0.5*(8.50-self%psscale*self%psfigwidth)
 fh=0.5*(11.0-self%psscale*self%psfigheight)
 write (self%psunit,"(F12.7,' ',F12.7,' T')") fw,fh
 write (self%psunit,"(F12.7,' setlinewidth')") 0.01
 write (self%psunit,"(F12.7,' ',F12.7,' scale')") self%psscale,self%psscale

! set page number counter to zero
 self%pspage = 0

end subroutine openfile_

!--------------------------------------------------------------------------
recursive subroutine closeFile_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: closeFile_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! close and save postscript file

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)        :: self

! write the trailer to the file
 write (self%psunit,*) 'showpage'
 write (self%psunit,"(' %%Pages: ',i3)") self%pspage
 write (self%psunit,"(' %%EOF')")

! and close it
  close(unit=self%psunit,status='keep')

end subroutine closeFile_

!--------------------------------------------------------------------------
recursive subroutine newpage_(self, frm, btxt)
!DEC$ ATTRIBUTES DLLEXPORT :: newpage_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! start a new page in the PS file

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
logical,INTENT(IN)                    :: frm
character(*),INTENT(IN)               :: btxt

 if (self%pspage.ne.0) then
  write (self%psunit,*) 'showpage saveobj restore'
 end if

! update the page counter
 self%pspage = self%pspage + 1
 write (self%psunit,"(' %%Page: ',i3,i3)") self%pspage-1,self%pspage
 write (self%psunit,*) '/saveobj save def'

! prepare to draw a header balloon
 call self%setfont(PSfonts(3),0.18)
 write (self%psunit,"(1x,F12.7,' ',F12.7,' M (',I8,') show')") 6.75,self%psfigheight-0.2,self%pspage
 if (frm.eqv..TRUE.) then  ! we need a frame
  call self%drawframe(6.75,self%psfigheight)
 endif

! output the text balloon
 call self%setlinewidth(0.012)
 call self%textballoon(2.0,9.2,btxt,PSfonts(2),0.25)
 call self%setfont(PSfonts(5),0.07)
 call self%text(0.1,-0.1,PSlbl)

end subroutine newpage_

!--------------------------------------------------------------------------
recursive subroutine setpspage_(self, pgnum)
!DEC$ ATTRIBUTES DLLEXPORT :: setpspage_
  !! author: MDG
  !! version: 1.0
  !! date: 01/27/20
  !!
  !! set a page number

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)   :: self
integer(kind=irg), INTENT(IN)       :: pgnum

self%pspage = pgnum

end subroutine setpspage_

!--------------------------------------------------------------------------
recursive function getpsunit_(self) result(psu)
!DEC$ ATTRIBUTES DLLEXPORT :: getpsunit_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! get the postscript output device id

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)  :: self
integer(kind=irg)                  :: psu

psu = self%psunit

end function getpsunit_

!--------------------------------------------------------------------------
recursive function getpsscale_(self) result(psscale)
!DEC$ ATTRIBUTES DLLEXPORT :: getpsscale_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! make the last path the clippath

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)      :: self
real(kind=sgl)                     :: psscale

psscale = self%psscale

end function getpsscale_

!--------------------------------------------------------------------------
recursive function getpsfigwidth_(self) result(psfw)
!DEC$ ATTRIBUTES DLLEXPORT :: getpsfigwidth_
  !! author: MDG
  !! version: 1.0
  !! date: 01/28/20
  !!
  !! return ps figure width

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)      :: self
real(kind=sgl)                     :: psfw

psfw = self%psfigwidth

end function getpsfigwidth_

!--------------------------------------------------------------------------
recursive function getpsfigheight_(self) result(psfh)
!DEC$ ATTRIBUTES DLLEXPORT :: getpsfigheight_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! make the last path the clippath

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)      :: self
real(kind=sgl)                     :: psfh

psfh = self%psfigheight

end function getpsfigheight_


!--------------------------------------------------------------------------
recursive subroutine clippath_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: clippath_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! make the last path the clippath

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)            :: self

 write (self%psunit,"('Cl clip')")

end subroutine clippath_

!--------------------------------------------------------------------------
recursive subroutine translate_(self,x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: translate_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! redefine the origin of the current coordinate frame

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)         :: self
real(kind=sgl),INTENT(IN)               :: x,y

 write (self%psunit,"(F18.7,' ',F18.7,' T')") x,y

end subroutine translate_

!--------------------------------------------------------------------------
recursive subroutine move_(self,x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: move_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! move to a given location

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)       :: self
real(kind=sgl),INTENT(IN)           :: x,y

 write (self%psunit,"(F18.7,' ',F18.7,' M')") x,y

end subroutine move_

!--------------------------------------------------------------------------
recursive subroutine draw_(self,x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: draw_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a line from the current point to the new point

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)         :: x,y

  write (self%psunit,"(F18.7,' ',F18.7,' L')") x,y

end subroutine draw_

!--------------------------------------------------------------------------
recursive subroutine line_gray_(self,x1,y1,x2,y2,gray)
!DEC$ ATTRIBUTES DLLEXPORT :: line_gray_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a line with a given gray level from the current point to the new point

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x1,y1
real(kind=sgl),INTENT(IN)            :: x2,y2
real(kind=sgl),INTENT(IN)            :: gray

  write (self%psunit,"(F18.7,' setgray ')") gray
  call self%move(x1,y1)
  call self%draw(x2,y2)

! and reset the gray level to black
  write (self%psunit,"('S  0.0 setgray ')")

end subroutine line_gray_

!--------------------------------------------------------------------------
recursive subroutine setlinewidth_(self,x)
!DEC$ ATTRIBUTES DLLEXPORT :: setlinewidth_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !!  set the line width

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x

 write (self%psunit,"(F12.7,' setlinewidth')") x

end subroutine setlinewidth_

!--------------------------------------------------------------------------
recursive subroutine setlinecolor_(self,rgb)
!DEC$ ATTRIBUTES DLLEXPORT :: setlinecolor_
  !! author: MDG
  !! version: 1.0
  !! date: 08/14/25
  !!
  !!  set the line color (rgb) 

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)    :: self
real(kind=sgl),INTENT(IN)            :: rgb(3)

 write (self%psunit,"(3(F12.7,' '),' setrgbcolor')") rgb

end subroutine setlinecolor_

!--------------------------------------------------------------------------
recursive subroutine square_(self,x,y,edge)
!DEC$ ATTRIBUTES DLLEXPORT :: square_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a square

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)           :: x,y
real(kind=sgl),INTENT(IN)           :: edge

real(kind=sgl)                        :: ed

 ed=0.5*edge
 write (self%psunit,"('0.0 setgray')")
 write (self%psunit,"('newpath')")
 write (self%psunit,"(2(F12.7,' '),'moveto')") x-ed,y-ed
 write (self%psunit,"(2(F12.7,' '),'lineto')") x-ed,y+ed
 write (self%psunit,"(2(F12.7,' '),'lineto')") x+ed,y+ed
 write (self%psunit,"(2(F12.7,' '),'lineto closepath S')") x+ed,y-ed

end subroutine square_

!--------------------------------------------------------------------------
recursive subroutine filledsquare_(self,x,y,edge,graylevel)
!DEC$ ATTRIBUTES DLLEXPORT :: filledsquare_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a filled square

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
real(kind=sgl),INTENT(IN)            :: edge
real(kind=sgl),INTENT(IN)            :: graylevel

real(kind=sgl)          :: ed

 ed=0.5*edge
 write (self%psunit,"(F12.7,' setgray')") graylevel
 call self%newpath()
 call self%move(x-ed,y-ed)
 call self%draw(x-ed,y+ed)
 call self%draw(x+ed,y+ed)
 write (self%psunit,"(2(F12.7,' '),'lineto closepath fill S')") x+ed,y-ed

end subroutine filledsquare_

!--------------------------------------------------------------------------
recursive subroutine cross_(self,x,y,edge,lw)
!DEC$ ATTRIBUTES DLLEXPORT :: cross_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a small cross

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
real(kind=sgl),INTENT(IN)            :: edge
real(kind=sgl),INTENT(IN)            :: lw

real(kind=sgl)                        :: ed

 ed=0.5*edge
 call self%setlinewidth(lw)
 call self%newpath()
 call self%move(x-ed,y-ed)
 call self%draw(x+ed,y+ed)
 call self%move(x-ed,y+ed)
 call self%draw(x+ed,y-ed)
 call self%stroke()

end subroutine cross_

!--------------------------------------------------------------------------
recursive subroutine sphere_(self,x,y,r,clr)
!DEC$ ATTRIBUTES DLLEXPORT :: sphere_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a colored sphere
  !!
  !! method modified from Earl J. Kirkland''s book, page 226, adapted for
  !! color PostScript

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT) :: self
real(kind=sgl),INTENT(IN)         :: x,y
real(kind=sgl),INTENT(IN)         :: r
integer(kind=irg),INTENT(IN)      :: clr(3)

write (self%psunit,"(1x,7(f12.5,1x),'sp')") clr(1:3),r,r,x,y

end subroutine sphere_

!--------------------------------------------------------------------------
recursive subroutine arc_(self,x0,y0,x,y,radius,ang1,ang2)
!DEC$ ATTRIBUTES DLLEXPORT :: arc_

  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw an arc of a circle (see PostScript 'arc' command for details)

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x0,y0
real(kind=sgl),INTENT(IN)            :: x,y
real(kind=sgl),INTENT(IN)            :: radius
real(kind=sgl),INTENT(IN)            :: ang1,ang2


 write (self%psunit,"('N ',2(F16.10,' '),' moveto ',5(E16.8,' '),' arc S')") x0,y0,x,y,radius,ang1,ang2

end subroutine arc_

!--------------------------------------------------------------------------
recursive subroutine circle_(self,x,y,radius)
!DEC$ ATTRIBUTES DLLEXPORT :: circle_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a circle

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
real(kind=sgl),INTENT(IN)            :: radius

 write (self%psunit,"('N ',3(F16.10,' '),'0 360 arc Cl S')") x,y,radius

end subroutine circle_

!--------------------------------------------------------------------------
recursive subroutine filledcircle_(self,x,y,radius,graylevel)
!DEC$ ATTRIBUTES DLLEXPORT :: filledcircle_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a filled circle

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
real(kind=sgl),INTENT(IN)            :: radius
real(kind=sgl),INTENT(IN)            :: graylevel

 write (self%psunit,"(F12.7,' setgray')") graylevel
 write (self%psunit,"('N ',3(F12.7,' '),'0 360 arc Cl F')") x,y,radius

end subroutine filledcircle_

!--------------------------------------------------------------------------
recursive subroutine drawframe_(self,x,y)
!DEC$ ATTRIBUTES DLLEXPORT :: drawframe_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw the main frame

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x
real(kind=sgl),INTENT(IN)            :: y

call self%drawrect(0.0,0.0,x,y)

end subroutine drawframe_

!--------------------------------------------------------------------------
recursive subroutine drawrect_(self,x1,y1,x2,y2)
!DEC$ ATTRIBUTES DLLEXPORT :: drawrect_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a rectangle

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x1, y1
real(kind=sgl),INTENT(IN)            :: x2, y2

 write (self%psunit,"('N')")
 call self%move(x1,y1)
 call self%draw(x1,y2)
 call self%draw(x2,y2)
 call self%draw(x2,y1)
 call self%draw(x1,y1)
 call self%closepathS()
 write (self%psunit,"('[0.15 0.03 0.02 0.03] 0 setdash')")
 write (self%psunit,"('[] 0 setdash')")

end subroutine drawrect_

!--------------------------------------------------------------------------
recursive subroutine line_(self,x1,y1,x2,y2)
!DEC$ ATTRIBUTES DLLEXPORT :: line_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a line between two points

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x1, y1
real(kind=sgl),INTENT(IN)            :: x2, y2

  call self%move(x1,y1)
  call self%draw(x2,y2)
  write (self%psunit,"('S')")

end subroutine line_

!--------------------------------------------------------------------------
recursive subroutine setdash_(self, num)
!DEC$ ATTRIBUTES DLLEXPORT :: setdash_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! define a dash pattern
  !!
  !! @details Note that the dash pattern must be defined in the calling program.

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
integer(kind=irg),INTENT(IN)        :: num

integer(kind=irg)                      :: i    ! loop counter

 write (self%psunit,"('[')")
 do i=1,num
   write (self%psunit,"(F12.7,' ')") self%psdash(i)
 end do
 write (self%psunit,"('] ',I4,' setdash')") int(self%psdash(num+1))

end subroutine setdash_

!--------------------------------------------------------------------------
recursive subroutine closepathS_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: closepathS_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! close current path and Stroke

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self

write (self%psunit,"('Cl S')")

end subroutine closepathS_

!--------------------------------------------------------------------------
recursive subroutine stroke_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: stroke_

  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! stroke the current path

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self

write (self%psunit,"('S ')")

end subroutine stroke_

!--------------------------------------------------------------------------
recursive subroutine gsave_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: gsave_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! save the current graphics settings

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self

write (self%psunit,"('gsave ')")

end subroutine gsave_

!--------------------------------------------------------------------------
recursive subroutine grestore_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: grestore_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! restore the previous graphics settings

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self

write (self%psunit,"('grestore ')")

end subroutine grestore_

!--------------------------------------------------------------------------
recursive subroutine closepath_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: closepath_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! close the current path

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self

write (self%psunit,"('Cl ')")

end subroutine closepath_

!--------------------------------------------------------------------------
recursive subroutine newpath_(self)
!DEC$ ATTRIBUTES DLLEXPORT :: newpath_

  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! start a new path

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self

write (self%psunit,"('newpath ')")

end subroutine newpath_

!--------------------------------------------------------------------------
recursive subroutine text_(self,x,y,line)
!DEC$ ATTRIBUTES DLLEXPORT :: text_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw text at a given location

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
character(*),INTENT(IN)              :: line

 write (self%psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (self%psunit,"('(')",advance="no")
 write (self%psunit,"(A)",advance="no") line
 write (self%psunit,"(') show')")

end subroutine text_

!--------------------------------------------------------------------------
recursive subroutine textv_(self,x,y,line)
!DEC$ ATTRIBUTES DLLEXPORT :: textv_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw text rotated counterclockwise by 90 degrees

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
character(*),INTENT(IN)              :: line

 write (self%psunit,"('gsave ')")
 write (self%psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (self%psunit,"('90.0 rotate')")
 write (self%psunit,"('( ',A,' ) show')") line
 write (self%psunit,"('-90.0 rotate grestore')")

end subroutine textv_

!--------------------------------------------------------------------------
recursive subroutine texttitle_(self,x,y,line,q)
!DEC$ ATTRIBUTES DLLEXPORT :: texttitle_

  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw the title

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
character(*),INTENT(IN)              :: line
real(kind=sgl),INTENT(IN)            :: q

 write (self%psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (self%psunit,"('(')",advance="no")
 write (self%psunit,"(A)",advance="no") line
 write (self%psunit,"('  [x',1PE8.0,'] ) show')") q

end subroutine texttitle_

!--------------------------------------------------------------------------
recursive subroutine textvtitle_(self,x,y,line,q)
!DEC$ ATTRIBUTES DLLEXPORT :: textvtitle_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw a vertical title

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)         :: x,y
character(*),INTENT(IN)           :: line
real(kind=sgl),INTENT(IN)         :: q

 write (self%psunit,"('gsave ')")
 write (self%psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (self%psunit,"('90.0 rotate')")
 write (self%psunit,"('(')",advance="no")
 write (self%psunit,"(A)",advance="no") line
 write (self%psunit,"('  [x',1PE8.0,'] ) show')") q
 write (self%psunit,"('-90.0 rotate grestore')")

end subroutine textvtitle_

!--------------------------------------------------------------------------
recursive subroutine textint_(self,x,y,line,vl)
!DEC$ ATTRIBUTES DLLEXPORT :: textint_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! text followed by an integer number

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
character(*),INTENT(IN)              :: line
integer(kind=irg),INTENT(IN)        :: vl

 write (self%psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (self%psunit,"('(')",advance="no")
 write (self%psunit,"(A)",advance="no") line
 write (self%psunit,"(I4)",advance="no") vl
 write (self%psunit,"(') show')")

end subroutine textint_

!--------------------------------------------------------------------------
recursive subroutine textvar_(self,x,y,line,vl)
!DEC$ ATTRIBUTES DLLEXPORT :: textvar_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! text followed by a real number

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
character(*),INTENT(IN)              :: line
real(kind=sgl),INTENT(IN)            :: vl

 write (self%psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (self%psunit,"('(')",advance="no")
 write (self%psunit,"(A)",advance="no") line
 write (self%psunit,"(F14.4)",advance="no") vl
 write (self%psunit,"(') show')")

end subroutine textvar_

!--------------------------------------------------------------------------
recursive subroutine textvardbl_(self,x,y,line,vl)
!DEC$ ATTRIBUTES DLLEXPORT :: textvardbl_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! text followed by a double precision real number

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
character(*),INTENT(IN)              :: line
real(kind=dbl),INTENT(IN)            :: vl

 write (self%psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (self%psunit,"('(')",advance="no")
 write (self%psunit,"(A)",advance="no") line
 write (self%psunit,"(F12.6)",advance="no") vl
 write (self%psunit,"(') show')")

end subroutine textvardbl_

!--------------------------------------------------------------------------
recursive subroutine textballoon_(self,x,y,line,font,sc)
!DEC$ ATTRIBUTES DLLEXPORT :: textballoon_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! text inside a rounded balloon

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
character(*),INTENT(IN)              :: line
character(*),INTENT(IN)              :: font
real(kind=sgl),INTENT(IN)            :: sc

 call self%setfont(font,sc)
 write (self%psunit,"('/length (')",advance="no")
 write (self%psunit,"(A)",advance="no") line
 write (self%psunit,"(') stringwidth pop def')")
 write (self%psunit,"('/height ',F6.4,' def /border ',F6.4,' def')") 0.11*sc/0.2,0.06*sc/0.2
 write (self%psunit,"('/bottom ',F12.7,' border sub def')") y
 write (self%psunit,"('/top ',F12.7,' height add border add def')") y
 write (self%psunit,"('/left ',F12.7,' border sub def')") x
 write (self%psunit,"('/right ',F12.7,' length add border add def')") x
 write (self%psunit,"('/rad 0.04 def frame')")
 write (self%psunit,"(F12.7,' ',F12.7,' M')") x,y
 write (self%psunit,"('(')",advance="no")
 write (self%psunit,"(A)",advance="no") line
 write (self%psunit,"(') show')")

end subroutine textballoon_

!--------------------------------------------------------------------------
recursive subroutine balloon_(self,x,y,le,he,w)
!DEC$ ATTRIBUTES DLLEXPORT :: balloon_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! draw an empty balloon

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: x,y
real(kind=sgl),INTENT(IN)            :: le, he
real(kind=sgl),INTENT(IN)            :: w

 write (self%psunit,"('/he ',F6.4,' def /bo ',F6.4,' def /wi ',F6.4,' def')") he,0.5*w,le
 write (self%psunit,"('/bottom ',F12.7,' bo add def')") y
 write (self%psunit,"('/top bottom he add bo sub bo sub def')")
 write (self%psunit,"('/left ',F12.7,' bo add def')") x
 write (self%psunit,"('/right left wi add bo sub def')")
 write (self%psunit,"('/rad 0.04 def frame')")

end subroutine balloon_

!--------------------------------------------------------------------------
recursive subroutine setfont_(self,line,sc)
!DEC$ ATTRIBUTES DLLEXPORT :: setfont_
  !! author: MDG
  !! version: 1.0
  !! date: 01/15/20
  !!
  !! select a font and make it active

IMPLICIT NONE

class(PostScript_T),INTENT(INOUT)     :: self
real(kind=sgl),INTENT(IN)            :: sc
character(*),INTENT(IN)              :: line

 write (self%psunit,"()",advance="no")
 write (self%psunit,"('/',A)",advance="no") line
 write (self%psunit,"(' findfont')")
 write (self%psunit,"(F6.4,' scalefont ')") sc
 write (self%psunit,"('setfont')")

end subroutine setfont_

end module mod_postscript
