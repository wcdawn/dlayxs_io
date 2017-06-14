PROGRAM dlayxsio
USE variables
USE text_io
IMPLICIT NONE

INTEGER :: i, ios
LOGICAL :: lfixstr = .FALSE.

! pseudo-input
CHARACTER(*),PARAMETER :: fname = 'DLAYXS_soft_fuel.300'
INTEGER,PARAMETER :: ifl = 730
LOGICAL,PARAMETER :: ltextio = .TRUE.

101 FORMAT(a)

!------------------------------------------------------------------------------!
! OPEN FILES
!------------------------------------------------------------------------------!
! DLAYXS in
! default open with little_endian. May be closed and reopened later
OPEN(UNIT = ifl, FILE = fname, STATUS = 'old', FORM = 'unformatted', ACTION = 'read', CONVERT = 'LITTLE_ENDIAN', IOSTAT = ios)
CALL checkopen(ifl,fname,ios)

!------------------------------------------------------------------------------!
! READ FILE IDENTIFICATION
!------------------------------------------------------------------------------!
READ(ifl, IOSTAT = ios) hname, huse(1:2), ivers
! IF ((ivers < 0) .OR. (ivers > 20)) THEN
  ! need big-endian open
  ! WRITE(*,101) 'closing DLAYXS and opening with BIG_ENDIAN'
  ! CLOSE(ifl)
  ! OPEN(UNIT = ifl, FILE = fname, STATUS = 'old', FORM = 'unformatted', ACTION = 'read', CONVERT = 'BIG_ENDIAN', IOSTAT = ios)
  ! READ(ifl, IOSTAT = ios) hname, huse(1:2), ivers
! ENDIF
IF (ios /= 0) THEN
  WRITE(*,101) 'FATAL -- error reading FILE IDENTIFICATION'
  STOP
ENDIF

! check if characters need fixing
IF (hname /= ' 0V DLAY') THEN
  lfixstr = .TRUE.
  write(*,101) 'fixing strings'
ENDIF
IF (lfixstr) THEN
  CALL fixstr(hname)
  CALL fixstr(huse(1))
  CALL fixstr(huse(2))
ENDIF

!------------------------------------------------------------------------------!
! READ FILE CONTROL   (1D RECORD)
!------------------------------------------------------------------------------!
READ(ifl, IOSTAT = ios) ngroup, nisod, nfam, idum
IF (ios /= 0) THEN
  WRITE(*,101) 'FATAL -- error reading FILE CONTROL    (1D RECORD)'
  STOP
ENDIF

CALL allocate_DLAYXS()

!------------------------------------------------------------------------------!
! READ FILE DATA, DECAY CONSTANTS, AND EMISSION SPECTRA    (2D RECORD)
!------------------------------------------------------------------------------!
READ(ifl, IOSTAT = ios) habsid(1:nisod), flam(1:nfam), chid(1:ngroup,1:nfam), emax(1:ngroup), emin, nkfam(1:nisod), loca(1:nisod)
IF (ios /= 0) THEN
  WRITE(*,101) 'FATAL -- error reading FILE DATA, DECAY CONSTANTS, AND EMISSION SPECTRA    (2D RECORD)'
  STOP
ENDIF

DO i = 1,nisod
  !------------------------------------------------------------------------------!
  ! READ DELAYED NEUTRON PRECURSOR YIELD DATA   (3D RECORD)
  !------------------------------------------------------------------------------!
  READ(ifl, IOSTAT = ios) snudel(i,1:ngroup,1:nkfam(i)), numfam(i,1:nkfam(i))
    IF (ios /= 0) THEN
    WRITE(*,101) 'FATAL -- error reading DELAYED NEUTRON PRECURSOR YIELD DATA   (3D RECORD)'
    WRITE(*,'(a,i3)') 'niso = ', i
    STOP
  ENDIF

  
ENDDO


IF (ltextio) THEN
  CALL textio('ascii.out')
ENDIF


ENDPROGRAM dlayxsio

SUBROUTINE fixstr(str)
  IMPLICIT NONE
  CHARACTER(8) :: str, str0
  str0 = str
  str(1:1) = str0(4:4)
  str(2:2) = str0(3:3)
  str(3:3) = str0(2:2)
  str(4:4) = str0(1:1)
  str(5:5) = str0(8:8)
  str(6:6) = str0(7:7)
  str(7:7) = str0(6:6)
  str(8:8) = str0(5:5)
ENDSUBROUTINE fixstr