MODULE variables
IMPLICIT NONE

! FILE IDENTIFICATION
CHARACTER(8) :: hname
CHARACTER(8),DIMENSION(2) :: huse
INTEGER :: ivers
! FILE CONTROL    (1D RECORD)
INTEGER :: ngroup, nisod, nfam, idum
! FILE DATA, DECAY CONSTANTS, AND EMISSION SPECTRA    (2D RECORD)
CHARACTER(8),ALLOCATABLE,DIMENSION(:) :: habsid
REAL(4),ALLOCATABLE,DIMENSION(:) :: flam
REAL(4),ALLOCATABLE,DIMENSION(:,:) :: chid
REAL(4),ALLOCATABLE,DIMENSION(:) :: emax
REAL(4) :: emin
INTEGER,ALLOCATABLE,DIMENSION(:) :: nkfam
INTEGER,ALLOCATABLE,DIMENSION(:) :: loca
! DELAYED NEUTRON PRECURSOR YIELD DATA   (3D RECORD)
REAL(4),ALLOCATABLE,DIMENSION(:,:,:) :: snudel
INTEGER,ALLOCATABLE,DIMENSION(:,:) :: numfam

CONTAINS

SUBROUTINE allocate_DLAYXS()
IMPLICIT NONE
  
  ! FILE DATA, DECAY CONSTANTS, AND EMISSION SPECTRA    (2D RECORD)
  ALLOCATE(habsid(nisod))
  ALLOCATE(flam(nfam))
  ALLOCATE(chid(ngroup,nfam))
  ALLOCATE(emax(ngroup))
  ALLOCATE(nkfam(nisod))
  ALLOCATE(loca(nisod))
  ! DELAYED NEUTRON PRECURSOR YIELD DATA   (3D RECORD)
  ALLOCATE(snudel(nisod,ngroup,nfam))
  ALLOCATE(numfam(nisod,nfam))
  snudel(:,:,:) = 0.0
  numfam(:,:) = 0.0

ENDSUBROUTINE allocate_DLAYXS

ENDMODULE variables