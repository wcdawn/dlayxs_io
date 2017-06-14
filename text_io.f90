MODULE text_io
USE variables
IMPLICIT NONE

CONTAINS

SUBROUTINE textio(fname)
CHARACTER(*),INTENT(IN) :: fname
INTEGER,PARAMETER :: iout = 11

INTEGER :: I, ios

101 FORMAT(a)
900 FORMAT(/, 'FILE IDENTIFICATION', &
           /, a,  ' HNAME         HOLLERITH FILE NAME - DLAYXS -', &
           /, a,  ' HUSE(1)       HOLLERITH USER IDENTIFICATION', &
           /, a,  ' HUSE(2)       HOLLERITH USER IDENTIFICATION', &
           /, i8, ' IVERS         FILE VERSION NUMBER')
901 FORMAT(/, 'FILE CONTROL    (1D RECORD)', &
           /, i8, ' NGROUP        NUMBER OF NEUTRON ENERGY GROUPS IN SET', &
           /, i8, ' NISOD         NUMBER OF ISOTOPES IN DELAYED NEUTRON SET', &
           /, i8, ' NFAM          NUMBER OF DELAYED NEUTRON FAMILIES IN SET', &
           /, i8, ' IDUM          DUMMY TO MAKE UP FOUR WORD RECORD.')

!------------------------------------------------------------------------------!
! OPEN FILES
!------------------------------------------------------------------------------!
! ASCII out
OPEN(UNIT = iout, FILE = fname, STATUS = 'replace', ACTION = 'write', IOSTAT = ios)
CALL checkopen(iout,fname,ios)

! FILE IDENTIFICATION
WRITE(iout,900) hname, huse(1), huse(2), ivers

! FILE CONTROL    (1D RECORD)
WRITE(iout,901) ngroup, nisod, nfam, idum

! FILE DATA, DECAY CONSTANTS, AND EMISSION SPECTRA    (2D RECORD)
WRITE(iout,*)
WRITE(iout,101) 'FILE DATA, DECAY CONSTANTS, AND EMISSION SPECTRA    (2D RECORD)'
WRITE(iout,101) 'HABSID(I)     HOLLERITH ABSOLUTE ISOTOPE LABEL FOR ISOTOPE I'
CALL write_fivetable_char(habsid,nisod,iout)
WRITE(iout,101) 'FLAM(N)       DELAYED NEUTRON PRECURSOR DECAY CONSTANT FOR FAMILY N'
CALL write_fivetable_real(flam,nfam,iout)
WRITE(iout,101) 'CHID(J,N)     FRACTION OF DELAYED NEUTRONS EMITTED INTO NEUTRON ENERGY GROUP J FROM PRECURSOR FAMILY N'
CALL write_realtable(chid,ngroup,nfam,iout)
WRITE(iout,101) 'EMAX(J)       MAXIMUM ENERGY BOUND OF GROUP J (EV)'
CALL write_fivetable_real(emax,ngroup,iout)
WRITE(iout,101) 'EMIN          MINIMUM ENERGY BOUND OF SET (EV)'
WRITE(iout,'(e12.6)') emin
WRITE(iout,101) 'NKFAM(I)      NUMBER OF FAMILIES TO WHICH FISSION IN ISOTOPE I CONTRIBUTES DELAYED NEUTRON PRECURSORS'
CALL write_fivetable_int(nkfam,nisod,iout)
WRITE(iout,101) 'LOCA(I)       NUMBER OF RECORDS TO BE SKIPPED TO READ DATA FOR ISOTOPE I.  LOCA(1)=0'
CALL write_fivetable_int(loca,nisod,iout)

! DELAYED NEUTRON PRECURSOR YIELD DATA   (3D RECORD)
WRITE(iout,*)
WRITE(iout,101) '--------------------------------------------------------------------------------'
WRITE(iout,101) 'DELAYED NEUTRON PRECURSOR YIELD DATA   (3D RECORD)'
WRITE(iout,101) '--------------------------------------------------------------------------------'
DO i = 1,nisod
  WRITE(iout,*)
  WRITE(iout,'(i3)') i
  WRITE(iout,101) 'SNUDEL(J,K)   NUMBER OF DELAYED NEUTRON PRECURSORS PRODUCED IN FAMILY NUMBER NUMFAM(K) PER FISSION IN GROUP J'
  CALL write_realtable(snudel(i,:,:),ngroup,nkfam(i),iout)
  WRITE(iout,101) 'NUMFAM(K)     FAMILY NUMBER OF THE K-TH YIELD VECTOR IN ARRAY SNUDEL(J,K)'
  CALL write_fivetable_int(numfam(i,:),nkfam(i),iout)
ENDDO

ENDSUBROUTINE textio

SUBROUTINE checkopen(ifl,fname,ios)
IMPLICIT NONE
  INTEGER,INTENT(IN) :: ifl, ios
  CHARACTER(*),INTENT(IN) :: fname
  
  IF (ios /= 0) THEN
    WRITE(*,'(a,i6,a,a)') 'FATAL -- erorr opening unit -- ', ifl, ' -- ', fname
    WRITE(*,'(a,i6)') 'ios = ', ios
    STOP
  ENDIF
  
ENDSUBROUTINE checkopen

SUBROUTINE write_fivetable_char(var,length,iout)
IMPLICIT NONE
  CHARACTER(8),DIMENSION(:),INTENT(IN) :: var
  INTEGER,INTENT(IN) :: length, iout
  INTEGER :: r, c, i
  
  i = 0
  DO r = 1,((length / 5) + 1)
    IF (i == length) EXIT
    DO C = 1,5
      i = i + 1
      IF (i > length) EXIT
      WRITE(iout,'(i3,x,a,3x)',ADVANCE = 'no') i, var(i)
    ENDDO
    WRITE(iout,*)
  ENDDO
  
ENDSUBROUTINE write_fivetable_char

subroutine write_fivetable_real(var,length,iout)
IMPLICIT NONE
real(4),dimension(:),intent(in) :: var
integer,intent(in) :: length, iout
integer :: r, c, i
i = 0
do r = 1,((length / 5) + 1)
	if (i .eq. length) exit
	do c = 1,5
		i = i + 1
		if (i .gt. length) exit
		write(iout,'(i3,x,e12.6,3x)',advance = 'no') i, var(i)
	enddo
	write(iout,*)
enddo
endsubroutine write_fivetable_real

subroutine write_realtable(var,dim1,dim2,iout)
IMPLICIT NONE
real(4),dimension(:,:),intent(in) :: var
integer,intent(in) :: dim1, dim2, iout
integer :: i, j
do i = 1,dim1
	do j = 1,dim2
		write(iout,'(e12.6,x)',advance = 'no') var(i,j)
	enddo
	write(iout,*)
enddo
endsubroutine write_realtable

subroutine write_fivetable_int(var,length,iout)
IMPLICIT NONE
integer,dimension(:),intent(in) :: var
integer,intent(in) :: length, iout
integer :: r, c, i
i = 0
do r = 1,((length / 5) + 1)
	if (i .eq. length) exit
	do c = 1,5
		i = i + 1
		if (i .gt. length) exit
		write(iout,'(i3,x,i6,3x)',advance = 'no') i, var(i)
	enddo
	write(iout,*)
enddo
endsubroutine write_fivetable_int

ENDMODULE text_io