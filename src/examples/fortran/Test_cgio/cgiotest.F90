      PROGRAM TEST
      USE CGNS

!---- This code has been modified to use the cgio interface
!---- The old ADF code has been left in to show the difference
!
!     SAMPLE ADF TEST PROGRAM TO BUILD ADF FILES ILLUSTRATED
!     IN THE EXAMPLE DATABASE FIGURE
!
      PARAMETER (MAXCHR=32)
!
      CHARACTER*(MAXCHR) TSTLBL,DTYPE
      CHARACTER*(MAXCHR) FNAM,PATH
!
      REAL*8 RID,PID,CID,TMPID,RIDF2
      REAL*4 A(4,3),B(4,3)
!      INTEGER IC(6),ID(6)
      integer*4 ic(6),id(6)
      INTEGER IERR,cgnum
      INTEGER(CGSIZE_T) IDIMC, IDIMD, IDIM(2)
      INTEGER(CGSIZE_T) IDIMA(2)
!
      DATA A /1.1,2.1,3.1,4.1, &
              1.2,2.2,3.2,4.2, &
              1.3,2.3,3.3,4.3/
      DATA IDIMA /4,3/
!
      DATA IC /1,2,3,4,5,6/
      DATA IDIMC /6/
!
      integer FILE_NONE, FILE_ADF, FILE_HDF5
      parameter (FILE_NONE = 0)
      parameter (FILE_ADF  = 1)
      parameter (FILE_HDF5 = 2)
!
!     SET ERROR FLAG TO ABORT ON ERROR
!
!      CALL ADFSES(1,IERR)
!
! *** 1.) OPEN 1ST DATABASE (ADF_FILE_TWO.ADF)
!     2.) CREATE THREE NODES AT FIRST LEVEL
!     3.) PUT LABEL ON NODE F3
!     4.) PUT DATA IN F3
!     5.) CREATE TWO NODES BELOW F3
!     6.) CLOSE DATABASE
!
!      CALL ADFDOPN('adf_file_two.adf','NEW',' ',RID,IERR)
      call cgio_open_file_f('adf_file_two.adf',MODE_WRITE,FILE_ADF, &
                            cgnum,ierr)
      call cgio_get_root_id_f(cgnum,rid,ierr)
      RIDF2 = RID
!      CALL ADFCRE(RID,'F1',TMPID,IERR)
      call cgio_create_node_f(cgnum,rid,'F1',tmpid,ierr)
!      CALL ADFCRE(RID,'F2',TMPID,IERR)
      call cgio_create_node_f(cgnum,rid,'F2',tmpid,ierr)
!      CALL ADFCRE(RID,'F3',PID,IERR)
      call cgio_create_node_f(cgnum,rid,'F3',pid,ierr)
!      CALL ADFSLB(PID,'LABEL ON NODE F3',IERR)
      call cgio_set_label_f(cgnum,pid,'LABEL ON NODE F3',ierr)
!      CALL ADFPDIM(PID,'R4',2,IDIMA,IERR)
      call cgio_set_dimensions_f(cgnum,pid,'R4',2,idima,ierr)
!      CALL ADFWALL(PID,A,IERR)
      call cgio_write_all_data_f(cgnum,pid,a,ierr)
!
!      CALL ADFCRE(PID,'F4',CID,IERR)
      call cgio_create_node_f(cgnum,pid,'F4',cid,ierr)
!
!      CALL ADFCRE(PID,'F5',CID,IERR)
      call cgio_create_node_f(cgnum,pid,'F5',cid,ierr)
!
!      CALL ADFDCLO(RID,IERR)
      call cgio_close_file_f(cgnum,ierr)
!
! *** 1.) OPEN 2ND DATABASE
!     2.) CREATE NODES
!     3.) PUT DATA IN N13
!
!      CALL ADFDOPN('adf_file_one.adf','NEW',' ',RID,IERR)
      call cgio_open_file_f('adf_file_one.adf',MODE_WRITE,FILE_ADF, &
                            cgnum,ierr)
      call cgio_get_root_id_f(cgnum,rid,ierr)
!
!     THREE NODES UNDER ROOT
!
!      CALL ADFCRE(RID,'N1',TMPID,IERR)
      call cgio_create_node_f(cgnum,rid,'N1',tmpid,ierr)
!      CALL ADFCRE(RID,'N2',TMPID,IERR)
      call cgio_create_node_f(cgnum,rid,'N2',tmpid,ierr)
!      CALL ADFCRE(RID,'N3',TMPID,IERR)
      call cgio_create_node_f(cgnum,rid,'N3',tmpid,ierr)
!
!     THREE NODES UNDER N1 (TWO REGULAR AND ONE LINK)
!
!      CALL ADFGNID(RID,'N1',PID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'N1',pid,ierr)
!      CALL ADFCRE(PID,'N4',TMPID,IERR)
      call cgio_create_node_f(cgnum,pid,'N4',tmpid,ierr)
!      CALL ADFLINK(PID,'L3','adf_file_two.adf','/F3',TMPID,IERR)
      call cgio_create_link_f(cgnum,pid,'L3','adf_file_two.adf','/F3', &
                              tmpid,ierr)
!      CALL ADFCRE(PID,'N5',TMPID,IERR)
      call cgio_create_node_f(cgnum,pid,'N5',tmpid,ierr)
!
!     TWO NODES UNDER N4
!
!      CALL ADFGNID(PID,'N4',CID,IERR)
      call cgio_get_node_id_f(cgnum,pid,'N4',cid,ierr)
!      CALL ADFCRE(CID,'N6',TMPID,IERR)
      call cgio_create_node_f(cgnum,cid,'N6',tmpid,ierr)
!      CALL ADFCRE(CID,'N7',TMPID,IERR)
      call cgio_create_node_f(cgnum,cid,'N7',tmpid,ierr)
!
!     ONE NODE UNDER N6
!
!      CALL ADFGNID(RID,'/N1/N4/N6',PID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'/N1/N4/N6',pid,ierr)
!      CALL ADFCRE(PID,'N8',TMPID,IERR)
      call cgio_create_node_f(cgnum,pid,'N8',tmpid,ierr)
!
!     THREE NODES UNDER N3
!
!      CALL ADFGNID(RID,'N3',PID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'N3',pid,ierr)
!      CALL ADFCRE(PID,'N9',TMPID,IERR)
      call cgio_create_node_f(cgnum,pid,'N9',tmpid,ierr)
!      CALL ADFCRE(PID,'N10',TMPID,IERR)
      call cgio_create_node_f(cgnum,pid,'N10',tmpid,ierr)
!      CALL ADFCRE(PID,'N11',TMPID,IERR)
      call cgio_create_node_f(cgnum,pid,'N11',tmpid,ierr)
!
!     TWO NODES UNDER N9
!
!      CALL ADFGNID(PID,'N9',CID,IERR)
      call cgio_get_node_id_f(cgnum,pid,'N9',cid,ierr)
!      CALL ADFCRE(CID,'N12',TMPID,IERR)
      call cgio_create_node_f(cgnum,cid,'N12',tmpid,ierr)
!      CALL ADFCRE(CID,'N13',TMPID,IERR)
      call cgio_create_node_f(cgnum,cid,'N13',tmpid,ierr)
!
!     PUT LABEL AND DATA IN N13
!
!      CALL ADFSLB(TMPID,'LABEL ON NODE N13',IERR)
      call cgio_set_label_f(cgnum,tmpid,'LABEL ON NODE N13',ierr)
!      CALL ADFPDIM(TMPID,'I4',1,IDIMC,IERR)
      call cgio_set_dimensions_f(cgnum,tmpid,'I4',1,idimc,ierr)
!      CALL ADFWALL(TMPID,IC,IERR)
      call cgio_write_all_data_f(cgnum,tmpid,ic,ierr)
!
!     TWO NODES UNDER N10
!
!      CALL ADFGNID(RID,'/N3/N10',PID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'/N3/N10',pid,ierr)
!      CALL ADFLINK(PID,'L1',' ','/N3/N9/N13',TMPID,IERR)
      call cgio_create_link_f(cgnum,pid,'L1',CHAR(0),'/N3/N9/N13', &
                              tmpid,ierr)
!      CALL ADFCRE(PID,'N14',TMPID,IERR)
      call cgio_create_node_f(cgnum,pid,'N14',tmpid,ierr)
!
!     TWO NODES UNDER N11
!
!      CALL ADFGNID(RID,'/N3/N11',PID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'/N3/N11',pid,ierr)
!      CALL ADFLINK(PID,'L2',' ','/N3/N9/N13',TMPID,IERR)
      call cgio_create_link_f(cgnum,pid,'L2',CHAR(0),'/N3/N9/N13', &
                              tmpid,ierr)
!      CALL ADFCRE(PID,'N15',TMPID,IERR)
      call cgio_create_node_f(cgnum,pid,'N15',tmpid,ierr)
!
! *** READ AND PRINT DATA FROM NODES
!     1.) NODE F5 THROUGH LINK L3
!
!      CALL ADFGNID(RID,'/N1/L3',PID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'/N1/L3',pid,ierr)
!      CALL ADFGLB(PID,TSTLBL,IERR)
      call cgio_get_label_f(cgnum,pid,tstlbl,ierr)
!      CALL ADFGDT(PID,DTYPE,IERR)
      call cgio_get_data_type_f(cgnum,pid,dtype,ierr)
!      CALL ADFGND(PID,NUMDIM,IERR)
!      CALL ADFGDV(PID,IDIM,IERR)
      call cgio_get_dimensions_f(cgnum,pid,numdim,idim,ierr)
!      CALL ADFRALL(PID,B,IERR)
      call cgio_read_all_data_type_f(cgnum,pid,dtype,b,ierr)
      PRINT *,    ' NODE F3 THROUGH LINK L3:'
      PRINT *,    '   LABEL       = ',TSTLBL
      PRINT *,    '   DATA TYPE   = ',DTYPE
      WRITE(*,101)'   NUM OF DIMS = ',NUMDIM
      WRITE(*,102)'   DIM VALS    = ',IDIM
      PRINT *,'   DATA:'
      WRITE(*,100)((B(J,I),I=1,3),J=1,4)
  100 FORMAT(5X,3F10.2)
  101 FORMAT(A,I6)
  102 FORMAT(A,2I6)
!
!     2.) N13
!
!      CALL ADFGNID(RID,'N3/N9/N13',PID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'N3/N9/N13',pid,ierr)
!      CALL ADFGLB(PID,TSTLBL,IERR)
      call cgio_get_label_f(cgnum,pid,tstlbl,ierr)
!      CALL ADFGDT(PID,DTYPE,IERR)
      call cgio_get_data_type_f(cgnum,pid,dtype,ierr)
!      CALL ADFGND(PID,NUMDIM,IERR)
!      CALL ADFGDV(PID,IDIMD,IERR)
      call cgio_get_dimensions_f(cgnum,pid,numdim,idimd,ierr)
!      CALL ADFRALL(PID,ID,IERR)
      call cgio_read_all_data_type_f(cgnum,pid,dtype,id,ierr)
      PRINT *,    ' '
      PRINT *,    ' NODE N13:'
      PRINT *,    '   LABEL       = ',TSTLBL
      PRINT *,    '   DATA TYPE   = ',DTYPE
      WRITE(*,101)'   NUM OF DIMS = ',NUMDIM
      WRITE(*,101)'   DIM VALS    = ',IDIMD
      PRINT *,    '   DATA:'
      WRITE(*,200)(ID(I),I=1,6)
  200 FORMAT(5X,6I6)
!
!     3.) N13 THROUGH L1
!
!      CALL ADFGNID(RID,'N3/N10/L1',TMPID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'N3/N10/L1',tmpid,ierr)
!      CALL ADFGLB(TMPID,TSTLBL,IERR)
      call cgio_get_label_f(cgnum,tmpid,tstlbl,ierr)
!      CALL ADFRALL(TMPID,ID,IERR)
      call cgio_read_all_data_type_f(cgnum,tmpid,dtype,id,ierr)
      PRINT *,' '
      PRINT *,' NODE N13 THROUGH LINK L1:'
      PRINT *,'   LABEL       = ',TSTLBL
      PRINT *,'   DATA:'
      WRITE(*,200)(ID(I),I=1,6)
!
!     4.) N13 THROUGH L2
!
!      CALL ADFGNID(RID,'N3/N11/L2',CID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'N3/N11/L2',cid,ierr)
!      CALL ADFGLB(CID,TSTLBL,IERR)
      call cgio_get_label_f(cgnum,cid,tstlbl,ierr)
!      CALL ADFRALL(CID,ID,IERR)
      call cgio_read_all_data_type_f(cgnum,cid,dtype,id,ierr)
      PRINT *,' '
      PRINT *,' NODE N13 THROUGH LINK L2:'
      PRINT *,'   LABEL       = ',TSTLBL
      PRINT *,'   DATA:'
      WRITE(*,200)(ID(I),I=1,6)
!
!     PRINT LIST OF CHILDREN UNDER ROOT NODE
!
!      CALL PRTCLD(RID)
      call prtcld(cgnum, rid)
!
!     PRINT LIST OF CHILDREN UNDER N3
!
!      CALL ADFGNID(RID,'N3',PID,IERR)
      call cgio_get_node_id_f(cgnum,rid,'N3',pid,ierr)
!      CALL PRTCLD(PID)
      call prtcld(cgnum, pid)
      call cgio_close_file_f(cgnum,ierr)
!
!     REOPEN ADF_FILE_TWO AND GET NEW ROOT ID
!
!      CALL ADFDOPN('adf_file_two.adf','OLD',' ',RID,IERR)
      call cgio_open_file_f('adf_file_two.adf',MODE_READ,FILE_NONE, &
                            cgnum,ierr)
      call cgio_get_root_id_f(cgnum,rid,ierr)
      call cgio_close_file_f(cgnum,ierr)
      PRINT *,' '
      PRINT *,' COMPARISON OF ROOT ID: '
      WRITE(*,'(A,F12.8)')' ADF_FILE_TWO.ADF ORIGINAL ROOT ID = ',RIDF2
      WRITE(*,'(A,F12.8)')' ADF_FILE_TWO.ADF NEW ROOT ID      = ',RID
!
      END
!
! ************* SUBROUTINES ****************
!
!      SUBROUTINE PRTCLD(PID)
      subroutine prtcld(cgnum, pid)
      USE CGNS
!
! *** PRINT TABLE OF CHILDREN GIVEN A PARENT NODE-ID
!
      integer cgnum,numc,ierr,istart,numret
      integer MAXCLD,MAXCHR
      PARAMETER (MAXCLD=10)
      PARAMETER (MAXCHR=32)
      REAL*8 PID
      CHARACTER*(MAXCHR) NODNAM,NDNMS(MAXCLD)

!      CALL ADFGNAM(PID,NODNAM,IERR)
      call cgio_get_name_f(cgnum,pid,nodnam,ierr)
!      CALL ADFNCLD(PID,NUMC,IERR)
      call cgio_number_children_f(cgnum,pid,numc,ierr)
      WRITE(*,120)NODNAM,NUMC
  120 FORMAT(/,' PARENT NODE NAME = ',A,/, &
             '     NUMBER OF CHILDREN = ',I2,/, &
             '     CHILDREN NAMES:')
      NLEFT = NUMC
      ISTART = 1
!     --- TOP OF DO-WHILE LOOP
  130 CONTINUE
!        CALL ADFCNAM(PID,ISTART,MAXCLD,LEN(NDNMS),
!---------------- g77 doesn't like this ^
!         CALL ADFCNAM(PID,ISTART,MAXCLD,MAXCHR,
!     X                NUMRET,NDNMS,IERR)
         call cgio_children_names_f(cgnum,pid,istart,maxcld,maxchr, &
                                    numret,ndnms,ierr)
         WRITE(*,140)(NDNMS(K),K=1,NUMRET)
  140    FORMAT(8X,A)
         NLEFT = NLEFT - MAXCLD
         ISTART = ISTART + MAXCLD
      IF (NLEFT .GT. 0) GO TO 130
      RETURN
      END


