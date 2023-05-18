MODULE richardson_module
  USE globals
  IMPLICIT NONE
  PRIVATE
  PUBLIC compute_rich_rho,compute_rich_general

CONTAINS

  SUBROUTINE compute_rich_rho()
    INTEGER :: i
    REAL(8) :: p
    LOGICAL :: valid_p=.FALSE.

    DO i=3,data_size
      p=(in_y(i)-in_y(i-1))/(in_y(i-1)-in_y(i-2))
      IF(p .GT. 0.0D0)THEN
        valid_p=.TRUE.
        p=LOG(p)/LOG(rho)
        rich_results(i)=(in_y(i)-rho**p*in_y(i-1))/(1.0D0-rho**p)
        p_results(i)=p
      ENDIF
    ENDDO
    IF(.NOT. valid_p)STOP 'No real p values computed. System is likely not in asymptotic region.'
  ENDSUBROUTINE compute_rich_rho

  SUBROUTINE compute_rich_general()
    INTEGER :: i
    REAL(8) :: p,rich
    LOGICAL :: valid_p=.FALSE.

    DO i=6,6
      WRITE(*,*)'i',i
      CALL comp_rich_point_gen(in_x(i-2:i),in_y(i-2:i),p,rich)
      IF(p .GT. 0.0D0 .AND. .NOT. ISNAN(p))THEN
        valid_p=.TRUE.
        rich_results(i)=rich
        p_results(i)=p
      ENDIF
    ENDDO
    IF(.NOT. valid_p)STOP 'No real p values computed. System is likely not in asymptotic region.'
    WRITE(*,*)rich_results
    WRITE(*,*)p_results
    STOP 'compute_rich_general not complete'
  ENDSUBROUTINE compute_rich_general


  SUBROUTINE comp_rich_point_gen(x_data,y_data,presult,fresult)
    REAL(8), INTENT(IN) :: x_data(3),y_data(3)
    REAL(8), INTENT(OUT) :: presult,fresult
    REAL(8) :: cval(2),pval(2),fval(2)
    REAL(8) :: cerror,perror,ferror
    INTEGER :: i,order(3)

    !get order of furthest from 1 to closest
    order(1)=1
    order(2)=2
    order(3)=3

    !initial guesses
    cval=1
    pval=1
    fval=y_data(3)+(y_data(3)-y_data(2))*(x_data(3)/x_data(2))
    WRITE(*,*)cval(2),pval(2),fval(2)
    DO i=1,1000
      !save previous iteration values
      cval(1)=cval(2)
      pval(1)=pval(2)
      fval(1)=fval(2)
      !compute current iteration values
      write(*,*)'point1',cval(2),pval(2),fval(2)
      cval(2)=(y_data(1)-fval(2))/((x_data(1)-x0)**pval(2))
      write(*,*)'point2',cval(2),pval(2),fval(2)
      pval(2)=LOG((y_data(2)-fval(2))/cval(2))/LOG(x_data(2)-x0)
      write(*,*)'point3',cval(2),pval(2),fval(2)
      fval(2)=y_data(3)-cval(2)*(x_data(3)-x0)**pval(2)
      write(*,*)'point4'
      !compute errors
      cerror=(cval(2)-cval(1))/cval(2)
      perror=(pval(2)-pval(1))/pval(2)
      ferror=(fval(2)-fval(1))/fval(2)
      WRITE(*,*)i,cval(2),pval(2),fval(2),cerror,perror,ferror
      IF(cerror .LE. 1e-16 .AND. perror .LE. 1e-16 .AND. ferror .LE. 1e-16)EXIT
      !check to see if a value becomes invalid
      IF(pval(2) .LE. 0.0 .OR. ISNAN(fval(2)))THEN
        pval=0
        fval=0
        cval=0
        EXIT
      ENDIF
    ENDDO
    WRITE(*,*)cval(2),pval(2),fval(2)
    presult=pval(2)
    fresult=fval(2)
  ENDSUBROUTINE comp_rich_point_gen

ENDMODULE richardson_module
