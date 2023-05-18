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

    DO i=3,data_size
      CALL comp_rich_point_gen(in_x(i-2:i),in_y(i-2:i),p,rich)
      IF(p .GT. 0.0D0 .AND. .NOT. ISNAN(p))THEN
        valid_p=.TRUE.
        rich_results(i)=rich
        p_results(i)=p
      ENDIF
    ENDDO
    IF(.NOT. valid_p)STOP 'No real p values computed. System is likely not in asymptotic region.'
  ENDSUBROUTINE compute_rich_general


  SUBROUTINE comp_rich_point_gen(x_data,y_data,presult,fresult)
    REAL(8), INTENT(IN) :: x_data(3),y_data(3)
    REAL(8), INTENT(OUT) :: presult,fresult
    REAL(8) :: cval(2),pval(2),fval(2)
    REAL(8) :: cerror,perror,ferror
    INTEGER :: i,order(3),temp_i,switch_1,switch_2

    !get order of furthest from 1 to closest
    order(1)=1
    order(2)=2
    order(3)=3
    !swap 2 and 3 if distance for 2 is 1
    IF(ABS(x_data(2)-x0-1) .LE. 1e-12)THEN
      order(2)=3
      order(3)=2
    ENDIF

    !initial guesses
    cval=1
    pval=1
    fval=y_data(3)+(y_data(3)-y_data(2))*(x_data(3)/x_data(2))
    DO i=1,1000
      !save previous iteration values
      cval(1)=cval(2)
      pval(1)=pval(2)
      fval(1)=fval(2)
      !compute current iteration values
      cval(2)=(y_data(order(1))-fval(2))/((x_data(order(1))-x0)**pval(2))
      pval(2)=LOG((y_data(order(2))-fval(2))/cval(2))/LOG(x_data(order(2))-x0)
      fval(2)=y_data(order(3))-cval(2)*(x_data(order(3))-x0)**pval(2)
      !compute errors
      cerror=ABS((cval(2)-cval(1))/cval(2))
      perror=ABS((pval(2)-pval(1))/pval(2))
      ferror=ABS((fval(2)-fval(1))/fval(2))
      IF(cerror .LE. 1e-16 .AND. perror .LE. 1e-16 .AND. ferror .LE. 1e-16)EXIT
      !check to see if a value becomes nans
      IF(ISNAN(fval(2)) .OR. ISNAN(pval(2)) .OR. ISNAN(cval(2)))THEN
        pval(2)=pval(1)
        fval(2)=fval(1)
        cval(2)=cval(1)
        !perturb order
        switch_1=1+FLOOR(RAND()*3)
        switch_2=1+FLOOR(RAND()*3)
        temp_i=order(switch_1)
        order(switch_1)=order(switch_2)
        order(switch_2)=temp_i
        IF(ABS(x_data(order(2))-x0-1) .LE. 1e-12)THEN
          temp_i=order(2)
          order(2)=order(3)
          order(3)=temp_i
        ENDIF
        CYCLE
      ENDIF
      !cycle if pval gets out of control
      IF(ABS(pval(2)) .GT. 400 .OR. ABS(cval(2)) .GE. 1e+20)THEN
        cval=1
        pval=1
        fval=y_data(3)+(y_data(3)-y_data(2))*(x_data(3)/x_data(2))
        !perturb order
        switch_1=1+FLOOR(RAND()*3)
        switch_2=1+FLOOR(RAND()*3)
        temp_i=order(switch_1)
        order(switch_1)=order(switch_2)
        order(switch_2)=temp_i
        IF(ABS(x_data(order(2))-x0-1) .LE. 1e-12)THEN
          temp_i=order(2)
          order(2)=order(3)
          order(3)=temp_i
        ENDIF
        CYCLE
      ENDIF
    ENDDO
    !check if it actually converged
    IF(i .GE. 999)THEN
      cval=0
      pval=0
      fval=0
    ENDIF
    presult=pval(2)
    fresult=fval(2)
  ENDSUBROUTINE comp_rich_point_gen

ENDMODULE richardson_module
