!RichardsonExtrapolator is licensed under the MIT License.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Richardson Extrapolation solvers.
!> @author Nicholas Herring
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
MODULE richardson_module
  IMPLICIT NONE
  PRIVATE
  PUBLIC comp_rich_es,comp_rich_gs

CONTAINS

  !---------------------------------------------------------------------------------------------------
  !> @brief This subroutine computes the richardson extrapolation for a given rho and set of 3 y points
  !>         solves the extrapolation using the standard method for equal spaced richardson extrapolation
  !> @param rho_val - Rho value for spacing of point distances
  !> @param y_p - y points to take the extrapolation of
  !> @param rich_comp - computed richardson extrapolation value. 0 when unsuccessful
  !> @param p_comp - computed convergence rate value. 0 when unsuccessful
  !>
  SUBROUTINE comp_rich_es(rho_val,y_p,rich_comp,p_comp)
    REAL(8), INTENT(IN) :: rho_val,y_p(3)
    REAL(8), INTENT(OUT) :: rich_comp,p_comp
    REAL(8) :: p

    rich_comp=0
    p_comp=0
    p=(y_p(3)-y_p(2))/(y_p(2)-y_p(1))
    IF(p .GT. 0.0D0)THEN
      p=LOG(p)/LOG(rho_val)
      rich_comp=(y_p(3)-rho_val**p*y_p(2))/(1.0D0-rho_val**p)
      p_comp=p
    ENDIF
  ENDSUBROUTINE comp_rich_es

  !---------------------------------------------------------------------------------------------------
  !> @brief This subroutine computes a generally spaced richardson extrapolation
  !>         solves the extrapolation using a "Gauss-Seidel"-like iterative scheme
  !> @param x_p - x points for each y point to take the extrapolation of
  !> @param x0_p - x goal point we are computing to
  !> @param y_p - y points to take the extrapolation of
  !> @param rich_comp - computed richardson extrapolation value. 0 when unsuccessful
  !> @param p_comp - computed convergence rate value. 0 when unsuccessful
  !>
  SUBROUTINE comp_rich_gs(x_p,x0_p,y_p,rich_comp,p_comp)
    REAL(8), INTENT(IN) :: x_p(3),y_p(3),x0_p
    REAL(8), INTENT(OUT) :: rich_comp,p_comp
    REAL(8) :: cval(2),pval(2),fval(2)
    REAL(8) :: cerror,perror,ferror
    INTEGER :: i,order(3),temp_i,switch_1,switch_2

    !get order of furthest from 1 to closest
    order(1)=1
    order(2)=2
    order(3)=3
    !swap 2 and 3 if distance for 2 is 1
    IF(ABS(ABS(x_p(2)-x0_p)-1) .LE. 1e-12)THEN
      order(2)=3
      order(3)=2
    ENDIF

    !initial guesses
    cval=1
    pval=1
    fval=y_p(3)+(y_p(3)-y_p(2))*(x_p(3)/x_p(2))
    DO i=1,10000
      !save previous iteration values
      cval(1)=cval(2)
      pval(1)=pval(2)
      fval(1)=fval(2)
      !compute current iteration values
      cval(2)=(y_p(order(1))-fval(2))/(ABS(x_p(order(1))-x0_p)**pval(2))
      pval(2)=LOG((y_p(order(2))-fval(2))/cval(2))/LOG(ABS(x_p(order(2))-x0_p))
      fval(2)=y_p(order(3))-cval(2)*ABS(x_p(order(3))-x0_p)**pval(2)
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
        IF(ABS(ABS(x_p(order(2))-x0_p)-1) .LE. 1e-12)THEN
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
        fval=y_p(3)+(y_p(3)-y_p(2))*(x_p(3)/x_p(2))
        !perturb order
        switch_1=1+FLOOR(RAND()*3)
        switch_2=1+FLOOR(RAND()*3)
        temp_i=order(switch_1)
        order(switch_1)=order(switch_2)
        order(switch_2)=temp_i
        IF(ABS(ABS(x_p(order(2))-x0_p)-1) .LE. 1e-12)THEN
          temp_i=order(2)
          order(2)=order(3)
          order(3)=temp_i
        ENDIF
        CYCLE
      ENDIF
    ENDDO
    !check if it actually converged
    IF(i .GE. 9999)THEN
      cval=0
      pval=0
      fval=0
    ENDIF
    p_comp=pval(2)
    rich_comp=fval(2)
  ENDSUBROUTINE comp_rich_gs

ENDMODULE richardson_module
