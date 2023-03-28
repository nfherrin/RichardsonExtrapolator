MODULE richardson_module
  USE globals
  IMPLICIT NONE
  PRIVATE
  PUBLIC compute_rich_rho

CONTAINS

  SUBROUTINE compute_rich_rho()
    INTEGER :: i
    REAL(8) :: p
    LOGICAL :: valid_p=.FALSE.

    DO i=3,data_size
      p=(indata(i)-indata(i-1))/(indata(i-1)-indata(i-2))
      IF(p .GT. 0.0D0)THEN
        valid_p=.TRUE.
        p=LOG(p)/LOG(rho)
        rich_results(i)=(indata(i)-rho**p*indata(i-1))/(1.0D0-rho**p)
        p_results(i)=p
      ENDIF
    ENDDO
    IF(.NOT. valid_p)STOP 'No real p values computed. System is likely not in asymptotic region.'
  ENDSUBROUTINE compute_rich_rho

ENDMODULE richardson_module
