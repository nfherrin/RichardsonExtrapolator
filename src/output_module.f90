MODULE output_module
  USE globals
  IMPLICIT NONE
  PRIVATE
  PUBLIC output_rich

CONTAINS

  SUBROUTINE output_rich()
    INTEGER :: i

    WRITE(*,'(A)')""
    WRITE(*,'(A)')'Extrapolation successful for at least 1 point!'
    WRITE(*,'(A)')"Note that if the later extrapolation results aren't close..."
    WRITE(*,'(A)')"...then the input data may be outside the asymptotic region"
    WRITE(*,'(A)')"Note that Richard Extrapolation is not valid outside the asymptotic region"
    WRITE(*,'(A)')""
    WRITE(*,'(A)')'----------------------------------------------'
    WRITE(*,'(A)')'   Extrapolation Result  | Computed Convergence Order | Data Points Used'
    DO i=1,data_size
      IF(ABS(rich_results(i)) .GE. 1.0D-15)THEN
        WRITE(*,'(ES24.16,A,ES24.16,A,I0,A,I0,A,I0)')rich_results(i),' | ',p_results(i),'   | ',i-2,' ',i-1,' ',i
      ENDIF
    ENDDO
    WRITE(*,'(A)')'----------------------------------------------'
  ENDSUBROUTINE output_rich

ENDMODULE output_module
