!RichardsonExtrapolator is licensed under the MIT License.
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
!> @brief Primary driver module to perform all richardson extrapolations.
!> @author Nicholas Herring
!++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++!
PROGRAM richardson_extrapolator
  USE globals
  USE input_module
  USE output_module
  USE richardson_module
  IMPLICIT NONE
  INTEGER :: i
  LOGICAL :: valid_p=.FALSE.

  CALL read_cmd_args()

  CALL read_input_file()

  !compute all the richardson extrapolations
  DO i=3,data_size
    IF(input_type .EQ. 'equal_space')THEN
      !compute the equally spaced richardson extrapolation
      CALL comp_rich_es(rho,in_y(i-2:i),rich_results(i),p_results(i))
    ELSEIF(input_type .EQ. 'general_space')THEN
      !compute the generally spaced richardson extrapolation
      CALL comp_rich_gs(in_x(i-2:i),x0,in_y(i-2:i),rich_results(i),p_results(i))
    ENDIF
    !check if there has been a valid p value
    IF(ABS(p_results(i)) .GT. 0)valid_p=.TRUE.
  ENDDO
  IF(.NOT. valid_p)STOP 'No real p values computed. System is likely not in asymptotic region.'

  CALL output_rich()

ENDPROGRAM richardson_extrapolator
