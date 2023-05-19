PROGRAM richardson_extrapolator
  USE globals
  USE input_module
  USE output_module
  USE richardson_module
  IMPLICIT NONE

  CALL read_cmd_args()

  CALL read_input_file()

  IF(input_type .EQ. 'equal_space')THEN
    CALL compute_rich_rho()
  ELSEIF(input_type .EQ. 'general_space')THEN
    CALL compute_rich_general()
  ENDIF

  CALL output_rich()

ENDPROGRAM richardson_extrapolator
