PROGRAM richardson_extrapolator
  USE globals
  USE input_module
  USE output_module
  USE richardson_module
  IMPLICIT NONE

  CALL read_cmd_args()

  CALL read_input_file()

  CALL compute_rich()

  CALL output_rich()

ENDPROGRAM richardson_extrapolator
