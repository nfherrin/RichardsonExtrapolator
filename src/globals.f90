MODULE globals
  IMPLICIT NONE

  CHARACTER(200) :: infile,input_type

  REAL(8) :: rho,x0

  INTEGER :: data_size

  REAL(8),ALLOCATABLE :: in_y(:),in_x(:),rich_results(:),p_results(:)
ENDMODULE globals