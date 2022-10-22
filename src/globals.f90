MODULE globals
  IMPLICIT NONE

  CHARACTER(200) :: infile

  REAL(8) :: rho

  INTEGER :: data_size

  REAL(8),ALLOCATABLE :: indata(:),rich_results(:)
ENDMODULE globals