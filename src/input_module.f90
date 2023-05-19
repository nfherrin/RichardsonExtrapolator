MODULE input_module
  USE globals
  IMPLICIT NONE
  PRIVATE
  PUBLIC read_cmd_args,read_input_file

CONTAINS

  SUBROUTINE read_cmd_args()
    INTEGER :: arg_count

    arg_count = COMMAND_ARGUMENT_COUNT()

    IF(arg_count .LT. 1)THEN
      WRITE(*,'(A)',ADVANCE='NO')'What is the name and path of your input file? '
      READ(*,'(A)')infile
    ELSE
      CALL GET_COMMAND_ARGUMENT(1,infile)
    ENDIF
  ENDSUBROUTINE read_cmd_args

  SUBROUTINE read_input_file()
    INTEGER :: t_int,i
    CHARACTER(200) :: t_char

    OPEN(UNIT=21, FILE=infile, STATUS='OLD', ACTION = "READ", IOSTAT=t_int, IOMSG=t_char)
    IF(t_int .NE. 0)THEN
      WRITE(*,*)t_char
      STOP 'error opening file!'
    ENDIF

    READ(21,*)input_type
    IF(input_type .EQ. 'equal_space')THEN
      READ(21,*)t_char,rho
    ELSE
      READ(21,*)t_char,x0
    ENDIF
    READ(21,*)t_char,data_size
    IF(data_size .LT. 3)STOP 'Richardson extrapolation requires at least 3 data points! Less than that given'
    ALLOCATE(in_y(data_size),rich_results(data_size),p_results(data_size))
    IF(input_type .EQ. 'general_space')THEN
      ALLOCATE(in_x(data_size))
    ENDIF
    in_y=0.0
    rich_results=0.0
    p_results=0.0

    DO i=1,data_size
      IF(input_type .EQ. 'equal_space')THEN
        READ(21,*,IOSTAT=t_int)in_y(i)
      ELSEIF(input_type .EQ. 'general_space')THEN
        READ(21,*,IOSTAT=t_int)in_x(i),in_y(i)
      ELSE
        WRITE(*,'(3A)')'ERROR: ',TRIM(input_type),' not  a valid input type'
        STOP 'Fatal Error'
      ENDIF
      IF(t_int .NE. 0)STOP 'end of file reached before all data found. is the data size right?'
    ENDDO

    CLOSE(21)
  ENDSUBROUTINE read_input_file

ENDMODULE input_module
