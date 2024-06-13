module globals

    implicit none

    integer, parameter :: kp = 4

    integer, save :: nx
    integer, save :: ny
    integer, save :: nz

    integer, save :: hourstep
    integer, save :: year_ini
    integer, save :: year_fin

    integer, save :: initial_record
    integer, save :: record_step
    
    character(128), save :: input_fname
    character(128), save :: output_fname

    character(32) , save :: timescale

end module globals

