program main

    use namelist, only : read_nml
    use tmean, only : make_timemean_file

    implicit none

    call read_nml()

    call make_timemean_file()

end program main

