module tmean

    use globals, only : kp                          , &
                      & nx, ny, nz                  , &
                      & hourstep, year_ini, year_fin, &
                      & initial_record, record_step , &
                      & input_fname, output_fname   , &
                      & timescale
    use fileio , only : finfo, fset, fopen, fclose, fread, fwrite
    use calendar, only : days_list
    use clock  , only : datetime, datetime_update_hourly, datetime_update_daily, set_datetime, check_datetime

    implicit none

    private
    public :: make_timemean_file

    contains


    subroutine make_timemean_file()
        integer :: year
        type(finfo) :: ifile
        type(finfo) :: ofile
        type(datetime) :: dataclock

        call set_datetime(dataclock, &  !! OUT
                        & year_ini , &  !! IN
                        & 12       , &  !! IN
                        & 1        , &  !! IN
                        & 0          )  !! IN

        call fset(ifile         , &  !! OUT
                & input_fname   , &  !! IN
                & 'read'        , &  !! IN
                & kp*nx*ny*nz   , &  !! IN
                & initial_record, &  !! IN
                & record_step     )  !! IN

        call fset(ofile         , &  !! OUT
                & output_fname  , &  !! IN
                & 'write'       , &  !! IN
                & kp*nx*ny*nz   , &  !! IN
                & 1             , &  !! IN
                & 1               )  !! IN

        !write(*,*) 'initial_record :', ifile%record
        !write(*,*) 'record_step    :', ifile%recstep

        call fopen(ifile)  !! INOUT
        call fopen(ofile)  !! INOUT

        do year = year_ini, year_fin-1
            
            call set_datetime(dataclock, &
                            & month=12   )
        
            if (timescale == 'halfmonthly') then
                call halfmonthly_1year(ifile  , &  !! INOUT
                                     & ofile  , &  !! INOUT
                                     & dataclock)  !! INOUT
            else if (timescale == 'monthly') then
                call monthly_1year(ifile  , &  !! INOUT
                                 & ofile  , &  !! INOUT
                                 & dataclock)  !! INOUT
            else if (timescale == 'DJF') then
                call DJF_1year(ifile  , &  !! INOUT
                             & ofile  , &  !! INOUT
                             & dataclock)  !! INOUT
            endif
        enddo

        call fclose(ifile)  !! INOUT
        call fclose(ofile)  !! INOUT

    end subroutine make_timemean_file


    subroutine halfmonthly_1year(ifile, ofile, dataclock)
        type(finfo)   , intent(inout) :: ifile
        type(finfo)   , intent(inout) :: ofile
        type(datetime), intent(inout) :: dataclock

        !integer :: datanum_daily
        !integer :: datanum_total
        integer, parameter :: daynum = 15
        integer, parameter :: periodnum = 90/daynum
        real(kp) :: mean(nx,ny,nz)
        integer :: t

        if (dataclock%day /= 1 .or. dataclock%hour /= 0) then
            write(*,*)
            write(*,'(a)')              'ERROR in halfmonthly_1year --------------------------------------'
            write(*,'(a)')              '|   Data starts from invalid date and time'
            write(*,'(a)')              '|   Date and time must start from YYYY/MM/dd 00:00:00,'
            write(*,'(a)',advance='no') '|   but starts from'
            call timeSignal(dataclock, 'no')
            write(*,'(a)')              ' now'
            write(*,'(a)')              '-----------------------------------------------------------------'
            error stop
        endif
        
        do t = 1, periodnum
            call timeMean(ifile               , &  !! INOUT
                        & mean(1:nx,1:ny,1:nz), &  !! OUT
                        & dataclock           , &  !! INOUT
                        & daynum=daynum         )  !! IN

            call fwrite(ofile             , &  !! INOUT
                      & mean(1:nx,1:ny,1:nz))  !! IN
        enddo

        do while (dataclock%day /= 1)
            call datetime_update_hourly(dataclock, &  !! INOUT
                                      & hourstep   )  !! IN
            ifile%record = ifile%record + ifile%recstep
        enddo

    end subroutine halfmonthly_1year


    subroutine monthly_1year(ifile, ofile, dataclock)
        type(finfo)   , intent(inout) :: ifile
        type(finfo)   , intent(inout) :: ofile
        type(datetime), intent(inout) :: dataclock

        integer, parameter :: out_month = 3
        type(datetime):: endclock
        real(kp) :: mean(nx,ny,nz)
        integer :: days(12)

        if (dataclock%day /= 1 .or. dataclock%hour /= 0) then
            write(*,*)
            write(*,'(a)')              'ERROR in monthly_1year --------------------------------------'
            write(*,'(a)')              '|   Data starts from invalid date and time'
            write(*,'(a)')              '|   Date and time must start from YYYY/MM/01 00:00:00,'
            write(*,'(a)',advance='no') '|   but starts from'
            call timeSignal(dataclock, 'no')
            write(*,'(a)')              ' now'
            write(*,'(a)')              '-----------------------------------------------------------------'
            error stop
        endif

        do while(dataclock%month /= out_month)

            call days_list(dataclock%year, &  !! IN
                         & days(1:12)      )  !! OUT
        
            call set_datetime(endclock             , &  !! OUT
                            & dataclock%year       , &  !! IN
                            & dataclock%month      , &  !! IN
                            & days(dataclock%month), &  !! IN
                            & 18                     )  !! IN

            call timeMean(ifile               , &  !! INOUT
                        & mean(1:nx,1:ny,1:nz), &  !! OUT
                        & dataclock           , &  !! INOUT
                        & endclock=endclock     )  !! IN
            
            call fwrite(ofile             , &  !! INOUT
                      & mean(1:nx,1:ny,1:nz))  !! IN

        enddo

    end subroutine monthly_1year


    subroutine DJF_1year(ifile, ofile, dataclock)
        type(finfo), intent(inout)    :: ifile
        type(finfo), intent(inout)    :: ofile
        type(datetime), intent(inout) :: dataclock

        integer, parameter :: lastmonth = 2
        type(datetime)     :: endclock
        real(kp)           :: mean(nx,ny,nz)
        integer            :: days(12)

        if (dataclock%day /= 1 .or. dataclock%hour /= 0) then
            write(*,*)
            write(*,'(a)')              'ERROR in DJF_1year --------------------------------------'
            write(*,'(a)')              '|   Data starts from invalid date and time'
            write(*,'(a)')              '|   Date and time must start from YYYY/12/01 00:00:00,'
            write(*,'(a)',advance='no') '|   but starts from'
            call timeSignal(dataclock, 'no')
            write(*,'(a)')              ' now'
            write(*,'(a)')              '-----------------------------------------------------------------'
            error stop
        endif

        call days_list(dataclock%year+1, &  !! IN
                     & days(1:12)        )  !! OUT

        call set_datetime(endclock        , &  !! OUT
                        & dataclock%year+1, &  !! IN
                        & lastmonth       , &  !! IN
                        & days(lastmonth) , &  !! IN
                        & 18                )  !! IN

        call timeMean(ifile               , &  !! INOUT
                    & mean(1:nx,1:ny,1:nz), &  !! OUT
                    & dataclock           , &  !! INOUT
                    & endclock=endclock     )  !! IN
        
        call fwrite(ofile             , &  !! INOUT
                  & mean(1:nx,1:ny,1:nz))  !! IN

    end subroutine DJF_1year


    subroutine timeMean(ftype, mean, dataclock, endclock, daynum)
        type(finfo)   , intent(inout)           :: ftype
        real(kp)      , intent(out)             :: mean(nx,ny,nz)
        type(datetime), intent(inout)           :: dataclock
        type(datetime), intent(in)   , optional :: endclock
        integer       , intent(in)   , optional :: daynum

        type(datetime) :: begin
        type(datetime) :: finish
        real(kp)       :: input_data(nx,ny,nz)
        integer        :: datanum

        mean(1:nx,1:ny,1:nz) = 0._kp
        datanum = 0

        call set_datetime(begin          , &  !! OUT
                        & dataclock%year , &  !! IN
                        & dataclock%month, &  !! IN
                        & dataclock%day  , &  !! IN
                        & dataclock%hour   )  !! IN

        if (present(endclock)) then
            call set_datetime(finish         , &  !! OUT
                            & endclock%year  , &  !! IN
                            & endclock%month , &  !! IN
                            & endclock%day   , &  !! IN
                            & endclock%hour    )  !! IN
            call datetime_update_hourly(finish  , &  !! OUT
                                      & hourstep  )  !! IN

        else if (present(daynum)) then
            call set_datetime(finish         , &  !! OUT
                            & dataclock%year , &  !! IN
                            & dataclock%month, &  !! IN
                            & dataclock%day  , &  !! IN
                            & dataclock%hour   )  !! IN
            call datetime_update_daily(finish, &  !! OUT
                                     & daynum  )  !! IN

        else
            write(*,*)
            write(*,'(a)') 'ERROR in timeMean --------------------------------------------'
            write(*,'(a)') '|   Invalid Argument'
            write(*,'(a)') '|   endclock or daynum is needed for the 4th argument'
            write(*,'(a)') '|   Note that endclock prioritized in this routine'
            write(*,'(a)') '--------------------------------------------------------------'
            error stop
        endif

        do while (dataclock%year  <= finish%year .and. &
                & dataclock%month /= finish%month .or. &
                & dataclock%day   /= finish%day   .or. &
                & dataclock%hour  /= finish%hour       )
            
            call fread(ftype                   , &  !! INOUT
                     & input_data(1:nx,1:ny,1:nz))  !! OUT

            mean(1:nx,1:ny,1:nz) = mean(1:nx,1:ny,1:nz) + input_data(1:nx,1:ny,1:nz)
            datanum = datanum + 1

            call datetime_update_hourly(dataclock, &  !! INOUT
                                      & hourstep   )  !! IN

        enddo

        mean(1:nx,1:ny,1:nz) = mean(1:nx,1:ny,1:nz) / real(datanum, kind=kp)

        write(*,'(a)',advance='no') 'Time Mean : '
        call timeSignal(begin, &      !! IN
                      & 'no'   )      !! IN
        write(*,'(a)',advance='no') ' - '
        call timeSignal(dataclock, &  !! IN
                      & 'yes'      )  !! IN

    end subroutine timeMean


    !! 'no' and 'yes' are valid as argument advance
    subroutine timeSignal(presentTime, advance)
        type(datetime), intent(in) :: presentTime
        character(*)  , intent(in) :: advance
        character(64), parameter :: fmt='(i4,2("/",i0.2)," ",i0.2,":00:00")'
        !character(64), parameter :: fmt='(i4,"/",i2.0,"/",i2.0," ",i2.0,":00:00")'

        write(*,fmt=trim(fmt),advance=advance) presentTime%year , &
                                             & presentTime%month, &
                                             & presentTime%day  , &
                                             & presentTime%hour

    end subroutine timeSignal


end module tmean

