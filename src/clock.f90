module clock

    use calendar, only : days_list

    implicit none

    private
    public :: datetime, datetime_update_hourly, datetime_update_daily, set_datetime, check_datetime


    type datetime
        integer :: year
        integer :: month
        integer :: day
        integer :: hour
    end type datetime

    contains


    ! Update datetime data for hourly data
    subroutine datetime_update_hourly(dt, hstep)
        type(datetime), intent(inout) :: dt
        integer                       :: hstep
        integer                       :: carry
        integer                       :: daynumList(12)

        if (hstep <= 0) then
            write(*,*)
            write(*,'(a)')      'ERROR in datetime_update_hourly ----------------------------------'
            write(*,'(a)')      '|   Invalid argument in hstep'
            write(*,'(a,i0,a)') '|   hstep must be 1 or more, but hstep=', hstep, ' now'
            write(*,'(a)')      '------------------------------------------------------------------'
            error stop
        endif

        call days_list(dt%year, daynumList)

        dt%hour = dt%hour + hstep
        carry   = dt%hour/24
        dt%hour = dt%hour - carry*24
        
        dt%day = dt%day + carry
        if (dt%day == daynumList(dt%month)) then
            carry = 0
        else
            carry = dt%day/daynumList(dt%month)
        endif
        dt%day = dt%day - carry*(daynumList(dt%month))

        dt%month = dt%month + carry
        if (dt%month == 12) then
            carry = 0
        else
            carry = dt%month/12
        endif
        dt%month = dt%month - carry*12

        dt%year = dt%year + carry

    end subroutine datetime_update_hourly


    subroutine datetime_update_daily(dt, dstep)
        type(datetime), intent(inout) :: dt
        integer       , intent(in)    :: dstep
        integer :: carry
        integer :: daynumList(12)
 
        if (dstep <= 0) then
            write(*,*)
            write(*,'(a)')      'ERROR in datetime_update_daily -----------------------------------'
            write(*,'(a)')      '|   Invalid argument in dstep'
            write(*,'(a,i0,a)') '|   dstep must be 1 or more, but dstep=', dstep, ' now'
            write(*,'(a)')      '------------------------------------------------------------------'
            error stop
        endif

        call days_list(dt%year, daynumList)
 
        dt%day = dt%day + dstep
        if (dt%day == daynumList(dt%month)) then
            carry = 0
        else
            carry = dt%day/daynumList(dt%month)
        endif
        dt%day = dt%day - carry*(daynumList(dt%month))
 
        dt%month = dt%month + carry
        if (dt%month == 12) then
            carry = 0
        else
            carry = dt%month/12
        endif
        dt%month = dt%month - carry*12
 
        dt%year = dt%year + carry
 
    end subroutine datetime_update_daily


    subroutine set_datetime(new, year, month, day, hour)
        type(datetime), intent(out)          :: new
        integer,        intent(in), optional :: year
        integer,        intent(in), optional :: month
        integer,        intent(in), optional :: day
        integer,        intent(in), optional :: hour

        integer :: list(12)

        if (present(year)) then
            if (year < 1) then
                write(*,*) 'ERROR in set_datetime --------------------------'
                write(*,*) '|   Invalid year value'
                write(*,*) '------------------------------------------------'
                error stop
            endif
            new%year  = year

        endif

        if (present(month)) then
            if (month < 1 .or. month > 12) then
                write(*,*) 'ERROR in set_datetime --------------------------'
                write(*,*) '|   Invalid month value'
                write(*,*) '------------------------------------------------'
                error stop
            endif
            new%month = month
        endif

        call days_list(new%year, list(1:12))

        if (present(day)) then
            if (day < 1 .or. day > list(month)) then
                write(*,*) 'ERROR in set_datetime --------------------------'
                write(*,*) '|   Invalid day value'
                write(*,*) '------------------------------------------------'
                error stop
            endif
            new%day   = day
        endif
        
        if (present(hour)) then
            if (hour < 0 .or. hour >= 24) then
                write(*,*) 'ERROR in set_datetime --------------------------'
                write(*,*) '|   Invalid hour value'
                write(*,*) '------------------------------------------------'
                error stop
            endif
            new%hour  = hour
        endif
        
    end subroutine set_datetime


    subroutine check_datetime(present, year, month, day, hour)
        type(datetime), intent(in) :: present
        integer,        intent(in) :: year
        integer,        intent(in) :: month
        integer,        intent(in) :: day
        integer,        intent(in) :: hour

        if (present%year/=year .or. present%month/=month .or. present%day/=day .or. present%hour/=hour) then
            write(*,'(a)')               'ERROR in check_datetime ------------------------'
            write(*,'(a)')               '|   Unexpexted datetime value'
            write(*,'(a,i4,10(a,i2.0))') '|   Expected datetime is ', year, '/', month, '/', day, '/', hour
            write(*,'(a,i4,10(a,i2.0))') '|    Present datetime is ', present%year , '/', &
                                                                    & present%month, '/', &
                                                                    & present%day  , '/', &
                                                                    & present%hour
            write(*,'(a)')               '------------------------------------------------'
            error stop
        endif

    end subroutine check_datetime


end module clock

