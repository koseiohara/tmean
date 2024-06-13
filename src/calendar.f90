module calendar

    implicit none

    private
    public :: days_list

    contains


    subroutine days_list(year, list)
        integer, intent(in)  :: year
        integer, intent(out) :: list(12)

        if (leapYear(year)) then
            ! for leap year
            ! Month :       1   2   3   4   5   6   7   8   9  10  11  12
            list(1:12) = (/31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
        else
            ! for non-leap year
            ! Month :       1   2   3   4   5   6   7   8   9  10  11  12
            list(1:12) = (/31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31/)
        endif

    end subroutine days_list


    ! True if year is a leap year, False if year is not a leap year
    function leapYear(year) result(output)
        integer, intent(in) :: year
        logical             :: output

        if (mod(year, 4) /= 0) then
            output = .False.
            return
        else if (mod(year, 100) == 0 .and. mod(year, 400) /= 0) then
            output = .False.
            return
        else
            output = .True.
        endif
    
    end function leapYear


end module calendar

