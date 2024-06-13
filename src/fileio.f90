module fileio

    use NaNchecker, only : isNaN

    implicit none

    private
    public :: finfo, fset, fopen, fclose, fread, fwrite

    type finfo
        integer        :: unit
        character(128) :: fname
        character(16)  :: action
        integer        :: recl
        integer        :: record
        integer        :: recstep
    end type finfo


    interface fread
        module procedure &
          & fread1d_s, &
          & fread1d_d, &
          & fread2d_s, &
          & fread2d_d, &
          & fread3d_s, &
          & fread3d_d
    end interface fread


    interface fwrite
        module procedure &
          & fwrite1d_s, &
          & fwrite1d_d, &
          & fwrite2d_s, &
          & fwrite2d_d, &
          & fwrite3d_s, &
          & fwrite3d_d
    end interface fwrite


    contains


    subroutine fset(ftype, fname, action, recl, record, recstep)
        type(finfo) , intent(out) :: ftype
        character(*), intent(in)  :: fname
        character(*), intent(in)  :: action
        integer     , intent(In)  :: recl
        integer     , intent(in)  :: record
        integer     , intent(in)  :: recstep

        ftype%fname   = trim(fname)
        ftype%action  = trim(action)
        ftype%recl    = recl
        ftype%record  = record
        ftype%recstep = recstep

    end subroutine fset


    subroutine fopen(ftype)
        type(finfo), intent(inout) :: ftype
        integer                    :: stat

        OPEN(NEWUNIT=ftype%unit , &
           & FILE=ftype%fname   , &
           & ACTION=ftype%action, &
           & FORM='UNFORMATTED' , &
           & ACCESS='DIRECT'    , &
           & RECL=ftype%recl    , &
           & IOSTAT=stat          )

        if (stat/=0) then
            write(*,*)
            write(*,'(a)')    'ERROR in fopen -------------------------------'
            write(*,'(a)')    '|   Failed to open ' // trim(ftype%fname)
            write(*,'(a,i0)') '|       UNIT   : ', ftype%unit
            write(*,'(a)')    '|       ACTION : ' // trim(ftype%action)
            write(*,'(a,i0)') '|       RECL   : ', ftype%recl
            write(*,'(a,i0)') '|       RECORD : ', ftype%record
            write(*,'(a)')    '----------------------------------------------'
            error stop
        endif

    end subroutine fopen


    subroutine fclose(ftype)
        type(finfo), intent(inout) :: ftype

        CLOSE(ftype%unit)

        ftype%unit    = 0
        ftype%fname   = 'ERROR'
        ftype%action  = 'ERROR'
        ftype%recl    = 0
        ftype%record  = 0
        ftype%recstep = 0

    end subroutine fclose


    subroutine fread1d_s(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: container(:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) container(:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread1d_s


    subroutine fread1d_d(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(out)   :: container(:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) container(:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread1d_d


    subroutine fread2d_s(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: container(:,:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) container(:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread2d_s


    subroutine fread2d_d(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(out)   :: container(:,:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) container(:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread2d_d


    subroutine fread3d_s(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(out)   :: container(:,:,:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) container(:,:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread3d_s


    subroutine fread3d_d(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(out)   :: container(:,:,:)

        call check_opened(ftype)

        read(ftype%unit,rec=ftype%record) container(:,:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fread3d_d


    subroutine fwrite1d_s(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(in)    :: container(:)

        call check_opened(ftype)

        if (isNaN(container(:))) then
            write(*,*)
            write(*,'(a)')    'WARNING in fwrite ------------------------------'
            write(*,'(a)')    '|   NaN is found in output data'
            write(*,'(a)')    '|   FILE NAME : ' // trim(ftype%fname)
            write(*,'(a,i0)') '|   UNIT      : ', ftype%unit
            write(*,'(a)')    '------------------------------------------------'
            write(*,*)
        endif

        write(ftype%unit,rec=ftype%record) container(:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite1d_s


    subroutine fwrite1d_d(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(in)    :: container(:)

        call check_opened(ftype)

        if (isNaN(container(:))) then
            write(*,*)
            write(*,'(a)')    'WARNING in fwrite ------------------------------'
            write(*,'(a)')    '|   NaN is found in output data'
            write(*,'(a)')    '|   FILE NAME : ' // trim(ftype%fname)
            write(*,'(a,i0)') '|   UNIT      : ', ftype%unit
            write(*,'(a)')    '------------------------------------------------'
            write(*,*)
        endif

        write(ftype%unit,rec=ftype%record) container(:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite1d_d


    subroutine fwrite2d_s(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(in)    :: container(:,:)

        call check_opened(ftype)

        if (isNaN(container(:,:))) then
            write(*,*)
            write(*,'(a)')    'WARNING in fwrite ------------------------------'
            write(*,'(a)')    '|   NaN is found in output data'
            write(*,'(a)')    '|   FILE NAME : ' // trim(ftype%fname)
            write(*,'(a,i0)') '|   UNIT      : ', ftype%unit
            write(*,'(a)')    '------------------------------------------------'
            write(*,*)
        endif

        write(ftype%unit,rec=ftype%record) container(:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite2d_s


    subroutine fwrite2d_d(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(in)    :: container(:,:)

        call check_opened(ftype)

        if (isNaN(container(:,:))) then
            write(*,*)
            write(*,'(a)')    'WARNING in fwrite ------------------------------'
            write(*,'(a)')    '|   NaN is found in output data'
            write(*,'(a)')    '|   FILE NAME : ' // trim(ftype%fname)
            write(*,'(a,i0)') '|   UNIT      : ', ftype%unit
            write(*,'(a)')    '------------------------------------------------'
            write(*,*)
        endif

        write(ftype%unit,rec=ftype%record) container(:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite2d_d


    subroutine fwrite3d_s(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(4)    , intent(in)    :: container(:,:,:)

        call check_opened(ftype)

        if (isNaN(container(:,:,:))) then
            write(*,*)
            write(*,'(a)')    'WARNING in fwrite ------------------------------'
            write(*,'(a)')    '|   NaN is found in output data'
            write(*,'(a)')    '|   FILE NAME : ' // trim(ftype%fname)
            write(*,'(a,i0)') '|   UNIT      : ', ftype%unit
            write(*,'(a)')    '------------------------------------------------'
            write(*,*)
        endif

        write(ftype%unit,rec=ftype%record) container(:,:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite3d_s


    subroutine fwrite3d_d(ftype, container)
        type(finfo), intent(inout) :: ftype
        real(8)    , intent(in)    :: container(:,:,:)

        call check_opened(ftype)

        if (isNaN(container(:,:,:))) then
            write(*,*)
            write(*,'(a)')    'WARNING in fwrite ------------------------------'
            write(*,'(a)')    '|   NaN is found in output data'
            write(*,'(a)')    '|   FILE NAME : ' // trim(ftype%fname)
            write(*,'(a,i0)') '|   UNIT      : ', ftype%unit
            write(*,'(a)')    '------------------------------------------------'
            write(*,*)
        endif

        write(ftype%unit,rec=ftype%record) container(:,:,:)
        ftype%record = ftype%record + ftype%recstep

    end subroutine fwrite3d_d


    subroutine check_opened(ftype)
        type(finfo), intent(in) :: ftype
        logical                 :: opened

        INQUIRE(UNIT=ftype%unit, &
              & OPENED=opened    )

        if (.NOT. opened) then
            write(*,*)
            write(*,'(a)')    'ERROR in fread/fwrite ------------------------'
            write(*,'(a)')    '|   File is not opened'
            write(*,'(a,i0)') '|       UNIT   : ', ftype%unit
            write(*,'(a)')    '|       ACTION : ' // trim(ftype%action)
            write(*,'(a,i0)') '|       RECL   : ', ftype%recl
            write(*,'(a,i0)') '|       RECORD : ', ftype%record
            write(*,'(a)')    '----------------------------------------------'
            error stop
        endif

    end subroutine check_opened


end module fileio

