module namelist

    use globals, only : nx, ny, nz                  , &
                      & hourstep, year_ini, year_fin, &
                      & initial_record, record_step , &
                      & input_fname, output_fname   , &
                      & timescale

    implicit none

    private
    public :: read_nml

    contains


    subroutine read_nml()
        integer :: grid_unit
        character(64) :: grid_fname='../nml/grid.nml'
        integer :: tdef_unit
        character(64) :: tdef_fname='../nml/tdef.nml'
        integer :: record_unit
        character(64) :: record_fname='../nml/record.nml'
        integer :: files_unit
        character(64) :: files_fname='../nml/files.nml'
        integer :: tscale_unit
        character(64) :: tscale_fname='../nml/tscale.nml'

        NAMELIST / GRID / nx, ny, nz
        NAMELIST / TDEF / hourstep, year_ini, year_fin
        NAMELIST / RECORD / initial_record, record_step
        NAMELIST / FILES / input_fname, output_fname
        NAMELIST / TSCALE / timescale

        nx = 0
        ny = 0
        nz = 0

        hourstep = 0
        year_ini = 0
        year_fin = 0

        initial_record = 0
        record_step    = 0
        
        input_fname  = ''
        output_fname = ''

        timescale = ''

        call nml_open(grid_unit, grid_fname)
        read(grid_unit,NML=GRID)
        close(grid_unit)

        call nml_open(tdef_unit, tdef_fname)
        read(tdef_unit,NML=TDEF)
        close(tdef_unit)

        call nml_open(record_unit, record_fname)
        read(record_unit, NML=RECORD)
        close(record_unit)

        call nml_open(files_unit, files_fname)
        read(files_unit,NML=FILES)
        close(files_unit)

        call nml_open(tscale_unit, tscale_fname)
        read(tscale_unit,NML=TSCALE)
        close(tscale_unit)

        call checker()

    end subroutine read_nml
    

    subroutine nml_open(unit, fname)
        integer     , intent(out) :: unit
        character(*), intent(in)  :: fname
        
        integer :: stat

        open(NEWUNIT=unit, FILE=fname, ACTION='READ', IOSTAT=stat)
        if (stat/=0) then
            write(*,*)
            write(*,'(a)') 'ERROR in nml_open -------------------------------------'
            write(*,'(a)') '|   Failed to open ' // trim(fname)
            write(*,'(a)') '-------------------------------------------------------'
            error stop
        endif

    end subroutine nml_open


    subroutine checker()
        
        if (nx <= 0) then
            write(*,*)
            write(*,'(a)')      'ERROR in read_nml -------------------------------------'
            write(*,'(a)')      '|   Invalid nx value in namelist'
            write(*,'(a,i0,a)') '|   nx must be 1 or more, but nx=', nx, ' now'
            write(*,'(a)')      '-------------------------------------------------------'
            error stop
        endif

        if (ny <= 0) then
            write(*,*)
            write(*,'(a)')      'ERROR in read_nml -------------------------------------'
            write(*,'(a)')      '|   Invalid ny value in namelist'
            write(*,'(a,i0,a)') '|   ny must be 1 or more, but ny=', ny, ' now'
            write(*,'(a)')      '-------------------------------------------------------'
            error stop
        endif

        if (nz <= 0) then
            write(*,*)
            write(*,'(a)')      'ERROR in read_nml -------------------------------------'
            write(*,'(a)')      '|   Invalid nz value in namelist'
            write(*,'(a,i0,a)') '|   nz must be 1 or more, but nz=', nz, ' now'
            write(*,'(a)')      '-------------------------------------------------------'
            error stop
        endif

        if (hourstep <= 0) then
            write(*,*)
            write(*,'(a)')      'ERROR in read_nml -------------------------------------'
            write(*,'(a)')      '|   Invalid hourstep value in namelist'
            write(*,'(a,i0,a)') '|   hourstep must be 1 or more, but houestep=', hourstep, ' now'
            write(*,'(a)')      '-------------------------------------------------------'
            error stop
        endif

        if (year_ini < 1800 .or. year_ini > 2200) then
            write(*,*)
            write(*,'(a)')      'ERROR in read_nml -------------------------------------'
            write(*,'(a)')      '|   Invalid year_ini value in namelist'
            write(*,'(a,i0,a)') '|   year_ini must be 1800<=year_ini<=2200, but year_ini=', year_ini, ' now'
            write(*,'(a)')      '-------------------------------------------------------'
            error stop
        endif

        if (year_fin < 1800 .or. year_fin > 2200) then
            write(*,*)
            write(*,'(a)')      'ERROR in read_nml -------------------------------------'
            write(*,'(a)')      '|   Invalid year_fin value in namelist'
            write(*,'(a,i0,a)') '|   year_fin must be 1800<=year_fin<=2200, but year_fin=', year_fin, ' now'
            write(*,'(a)')      '-------------------------------------------------------'
            error stop
        endif

        if (year_ini >= year_fin) then
            write(*,*)
            write(*,'(a)')    'ERROR in read_nml -------------------------------------'
            write(*,'(a)')    '|   Invalid year_ini or year_fin value in namelist'
            write(*,'(a)')    '|   year_ini must be less than year_fin'
            write(*,'(a,i0)') '|       year_ini=', year_ini
            write(*,'(a,i0)') '|       year_fin=', year_fin
            write(*,'(a)')    '-------------------------------------------------------'
            error stop
        endif

        if (initial_record <= 0) then
            write(*,*)
            write(*,'(a)')      'ERROR in read_nml -------------------------------------'
            write(*,'(a)')      '|   Invalid initial_record value in namelist'
            write(*,'(a,i0,a)') '|   initial_record must be 1 or more, but initial_record=', initial_record, ' now'
            write(*,'(a)')      '-------------------------------------------------------'
            error stop
        endif

        if (record_step <= 0) then
            write(*,*)
            write(*,'(a)')      'ERROR in read_nml -------------------------------------'
            write(*,'(a)')      '|   Invalid record_step value in namelist'
            write(*,'(a,i0,a)') '|   record_step must be 1 or more, but record_step=', record_step, ' now'
            write(*,'(a)')      '-------------------------------------------------------'
            error stop
        endif

        if (input_fname == '') then
            write(*,*)
            write(*,'(a)') 'ERROR in read_nml -------------------------------------'
            write(*,'(a)') '|   Invalid input_fname in namelist'
            write(*,'(a)') '|   input_fname is not defiened'
            write(*,'(a)') '-------------------------------------------------------'
            error stop
        endif

        if (output_fname == '') then
            write(*,*)
            write(*,'(a)') 'ERROR in read_nml -------------------------------------'
            write(*,'(a)') '|   Invalid output_fname in namelist'
            write(*,'(a)') '|   output_fname is not defiened'
            write(*,'(a)') '-------------------------------------------------------'
            error stop
        endif

        if (timescale == '') then
            write(*,*)
            write(*,'(a)') 'ERROR in read_nml -------------------------------------'
            write(*,'(a)') '|   Invalid timescale in namelist'
            write(*,'(a)') '|   timescale is not defiened'
            write(*,'(a)') '-------------------------------------------------------'
            error stop
        endif

    end subroutine checker


end module namelist

