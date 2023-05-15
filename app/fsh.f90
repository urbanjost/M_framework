program testtype
! evaluate an interactive environment wrapped around fpm(1) and M_framework(1)
use, intrinsic :: iso_fortran_env, only: ERROR_UNIT
use M_framework, only: str, wrt
implicit none
character(len=*), parameter :: g = '(*(g0,1x))'
integer :: icase
integer :: ios
integer :: iostat
character(len=:),allocatable  :: string
character(len=256) :: line
character(len=256) :: iomsg
character(len=256) :: answer
integer            :: lun
character(len=:),allocatable :: strlevel
integer            :: level=0;         namelist /args/ level
character(len=80)  :: compiler='' ;    namelist /args/ compiler
character(len=80)  :: profile='' ;     namelist /args/ profile
character(len=80)  :: link_flag='' ;   namelist /args/ link_flag
character(len=80)  :: flag='' ;        namelist /args/ flag
character(len=80)  :: cxx_flag='';     namelist /args/ cxx_flag
character(len=80)  :: cxx_compiler=''; namelist /args/ cxx_compiler
character(len=80)  :: c_flag='';       namelist /args/ c_flag
character(len=80)  :: c_compiler='';   namelist /args/ c_compiler
character(len=80)  :: archiver='';     namelist /args/ archiver
character(len=:),allocatable :: options

   options=' --compiler gfortran --profile debug'
   strlevel='level=0'
   write (*, g) ' try different fpm(1) test commands, setting values via NAMELIST:'
   write (*, g) ' Enter command ("help" for more information):'
   INFINITE: do
      write (*,g, iostat=ios, iomsg=iomsg, advance='no') 'fsh>'
      read (*, '(a)', iostat=ios, iomsg=iomsg) line
      if (ios .ne. 0) then
         write (*, g) trim(iomsg)
         cycle INFINITE
      endif
      if (adjustl(line(1:1)) .eq. '#') cycle
      select case (line)

      case ('test')
         string=str('fpm test --target ''*'' ',options,' -- luns=6',strlevel)
         call run(string)
      case ('brief')
         string=str('fpm test --target ''*'' ',options,' -- luns=6 brief',strlevel)
         call run(string)
      case ('build')
         string=str('fpm build                ',options,' -- luns=6',strlevel)
         call run(string)
      case ('run')
         call run(str('fpm run --target ''*'' ',options,' -- luns=6 brief',strlevel))
      case ('example')
         call run(str('fpm run --target ''*'' ',options,' --example -- luns=6 brief',strlevel))
      case ('valgrind')
         call run(str('fpm test --target ''*'' ',options, &
         & ' --runner ''valgrind'' -- luns=6 brief interactive ',strlevel))
      case ('valgrind+')
         call run(str('fpm test --target ''*'' ',options, &
         & ' --runner ''valgrind --leak-check=full '' -- luns=6 brief interactive ',strlevel))
      case ('debug')
         call run(str('fpm test --target ''*'' ',options, &
         & ' --verbose --runner ''gdb -ex "list,0" -ex run --quiet --args'' -- luns=6 ',strlevel))
      case ('shell', 'sh')
         call run('bash')
      case ('list' )
         write(*,g)'APPLICATIONS:'
         call run('fpm run --list')
         write(*,g)'EXAMPLES:'
         call run('fpm run --example --list')
         write(*,g)'TESTS:'
         call run('fpm test --list')
      case ('q', 'stop', 'quit', 'exit', 'end','.')
         exit INFINITE
      case ('help', '?')
          write(*,'(a)') [character(len=80) :: &
          'Any unrecognized command is passed to the system. Verbs are                     ', &
          'TESTS:                                                                          ', &
          '   test      -- run "fpm test"  with current options                            ', &
          '   brief     -- run "fpm test" tests in brief mode                              ', &
          '   build     -- run "fpm build" with current options                            ', &
          '   run       -- run "fpm run" with current options                              ', &
          '   example   -- run "fpm run --example " with current options                   ', &
          '   debug     -- run tests with gdb(1)                                           ', &
          '   valgrind  -- run tests with "valgrind"                                       ', &
          '   valgrind+ -- run tests with "valgrind --leak-check=full"                     ', &
          'NAMELIST VARIABLES: set current options                                         ', &
          '   save      -- write NAMELIST config file using current settings               ', &
          '   load      -- read NAMELIST config file to set options                        ', &
          '   show      -- show variables that may be set                                  ', &
          '   set       -- set NAMELIST values in submode                                  ', &
          'HOUSEKEEPING:                                                                   ', &
          '   list      -- list targets                                                    ', &
          '   bash|sh   -- bash shell                                                      ', &
          '   stop|q|quit|exit|end                                                         ', &
          'EXAMPLE:                                                                        ', &
          '    set                                                                         ', &
          '    compiler="ifx"                                                              ', &
          '    level=3                                                                     ', &
          '    .                                                                           ', &
          '    brief                                                                       ', &
          '    debug                                                                       ', &
          '    quit                                                                        ', &
          ' ' ]
      case ('show')
         write (*, g) 'options...'
         write (*, nml=args,delim='quote')
            !! something where you could restrict nml output to just listed names would be nice
            !!write(*,nml=args)['A','H']
            !!write(*,nml=*NML)args['A','H']
      case ('load')
         write (*, '(a)', advance='no') 'filename:'
         read (*, '(a)', iostat=iostat) answer
         if (iostat .ne. 0) exit
         open (file=answer, iostat=iostat, newunit=lun)
         if (iostat .ne. 0) exit
         read (lun, args, iostat=iostat)
         close (unit=lun, iostat=iostat)
      case ('save')
         write (*, g, advance='no') 'filename:'
         read (*, '(a)', iostat=iostat) answer
         if (iostat .ne. 0) exit
         open (file=answer, iostat=iostat, newunit=lun)
         if (iostat .ne. 0) exit
         write (lun, args, iostat=iostat)
         close (unit=lun, iostat=iostat)
      case ('set')
         UPDATE: block
            character(len=:), allocatable :: intmp
            do
               write(*,g)'enter "." to exit set mode. Current values are ...'
               write (*, nml=args, iostat=iostat, iomsg=iomsg)
               read(*,'(a)',iostat=iostat)line
               if(line.eq.'.')exit
               intmp = '&ARGS '//trim(line)//'/'
               read (intmp, nml=args, iostat=iostat, iomsg=iomsg)
               if (iostat .ne. 0) then
                  write (*, *) 'ERROR:', trim(iomsg)
               endif
            enddo
            strlevel=str('level=',level,sep='')
         end block UPDATE
         ! add //'' to avoid gfortran-11 bug
         options=str( &
         & str(' --compiler "',     compiler,      '"' ,if=compiler.ne.'',sep='')//'',     &
         & str(' --profile "',      profile,       '"' ,if=profile.ne.'',sep='')//'',      &
         & str(' --link-flag "',    link_flag,     '"' ,if=link_flag.ne.'',sep='')//'',    &
         & str(' --flag flag "',    flag,          '"' ,if=flag.ne.'',sep='')//'',         &
         & str(' --cxx-flag "',     cxx_flag,      '"' ,if=cxx_flag.ne.'',sep='')//'',     &
         & str(' --cxx-compiler "', cxx_compiler,  '"' ,if=cxx_compiler.ne.'',sep='')//'', &
         & str(' --c-flag "',       c_flag,        '"' ,if=c_flag.ne.'',sep='')//'',       &
         & str(' --c-compiler "',   c_compiler,    '"' ,if=c_compiler.ne.'',sep='')//'',   &
         & str(' --archiver "',     archiver,      '"' ,if=archiver.ne.'',sep='')//'')        
      case default
         call run(line)
      end select
   enddo INFINITE
contains

subroutine run(command) ! convenience routine so command does not cause program to stop
character(len=*)             :: command
logical, parameter            :: wait = .true.
integer                      :: exitstat
integer                      :: cmdstat
character(len=256)           :: cmdmsg
      call wrt([error_unit], "+ ", command)
      cmdmsg = ' '
      call execute_command_line(command, wait, exitstat, cmdstat, cmdmsg)
      if (cmdstat /= 0) then
         call wrt([error_unit], "cmdstat: ", cmdmsg, 'for command :', command)
      elseif (cmdmsg /= '') then
         call wrt([error_unit], "exitstat: ", cmdmsg, 'for command :', command)
      endif
end subroutine run

end program testtype
