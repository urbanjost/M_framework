       program demo_journal
       !! showing creating debug messages
       use M_framework__journal, only : journal
       implicit none
       !! produces no output because trail is not on
       call journal('D','*demo* DEBUG MESSAGE 001 IGNORED')
       !! turn on debug messages
       call journal('>','debug on')
       !! produces output on stdout because debug mode
       !! is on but no named trail file
       call journal('D','*demo* DEBUG MESSAGE 002 ON STDOUT')
       !! open trail file
       call journal('O','mytrail.txt')
       !! debug messages now go to the trail file only
       call journal('D','*demo* DEBUG MESSAGE 003 TO TRAIL')
       !! or always to stdout and trail file only if on
       call journal('DS','*demo* DEBUG MESSAGE 003 TO TRAIL')
       !! close trail file so messages go only to stdout again
       call journal('O','')
       !! debug on stdout now
       call journal('D','*demo* DEBUG MESSAGE 004 TO STDOUT')
       !! turn off debug messages
       call journal('<','debug off')
       !! back to no output from the next message
       call journal('D','*demo* DEBUG MESSAGE 005 IGNORED')
       end program demo_journal
