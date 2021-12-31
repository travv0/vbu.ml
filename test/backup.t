  $ chmod +w config.json

backup files
  $ ../sbu.exe add --config config.json files -p files -g "{,*/}*.txt"
  Game added successfully:
  
  Name: files
  Save path: $TESTCASE_ROOT/files
  Save glob: {,*/}*.txt
  

  $ ../sbu.exe backup --config config.json | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up 3 files for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' | sort
  
  
       ./.sbu-backups/files/a.txt
       ./.sbu-backups/files/files/b.txt
       ./.sbu-backups/files/files/c.txt
  $TESTCASE_ROOT/files/a.txt ==>
  $TESTCASE_ROOT/files/files/b.txt ==>
  $TESTCASE_ROOT/files/files/c.txt ==>
  Finished backing up 3 files for files in $SECONDSs on $DATE_AND_TIME
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path

  $ ../sbu.exe backup --config config.json
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path
