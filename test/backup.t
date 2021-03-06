  $ chmod +w config.json

backup files
  $ ../vbu.exe add --config config.json files -p files -g "*.{txt,sh}"
  Group added successfully:
  
  Name: files
  Path: $TESTCASE_ROOT/files
  Glob: *.{txt,sh}
  

  $ ../vbu.exe backup --config config.json -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' | sort
  
  
  	./.vbu-backups/files/a.txt
  	./.vbu-backups/files/files/b.txt
  	./.vbu-backups/files/files/c.txt
  	./.vbu-backups/files/files/files/[0-9].txt
  	./.vbu-backups/files/files/files/d.sh
  $TESTCASE_ROOT/files/a.txt ==>
  $TESTCASE_ROOT/files/files/b.txt ==>
  $TESTCASE_ROOT/files/files/c.txt ==>
  $TESTCASE_ROOT/files/files/files/[0-9].txt ==>
  $TESTCASE_ROOT/files/files/files/d.sh ==>
  Finished backing up 5 files for files in $SECONDSs on $DATE_AND_TIME
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path

  $ ../vbu.exe backup --config config.json -v
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path

check backup directory contents
  $ ls .vbu-backups/files
  a.txt
  files

  $ ls .vbu-backups/files/files
  b.txt
  c.txt
  files

  $ ls .vbu-backups/files/files/files
  [0-9].txt
  d.sh

check versioning
  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt

  $ ../vbu.exe backup --config config.json -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' | sort
  
  
  	./.vbu-backups/files/files/b.txt
  	./.vbu-backups/files/files/files/[0-9].txt
  $TESTCASE_ROOT/files/files/b.txt ==>
  $TESTCASE_ROOT/files/files/files/[0-9].txt ==>
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path

  $ ls .vbu-backups/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  b.txt
  b.txt.bak.$TIMESTAMP
  c.txt
  files

check cleanup
  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt

  $ ../vbu.exe backup --config config.json -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' | sort
  
  
  	./.vbu-backups/files/files/b.txt
  	./.vbu-backups/files/files/files/[0-9].txt
  $TESTCASE_ROOT/files/files/b.txt ==>
  $TESTCASE_ROOT/files/files/files/[0-9].txt ==>
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path

  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt

  $ ../vbu.exe backup --config config.json -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/' | sort
  
  
  	./.vbu-backups/files/files/b.txt
  	./.vbu-backups/files/files/files/[0-9].txt
  $TESTCASE_ROOT/files/files/b.txt ==>
  $TESTCASE_ROOT/files/files/files/[0-9].txt ==>
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  Note: Deleting ./.vbu-backups/files/files/b.txt.bak.$TIMESTAMP
  Note: Deleting ./.vbu-backups/files/files/files/[0-9].txt.bak.$TIMESTAMP
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path

  $ sleep 1 && touch files/a.txt && touch files/files/b.txt && touch files/files/c.txt && touch files/files/files/\[0-9\].txt

  $ ../vbu.exe backup --config config.json -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/' | sort
  
  
  	./.vbu-backups/files/a.txt
  	./.vbu-backups/files/files/b.txt
  	./.vbu-backups/files/files/c.txt
  	./.vbu-backups/files/files/files/[0-9].txt
  $TESTCASE_ROOT/files/a.txt ==>
  $TESTCASE_ROOT/files/files/b.txt ==>
  $TESTCASE_ROOT/files/files/c.txt ==>
  $TESTCASE_ROOT/files/files/files/[0-9].txt ==>
  Finished backing up 4 files for files in $SECONDSs on $DATE_AND_TIME
  Note: Deleting ./.vbu-backups/files/files/b.txt.bak.$TIMESTAMP
  Note: Deleting ./.vbu-backups/files/files/files/[0-9].txt.bak.$TIMESTAMP
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path

  $ ls .vbu-backups/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  b.txt
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  c.txt
  c.txt.bak.$TIMESTAMP
  files

  $ ls .vbu-backups/files/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  [0-9].txt
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP
  d.sh

keep all
  $ ../vbu.exe config --config config.json --keep 0
  Backup path: ./.vbu-backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 3 -> 0
  

  $ sleep 1 && touch files/files/b.txt && touch files/files/files/\[0-9\].txt

  $ ../vbu.exe backup --config config.json -v | sed -E -e 's/[0-9]+\.[0-9]+/$SECONDS/' -E -e 's/(Finished backing up [0-9]+ files? for files in \$SECONDSs on).*/\1 $DATE_AND_TIME/' -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/' | sort
  
  
  	./.vbu-backups/files/files/b.txt
  	./.vbu-backups/files/files/files/[0-9].txt
  $TESTCASE_ROOT/files/files/b.txt ==>
  $TESTCASE_ROOT/files/files/files/[0-9].txt ==>
  Finished backing up 2 files for files in $SECONDSs on $DATE_AND_TIME
  Warning: Path set for another doesn't exist: /another/path
  Warning: Path set for test doesn't exist: /test/game/path

  $ ls .vbu-backups/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  b.txt
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  b.txt.bak.$TIMESTAMP
  c.txt
  c.txt.bak.$TIMESTAMP
  files

  $ ls .vbu-backups/files/files/files | sed -E -e 's/bak\.[0-9_]+/bak.$TIMESTAMP/'
  [0-9].txt
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP
  [0-9].txt.bak.$TIMESTAMP
  d.sh
