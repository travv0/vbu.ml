list info for all groups
  $ ../vbu.exe info --config config.json
  Name: another
  Path: /another/path
  Glob: save*
  
  Name: test
  Path: /test/game/path
  Glob: **
  

list info for selected groups
  $ ../vbu.exe info --config config.json another
  Name: another
  Path: /another/path
  Glob: save*
  

  $ ../vbu.exe info --config config.json another new
  Warning: No group named `new'
  
  Name: another
  Path: /another/path
  Glob: save*
  
