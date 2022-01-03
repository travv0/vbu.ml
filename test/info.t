list info for all groups
  $ ../vbu.exe --config config.json info
  Name: another
  Path: /another/path
  Glob: save*
  
  Name: test
  Path: /test/game/path
  

list info for selected groups
  $ ../vbu.exe --config config.json info another
  Name: another
  Path: /another/path
  Glob: save*
  

  $ ../vbu.exe --config config.json info another new
  Warning: No group named `new'
  
  Name: another
  Path: /another/path
  Glob: save*
  
