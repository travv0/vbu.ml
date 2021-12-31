list info for all games
  $ ../sbu.exe info --config config.json
  Name: another
  Save path: /another/path
  Save glob: save*
  
  Name: test
  Save path: /test/game/path
  

list info for selected games
  $ ../sbu.exe info --config config.json another
  Name: another
  Save path: /another/path
  Save glob: save*
  

  $ ../sbu.exe info --config config.json another new
  Warning: No game named `new'
  
  Name: another
  Save path: /another/path
  Save glob: save*
  
