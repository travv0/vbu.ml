  $ chmod +w config.json

don't remove groups by answering no
  $ echo n | ../vbu.exe --config config.json remove test
  
  Are you sure you want to remove test? (y/N) 

  $ ../vbu.exe --config config.json list
  another
  test

remove groups
  $ echo y | ../vbu.exe --config config.json remove test
  
  Are you sure you want to remove test? (y/N) Removed test

  $ ../vbu.exe --config config.json list
  another

  $ ../vbu.exe --config config.json remove another -y
  Removed another

  $ ../vbu.exe --config config.json list
