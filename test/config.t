edit config
  $ ../sbu.exe config --config new_config.json
  Creating new config file at `new_config.json'.
  Use the `config' command to update default values, which are:
  
  Backup path: ./.sbu-backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  
  Backup path: ./.sbu-backups
  Backup frequency (in minutes): 15
  Number of backups to keep: 20
  

  $ ../sbu.exe config --config new_config.json -p /edited -f 5 -k 6
  Backup path: ./.sbu-backups -> /edited
  Backup frequency (in minutes): 15 -> 5
  Number of backups to keep: 20 -> 6
  

  $ ../sbu.exe config --config new_config.json -f 15
  Backup path: /edited
  Backup frequency (in minutes): 5 -> 15
  Number of backups to keep: 6
  

  $ ../sbu.exe config --config new_config.json
  Backup path: /edited
  Backup frequency (in minutes): 15
  Number of backups to keep: 6
  
