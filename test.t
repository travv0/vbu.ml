  $ cat >test <<EOF
  > target area: x=20..30, y=-10..-5
  > EOF

  $ ./sbu.exe test
  The highest y position the probe can reach is 45
  There are 112 distinct initial velocity values that cause the probe to fall within the target area
