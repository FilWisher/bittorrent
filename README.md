# bittorrent

A bittorrent client written in Haskell. It doesn't actually work yet but (I
think) it's close. It is in need of some love.

What needs fixing:
 - Unnecessarily single threaded: progress gets blocked waiting to e.g. connect
   to clients.
 - Requests for pieces aren't tracked. That means once we have n open requests
   for pieces that aren't fulfilled, we stop making progress. These requests
   need to be timed out.
