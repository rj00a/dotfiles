#!/usr/bin/env python

# trackord.py
# Given a list of music files, print them back to stdout in album-order.

from subprocess import run
from re import search, IGNORECASE
from sys import argv

# A dict of "Album -> (Track -> TrackNum)"
albums = {}

# If a track is not part of an album, add it to this list.
orphans = []

for filename in argv:
    res = run(['ffprobe', filename], capture_output = True)
    if res.returncode != 0:
        continue

    info = res.stderr.decode('utf-8')
    album_name_match = search(r'album\s*: (.+)', info, IGNORECASE)
    track_num_match = search(r'track\s*: (\d+)', info, IGNORECASE)

    if not album_name_match or not track_num_match:
        orphans.append(filename)
        continue

    album_name = album_name_match[1]
    track_num = int(track_num_match[1])

    if album_name not in albums:
        albums[album_name] = {track_num: filename}
    else:
        albums[album_name][track_num] = filename

for alb in albums.values():
    for _, filename in sorted(alb.items()):
        print(filename)

for filename in orphans:
    print(filename)
