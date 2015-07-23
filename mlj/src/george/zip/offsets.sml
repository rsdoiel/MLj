
(* Central Header *)
0	central file header signature   4 bytes  (0x02014b50)
4	version made by                 2 bytes
6	version needed to extract       2 bytes
8	general purpose bit flag        2 bytes
10	compression method              2 bytes
12	last mod file time              2 bytes
14	last mod file date              2 bytes
16	crc-32                          4 bytes
20	compressed size                 4 bytes
24	uncompressed size               4 bytes
28	filename length                 2 bytes
30	extra field length              2 bytes
32	file comment length             2 bytes
34	disk number start               2 bytes
36	internal file attributes        2 bytes
38	external file attributes        4 bytes
42	relative offset of local header 4 bytes

46	filename (variable size)
46+f	extra field (variable size)
46+f+e	file comment (variable size)
46+f+e+c



(* Local Header *)

0	local file header signature     4 bytes  (0x04034b50)
4	version needed to extract       2 bytes
6	general purpose bit flag        2 bytes
8	compression method              2 bytes
10	last mod file time              2 bytes
12	last mod file date              2 bytes
14      crc-32                          4 bytes
18	compressed size                 4 bytes
22	uncompressed size               4 bytes
26	filename length                 2 bytes
28	extra field length              2 bytes

30	filename (variable size)
30+f	extra field (variable size)
30+f+e


