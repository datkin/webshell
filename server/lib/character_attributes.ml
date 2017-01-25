(* See https://en.wikipedia.org/wiki/ANSI_escape_code#Colors for more details on
 * colors *)
(*
0 -> Normal (default)
1 -> Bold
4 -> Underlined
5 -> Blink (appears as Bold)
7 -> Inverse
8 -> Invisible, i.e., hidden (VT300)
22 -> Normal (neither bold nor faint)
24 -> Not underlined
25 -> Steady (not blinking)
27 -> Positive (not inverse)
28 -> Visible, i.e., not hidden (VT300)
30 -> Set foreground color to Black
31 -> Set foreground color to Red
32 -> Set foreground color to Green
33 -> Set foreground color to Yellow
34 -> Set foreground color to Blue
35 -> Set foreground color to Magenta
36 -> Set foreground color to Cyan
37 -> Set foreground color to White
39 -> Set foreground color to default (original)
40 -> Set background color to Black
41 -> Set background color to Red
42 -> Set background color to Green
43 -> Set background color to Yellow
44 -> Set background color to Blue
45 -> Set background color to Magenta
46 -> Set background color to Cyan
47 -> Set background color to White
49 -> Set background color to default (original). If 16-color support is compiled, the following apply. Assume that xterm’s resources are set so that the ISO color codes are the first 8 of a set of 16. Then the aixterm colors are the bright versions of the ISO colors:
90 -> Set foreground color to Black
91 -> Set foreground color to Red
92 -> Set foreground color to Green
93 -> Set foreground color to Yellow
94 -> Set foreground color to Blue
95 -> Set foreground color to Magenta
96 -> Set foreground color to Cyan
97 -> Set foreground color to White
100 -> Set background color to Black
101 -> Set background color to Red
102 -> Set background color to Green
103 -> Set background color to Yellow
104 -> Set background color to Blue
105 -> Set background color to Magenta
106 -> Set background color to Cyan
107 -> Set background color to White If xterm is compiled with the 16-color support disabled, it supports the following, from rxvt:
100 -> Set foreground and background color to default If 88- or 256-color support is compiled, the following apply.
38 ; 5 ; P s -> Set foreground color to the second P s
48 ; 5 ; P s -> Set background color to the second P s
*)
