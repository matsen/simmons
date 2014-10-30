# simmons

![](http://www.math.canterbury.ac.nz/matsen/simmons/framed.png)

### Purpose
simmons computes tree shape statistics of trees in Newick format.

### Usage
simmons is a command-line program. It can take tree files as arguments or accept input from stdin. The file format is FASTA-like and should be self-explanatory given the sample tree file below. The output is a tab-delimited table appropriate for column or any spreadsheet program. Here are some sample uses:

First the basic use. We "pipe" the output of simmons using the character | to column which aligns it nicely:

    [w:~/trees]$ ./simmons -c sample.tre | column -t
    #name   I_c        N_bar    I_2       B_1      B_2      Var_N     Q_1        cherries  A_1        A_2
    test0  0.194444   3.5      0.34375   5.58333  3.1875   0.65      0.0535714  4         0.143155   0.296448
    test1  0.321637   5.85     0.485209  10.5512  2.76953  3.9275    0.199617   7         1.49763    -0.223209
    test2  0.0713255  8.26     0.523566  49.5544  5.22461  4.9124    0.170084   30        -0.873295  0.35144
    test3  0.179803   5.96667  0.622722  13.9218  4.13867  3.89889   0.508773   8         -1.37958   0.55414
    test4  0.333333   3        0.386667  3.83333  2.625    0.571429  -0.133333  3         5.69376    -1.60146

Here is the same example showing how simmons accepts input from stdin:

    [w:~/trees]$ cat sample.tre | ./simmons -c | column -t
    #name   I_c        N_bar    I_2       B_1      B_2      Var_N     Q_1        cherries  A_1        A_2
    test0  0.194444   3.5      0.34375   5.58333  3.1875   0.65      0.0535714  4         0.143155   0.296448
    test1  0.321637   5.85     0.485209  10.5512  2.76953  3.9275    0.199617   7         1.49763    -0.223209
    test2  0.0713255  8.26     0.523566  49.5544  5.22461  4.9124    0.170084   30        -0.873295  0.35144
    test3  0.179803   5.96667  0.622722  13.9218  4.13867  3.89889   0.508773   8         -1.37958   0.55414
    test4  0.333333   3        0.386667  3.83333  2.625    0.571429  -0.133333  3         5.69376    -1.60146

Of course, the main reason one might want to use simmons is to evaluate some BRTSS statistics, which are described in this paper. This is done with -f like so:

    [s:~/trees]$ simmons -f brtss.st sample.tre | column -t
    #name  0;1|_>(x[2]-y[2])+_+x[1];_+x[2]  1|_+x[1]  8|p(l(_+x[1]),5)
    test0  7                                10        230366
    test1  55                               20        333170
    test2  346                              100       474066
    test3  73                               30        434065
    test4  5                                7         130505

### About tree shape statistics
Tree shape statistics are numerical summaries of some aspect of the shape of a phylogenetic tree. The definition of the tree shape statistics computed by simmons can be found here.

### Details
Because the vast majority of tree shape statistics in use only make sense for rooted binary trees, this is the only type of tree which simmons accepts. Also note that edglengths are irrelevant in all of the current tree shape statistics.

The tree parser inside simmons can parse all of the trees on Joe Felsenstein's Newick format definition page. It doesn't accept internal node labels, and there are other things which it may not accept. If this is the case for a tree you would like to parse, let me know and I will fix it.

simmons was written in ocaml using the GNU linear programming package GLPK. I will release it on other platforms as soon as I can get around to compiling on them. It is free software. If you would like the source please let me know. Also, it's not particularly optimized so if your application requires more speed I should be able to arrange that.

### About the name
simmons is named after the legendary surfboard shaper Bob Simmons pictured above. Simmons was the father of the modern surfboard: he was the first to make a board out of fiberglass and foam, the first to put fins on a board, and the first to design a surfboard with rocker. He was a professional applied mathematician who applied hydrodynamic principles to his surfboard construction. He was born in 1919 and died surfing in 1954.
