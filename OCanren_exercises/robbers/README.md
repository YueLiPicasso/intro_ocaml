# The Robbers' Problem

## Backgroud 

Three men robbed a gentleman of a vase, containing 24 ounces of balsam.
Whilst running away they met a glass-seller, of whom they purchased three
vessels. On reaching a place of safety they wished to divide the booty, but
found that their vessels contained 5, 11 and 13 ounces respectively. How
could they divide the balsam into equal portions ?


One of the several solutions is as follows:

The vessels can contain        | 24 oz. | 13 oz. |  11 oz. | 5 oz.
---                            | ---    | ---    | ---     | ---
Their constents originally are | 24     | 0      |  0      | 0
First, make their contents     | 0      | 8      | 11      | 5
Second, make their contents    | 16     | 8      |  0      | 0
Third, make their contents     | 16     | 0      | 8       | 0
Fourth, make their contents    | 3      | 13     | 8       | 0
Fifth, make their contents     | 3      | 8      | 8       | 5
Lastly, make their contents    | 8      | 8      | 8       | 0

(The above background description was quoted verbatim from the reference given below. )

## OCanren's Answer

Using OCanren we can find more than 60 solutions for the robbers ! A few are shown below. A solution is given as an OCaml list of quadruples, and distinct solutions are separated by empty lines. 

```ocaml
[(24, 0, 0, 0); (11, 13, 0, 0); (0, 13, 11, 0);(0, 8, 11, 5); (11, 8, 0, 5);
 (16, 8, 0, 0); (16, 0, 8, 0); (3, 13, 8, 0); (3, 8, 8, 5); (8, 8, 8, 0)]


[(24, 0, 0, 0); (19, 0, 0, 5); (8, 0, 11, 5); (13, 0, 11, 0); (0, 13, 11, 0);
 (0, 8, 11, 5); (11, 8, 0, 5); (16, 8, 0, 0); (16, 0, 8, 0); (3, 13, 8, 0);
 (3, 8, 8, 5); (8, 8, 8, 0)]

[(24, 0, 0, 0); (11, 13, 0, 0); (6, 13, 0, 5); (0, 13, 6, 5); (0, 8, 11, 5);
 (11, 8, 0, 5); (16, 8, 0, 0); (16, 0, 8, 0); (3, 13, 8, 0); (3, 8, 8, 5);
 (8, 8, 8, 0)]

[(24, 0, 0, 0); (19, 0, 0, 5); (19, 5, 0, 0); (11, 13, 0, 0); (6, 13, 0, 5);
 (0, 13, 6, 5); (0, 8, 11, 5); (11, 8, 0, 5); (16, 8, 0, 0); (16, 0, 8, 0);
 (3, 13, 8, 0); (3, 8, 8, 5); (8, 8, 8, 0)]

[(24, 0, 0, 0); (13, 0, 11, 0); (13, 0, 6, 5); (0, 13, 6, 5); (0, 8, 11, 5);
 (11, 8, 0, 5); (16, 8, 0, 0); (16, 0, 8, 0); (3, 13, 8, 0); (3, 8, 8, 5);
 (8, 8, 8, 0)]

[(24, 0, 0, 0); (11, 13, 0, 0); (6, 13, 0, 5); (19, 0, 0, 5); (8, 0, 11, 5);
 (13, 0, 11, 0); (0, 13, 11, 0); (0, 8, 11, 5); (11, 8, 0, 5); (16, 8, 0, 0);
 (16, 0, 8, 0); (3, 13, 8, 0);(3, 8, 8, 5); (8, 8, 8, 0)]

[(24, 0, 0, 0); (11, 13, 0, 0); (11, 2, 11, 0); (6, 2, 11, 5); (6, 13, 0, 5);
 (0, 13, 6, 5); (0, 8, 11, 5); (11, 8, 0, 5); (16, 8, 0, 0); (16, 0, 8, 0);
 (3, 13, 8, 0); (3, 8, 8, 5); (8, 8, 8, 0)]
```


## Reference



W. W. Rouse Ball and H. S. M. Coxeter 1947 
 _Mathematical Recreations and Essays (12 ed.)_
 The MacMillan Company, New York  p.28 and p.40