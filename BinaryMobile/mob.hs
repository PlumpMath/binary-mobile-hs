type Arm = Int
type Wt = Int

data Mobile =
     Simpl Arm Wt |
     Comp Arm BinMobile

data BinMobile = BinMobile Mobile Mobile

isBalanced (BinMobile m1 m2) =
           moment m1 == moment m2 && 
           balanced m1 && balanced m2

moment (Simpl arm wt) = arm * wt
moment (Comp arm binMob) = arm * (weight binMob)

weight (BinMobile m1 m2) = bweight m1 + bweight m2

bweight (Simpl arm wt) = wt
bweight (Comp arm binMob) = weight binMob

balanced (Simpl _ _) = True
balanced (Comp _ binMob) = isBalanced binMob

{-
x = isBalanced bm 
    where
       m1 = Simpl 3 5
       m2 = Simpl 5 3
       bm = BinMobile m1 m2
-}
x = isBalanced bm
    where
      m1 = Simpl 3 5
      m2 = Simpl 5 3
      bm1 = BinMobile m1 m2

      m3 = Simpl 8 3
      m4 = Simpl 6 4
      bm2 = BinMobile m3 m4

      m5 = Comp 7 bm1
      m6 = Comp 8 bm2
      bm = BinMobile m5 m6
