***Create SR Modesty scale score

COMPUTE ih_27x=8-ih_27.
VARIABLE LABELS ih_27x 'R_I think that paying attention to others who disagree with me is a waste of time'.
EXECUTE.

COMPUTE ih_51x=8-ih_51.
VARIABLE LABELS ih_51x 'R_I dont take people seriously if theyre very different from me'.
EXECUTE.

COMPUTE ih_55x=8-ih_55.
VARIABLE LABELS ih_55x 'R_Only wimps admit that theyre confused'.
EXECUTE.

COMPUTE Modesty=Mean.3(ih_27x, ih_34, ih_35, ih_45, ih_55x, ih_51x).
EXECUTE.

*Modesty scale with original item

COMPUTE ih_50x=8-ih_50.
Variable Labels ih_50x 'R_Only wimps admit they make mistakes'.
EXECUTE.

COMPUTE Modesty_original=Mean.3(ih_27x, ih_34, ih_35, ih_45, ih_50x, ih_51x).
EXECUTE.

***Create INF Modesty scale score

COMPUTE iih_27x=8-iih_27.
VARIABLE LABELS iih_27x 'R_thinks that paying attention to people who disagree with him/her is a waste of time'.
EXECUTE.

COMPUTE iih_51x=8-iih_51.
VARIABLE LABELS iih_51x 'R_doesnt take people seriously if theye very different from him/her.'.
EXECUTE.

COMPUTE iih_55x=8-iih_55.
VARIABLE LABELS iih_55x 'R_thinks that only wimps admit that theyre confused.'.
EXECUTE.

COMPUTE iModesty=Mean.3(iih_27x, iih_34, iih_35, iih_45, iih_55x, iih_51x).
EXECUTE.

COMPUTE iih_50x=8-iih_50.
Variable Labels iih_50x 'R_thinks that only wimps admit that theyve made mistakes'.
EXECUTE.

COMPUTE iModesty_original=Mean.3(iih_27x, iih_34, iih_35, iih_45, iih_50x, iih_51x).
EXECUTE.

***Create Vanity SR scale score

COMPUTE ih_8x=8-ih_8.
VARIABLE LABELS ih_8x 'R_Being smarter than other people is not especially important to me'.
EXECUTE.

COMPUTE ih_13x=8-ih_13.
VARIABLE LABELS ih_13x 'R_I wouldnt want people to treat me as though I were intellectually superior to them'.
EXECUTE.

COMPUTE Vanity=Mean.4(ih_8x, ih_10, ih_11, ih_13x, ih_15, ih_32).
EXECUTE.

***Create Vanity INF scale score

COMPUTE iih_8x=8-iih_8.
VARIABLE LABELS iih_8x 'R_feels that being smarter than other people is not especially important'.
EXECUTE.

COMPUTE iih_13x=8-iih_13.
VARIABLE LABELS iih_13x 'R_wouldnt want people to treat him/her as though she was intellectually superior to them'.
EXECUTE.

COMPUTE iVanity=Mean.4(iih_8x, iih_10, iih_11, iih_13x, iih_15, iih_32).
EXECUTE.


***Create SR Intellectual Neuroticism scale score

COMPUTE ih_39x=8-ih_39.
VARIABLE LABELS ih_39x 'R_I appreciate being corrected when I make a mistake.'.
EXECUTE.

COMPUTE ih_40x=8-ih_40.
VARIABLE LABELS ih_40x 'R_When someone corrects a mistake that Ive made, I do not feel embarrassed.'.
EXECUTE.

COMPUTE Neurot=Mean.3(ih_37, ih_38, ih_39x, ih_40x, ih_43).
EXECUTE.

***Create INF neurot scale

COMPUTE iih_39x=8-iih_39.
VARIABLE LABELS iih_39x 'R_appreciates being corrected when she makes a mistake'.
EXECUTE.

COMPUTE iih_40x=8-iih_40.
VARIABLE LABELS iih_40x 'R_does not feel embarrassed when someone corrects a mistake that they have made'.
EXECUTE.

COMPUTE iNeurot=Mean.3(iih_37, iih_38, iih_39x, iih_40x, iih_43).
EXECUTE.


***Create SR Boredom scale score

COMPUTE ih_24x=8-ih_24.
VARIABLE LABELS ih_24x 'R_I enjoy reading about the ideas of different cultures'.
EXECUTE.

COMPUTE Boredom=Mean.4(ih_18, ih_24x, ih_25, ih_26, ih_29, ih_31).
EXECUTE.

***Create INF Boredom scale score

COMPUTE iih_24x=8-iih_24.
VARIABLE LABELS iih_24x 'R_enjoys reading about the ideas of different cultures'.
EXECUTE.

COMPUTE iBoredom=Mean.4(iih_18, iih_24x, iih_25, iih_26, iih_29, iih_31).
EXECUTE.

***Create SR Grit scale score

COMPUTE ih_21x=8-ih_21.
VARIABLE LABELS ih_21x 'R_Often when Im in the process of learning something, I end up quitting without having really mastered it'.
EXECUTE.

COMPUTE ih_22x=8-ih_22.
VARIABLE LABELS ih_22x 'R_I learn only the minimum amount needed to get by'.
EXECUTE.

COMPUTE Grit=Mean.2(ih_19, ih_20, ih_21x, ih_22x).
EXECUTE.

***Create INF Grit scale score

COMPUTE iih_21x=8-iih_21.
VARIABLE LABELS iih_21x 'R_when in the process of learning something, ends up quitting without having really mastered it'.
EXECUTE.

COMPUTE iih_22x=8-iih_22.
VARIABLE LABELS iih_22x 'R_learns only the minimum amount needed to get by'.
EXECUTE.

COMPUTE iGrit=Mean.2(iih_19, iih_20, iih_21x, iih_22x).
EXECUTE.

DATASET ACTIVATE DataSet1.
CORRELATIONS
  /VARIABLES=Modesty ih_27x ih_34 ih_35 ih_45 ih_55x ih_51x 
Vanity ih_8x ih_10 ih_11 ih_13x ih_15 ih_32
Boredom ih_18 ih_24x ih_25 ih_26 ih_29 ih_31
Grit ih_19 ih_20 ih_21x ih_22x
Neurot ih_37 ih_38 ih_39x ih_40x ih_43
WITH iModesty iih_27x iih_34 iih_35 iih_45 iih_55x iih_51x 
iVanity iih_8x iih_10 iih_11 iih_13x iih_15 iih_32
iBoredom iih_18 iih_24x iih_25 iih_26 iih_29 iih_31
iGrit iih_19 iih_20 iih_21x iih_22x
iNeurot iih_37 iih_38 iih_39x iih_40x iih_43
  /PRINT=TWOTAIL NOSIG
  /STATISTICS DESCRIPTIVES
  /MISSING=PAIRWISE.
