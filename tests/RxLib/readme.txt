  JVCL Conversion of RX Library 2.75 official conversion to Delphi 6, v1.0 (by SGB Software)
  =========================================================

TABLE OF CONTENTS
-----------------
  Overview
  Compatibility with your older code
  History
  Installation
  Help files
  Copyright Notes


Overview
--------
This is an official conversion of the Rx Library of components for Borland Delphi 6.  You are hearby given permittion to distribute the contents of this zip file as long as all copyright notices are kept in there current state.

Rx Library is a set of components and classes for Borland Delphi and C Builder.  Originally developed by Fedor Koshevnikov, Igor Pavluk and Serge Korolev, it has become a widely used and respected addition to the Borland VCL.


Compatibility with your older code
-----------------------------------
  - StrUtils has been renamed to rxStrUtils to avoid a naming conflict with borlands new StrUtils module.
  - RxGrids.TInplaceEditStyle type has been converted to the Delphi 6 TEditStyle type.


History
-------
20/02/2002
  First Official release of RX Library 2.75 For delphi 6.


Installation
------------

IF YOU HAVE DELPHI 6 PROFESSIONAL OR PERSON EDITITION YOU MUST DEACTIVATE THE CONDITIONAL DEFINE {$DEFINE DCS} IN THE RX.INC FILE BEFORE ATTEMPTING TO COMPILE THIS LIBRARY.  FAILURE TO DO THIS COULD CAUSE THE LIBRARY TO INSTALL INCORRECTLY.

On the File Menu select the Open command (Ctrl+ O) and browse for the Delphi Package RXCTL6.DPK that you extracted from this archive. In the Package window click on the compile button to
compile the package, RXCTL6.DPK. 

After compiling repeat these steps for the remaining RX Library run-time packages RXDB6.DPK, RXBDE6.DPK. 

Put the compiled BPL files into a directory that is accessible through the search PATH. After compiling RX run-time packages you must install RX design-time packages into the IDE.

On the File Menu select the Open command (Ctrl+ O) and browse for the Delphi Package DCLRX6.DPK that you extracted from this archive. In the Package window click on the compile button to
compile the package and then click on the Install buttonn to register the package in the IDE.

Repeat the above step for the remaining RX Library Design Time packages, DCLRXDB6.DPK and DCLRXBD6.DPK.

NOTE: do not save the package sources in the Delphi IDE.

Help files
----------

Help files for the Rx Library are currently provided in 2 languages , English and Russian.

English help file is copyright of Check frasersoft (http://www.frasersoft.clara.net/rxhelp) 2001

Copyright Notes
---------------
  RX Library is a copyright of SGB Software,Fedor Kozhevnikov, Igor Pavluk and Serge Korolev.  All rights reserved.  For Detailed licencing and distribution instructions please navigate to www.sgbsoftware.com or consult your help file.

---END OF FILE---




last updated - 2002-07-04