{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvDConst;

{ RX Data aware controls constants }
{
  Reserved range
  from MaxExtStrID - 86
  to   MaxExtStrID - 134
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

const

{ JvDBLists }

  SLocalDatabase          = MaxExtStrID - 86;

{ JvDBUtils }

  SRetryLogin             = MaxExtStrID - 87;

{ JvDBFilter }

  SExprNotBoolean         = MaxExtStrID - 88;
  SExprBadNullTest        = MaxExtStrID - 89;
  SExprBadField           = MaxExtStrID - 90;
  SCaptureFilter          = MaxExtStrID - 91;
  SNotCaptureFilter       = MaxExtStrID - 92;

{ RxDBCtrl }

  SInactiveData           = MaxExtStrID - 93;
  SBrowseData             = MaxExtStrID - 94;
  SEditData               = MaxExtStrID - 95;
  SInsertData             = MaxExtStrID - 96;
  SSetKeyData             = MaxExtStrID - 97;
  SCalcFieldsData         = MaxExtStrID - 98;

{ LoginDlg }

  SRegistration           = MaxExtStrID - 99;
  SAppTitleLabel          = MaxExtStrID - 100;
  SHintLabel              = MaxExtStrID - 101;
  SUserNameLabel          = MaxExtStrID - 102;
  SPasswordLabel          = MaxExtStrID - 103;
  SInvalidUserName        = MaxExtStrID - 104;

{ JvChPswDlg }

  SChangePassword         = MaxExtStrID - 105;
  SOldPasswordLabel       = MaxExtStrID - 106;
  SNewPasswordLabel       = MaxExtStrID - 107;
  SConfirmPasswordLabel   = MaxExtStrID - 108;
  SPasswordChanged        = MaxExtStrID - 109;
  SPasswordNotChanged     = MaxExtStrID - 110;
  SPasswordsMismatch      = MaxExtStrID - 111;

{ JvDbExcpt }

  SDBExceptCaption        = MaxExtStrID - 112;
  SBDEErrorLabel          = MaxExtStrID - 113;
  SServerErrorLabel       = MaxExtStrID - 114;
  SErrorMsgLabel          = MaxExtStrID - 115;
  SNextButton             = MaxExtStrID - 116;
  SPrevButton             = MaxExtStrID - 117;

{ JvDBFilter expression parser }

  SExprIncorrect          = MaxExtStrID - 118;
  SExprTermination        = MaxExtStrID - 119;
  SExprNameError          = MaxExtStrID - 120;
  SExprStringError        = MaxExtStrID - 121;
  SExprInvalidChar        = MaxExtStrID - 122;
  SExprNoRParen           = MaxExtStrID - 123;
  SExprExpected           = MaxExtStrID - 124;
  SExprBadCompare         = MaxExtStrID - 125;

{ JvDBUtils }

  SConfirmSave            = MaxExtStrID - 126;
  SDatabaseName           = MaxExtStrID - 127;  

{ LoginDlg }
  
  SUnlockCaption          = MaxExtStrID - 128;
  SUnlockHint             = MaxExtStrID - 129;

{ RxDBCtrl }

  SDeleteMultipleRecords  = MaxExtStrID - 130;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.
