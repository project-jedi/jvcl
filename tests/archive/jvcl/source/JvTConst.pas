{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTConst.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}


unit JvTConst;

{ RX tools components constants }
{
  Reserved diapasone
  from MaxExtStrID - 136
  to   MaxExtStrID - 184
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

const

{ JvDualList }

  SDualListSrcCaption         = MaxExtStrID - 136;
  SDualListDestCaption        = MaxExtStrID - 137;

{ JvClipView }

  SClipbrdUnknown             = MaxExtStrID - 138;
  SClipbrdEmpty               = MaxExtStrID - 139;

{ JvSpeedbar }

  SCustomizeSpeedbar          = MaxExtStrID - 140;
  SAvailButtons               = MaxExtStrID - 141;
  SSpeedbarCategories         = MaxExtStrID - 142;
  SSpeedbarEditHint           = MaxExtStrID - 143;

{ MathParser }

  SParseSyntaxError           = MaxExtStrID - 145;
  SParseNotCramp              = MaxExtStrID - 146;
  SParseDivideByZero          = MaxExtStrID - 147;
  SParseSqrError              = MaxExtStrID - 148;
  SParseLogError              = MaxExtStrID - 149;
  SParseInvalidFloatOperation = MaxExtStrID - 150;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.
