{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLConst.PAS, released on 2002-07-04.

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

unit JvLConst;

{ RX Library constants }
{
  Reserved diapasone
  from MaxExtStrID + 10
  to   MaxExtStrID - 20
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

const

{ Component pages }

  srRXControls         = MaxExtStrID;
  srRXDBAware          = MaxExtStrID - 1;
  srRXTools            = MaxExtStrID - 2;

{ TImageList component editor }

  srSaveImageList      = MaxExtStrID - 3;

{ TJvFormStorage component editor }

  srStorageDesigner    = MaxExtStrID - 4;

{ TJvPageManager component editor }

  srProxyEditor        = MaxExtStrID - 5;
  srPageProxies        = MaxExtStrID - 6;
  srProxyName          = MaxExtStrID - 7;
  srPageName           = MaxExtStrID - 8;

{ TJvSpeedBar component editor }

  srSBItemNotCreate    = MaxExtStrID - 9;
  srConfirmSBDelete    = MaxExtStrID - 10;
  srSpeedbarDesigner   = MaxExtStrID - 11;
  srNewSectionName     = MaxExtStrID - 12;

{ TJvTimerList component editor }

  srEventNotCreate     = MaxExtStrID - 13;
  srTimerDesigner      = MaxExtStrID - 14;
  srTimerEvents        = MaxExtStrID - 15;

{ TJvAnimatedImage component editor }

  srAniCurFilter       = MaxExtStrID - 16;
  srEditPicture        = MaxExtStrID - 17;
  srLoadAniCursor      = MaxExtStrID - 18;

{ TJvIconList property editor }

  srLoadIcon           = MaxExtStrID - 19;

{ TJvxGradientCaption component editor }

  srCaptionDesigner    = MaxExtStrID - 20;
  srGradientCaptions   = MaxExtStrID + 10;

{ TJvMemoryTable & TJvMemoryData component editor }

  srBorrowStructure    = MaxExtStrID + 9;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.
