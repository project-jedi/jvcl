{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvGConst.PAS, released on 2002-07-04.

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


unit JvGConst;

{ RX graphic support constants }
{
  Reserved diapasone
  from MaxExtStrID - 200
  to   MaxExtStrID - 230
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

{ JvGIF }

const
  SGIFImage            = MaxExtStrID - 200;
  SChangeGIFSize       = MaxExtStrID - 201;
  SNoGIFData           = MaxExtStrID - 202;
  SUnrecognizedGIFExt  = MaxExtStrID - 203;
  SWrongGIFColors      = MaxExtStrID - 204;
  SBadGIFCodeSize      = MaxExtStrID - 205;
  SGIFDecodeError      = MaxExtStrID - 206;
  SGIFEncodeError      = MaxExtStrID - 207;
  SGIFVersion          = MaxExtStrID - 208;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.
