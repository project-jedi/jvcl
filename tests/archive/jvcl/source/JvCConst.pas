{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCConst.PAS, released on 2002-07-04.

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

unit JvCConst;

{ Jv controls and components constants }
{
  Reserved diapasone
  from MaxExtStrID - 36
  to   MaxExtStrID - 84
}

interface

const
{ The minimal VCL's used string ID is 61440. The custom IDs must be
  less that above. }
  MaxExtStrID = 61300;

const

{ JvToolEdit }

  SBrowse                = MaxExtStrID - 36;
  SDefaultFilter         = MaxExtStrID - 37;

{ JvPickDate }

  SDateDlgTitle          = MaxExtStrID - 38;
  SNextYear              = MaxExtStrID - 39;
  SNextMonth             = MaxExtStrID - 40;
  SPrevYear              = MaxExtStrID - 41;
  SPrevMonth             = MaxExtStrID - 42;

{ JvVCLUtils }

  SNotImplemented        = MaxExtStrID - 43;
  SFileNotExec           = MaxExtStrID - 44;
  SLoadLibError          = MaxExtStrID - 45;
  SDetails               = MaxExtStrID - 46;

implementation

{$IFDEF WIN32}
 {$R *.Res}
{$ELSE}
 {$R *.R16}
{$ENDIF}

end.
