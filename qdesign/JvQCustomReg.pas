{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCustomReg.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is John Doe.
Portions created by John Doe are Copyright (C) 2003 John Doe.
All Rights Reserved.

Contributor(s):

Last Modified: 2003-11-09

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQCustomReg;

interface

procedure Register;

implementation

uses
  Classes, QImgList,

  DesignEditors, DesignIntf,

  //FiltEdit,

  //ToolsAPI,
  JclSchedule,
  JvQDsgnConsts,
  JvQGammaPanel, JvQInspector, JvQLinkLabel,
  JvQTMTimeLine,
  JvQValidateEdit,
  JvQChart;

{$IFDEF MSWINDOWS}
{$R ..\resources\JvCustomReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF LINUX}
{$R ../Resources/JvCustomReg.dcr}
{$ENDIF LINUX}

procedure Register;
const
  cActivePageIndex = 'ActivePageIndex';
  cImageIndex = 'ImageIndex';
  cColors = 'Colors';
  cSchedule = 'Schedule';
  cFilter = 'Filter';
begin
//  RegisterComponents(RsPaletteButton, [TJvLookOutButton, TJvExpressButton]);
  RegisterComponents(RsPaletteEdit, [TJvValidateEdit]);
  RegisterComponents(RsPaletteBarPanel, [TJvGammaPanel]);
  RegisterComponents(RsPaletteLabel, [TJvLinkLabel]);
//  RegisterComponents(RsPaletteImageAnimator, [TJvThumbView, TJvThumbnail,
//    TJvThumbImage]);
  RegisterComponents(RsPaletteVisual, [TJvInspector, TJvInspectorBorlandPainter,
    TJvInspectorDotNETPainter, TJvTMTimeLine, TJvChart]);
//  RegisterComponents(RsPaletteNonVisual, [TJvTrayIcon, TJvScheduledEvents,
//    TJvBalloonHint]);
end;

end.
