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

  ToolsAPI,
  JclSchedule,
  JvQDsgnConsts,
  JvQGammaPanel, JvQInspector, JvQLinkLabel,
  JvQTMTimeLine, JvQLookOut, JvQOutlookBar,
  JvQValidateEdit,
  JvQOutlookBarEditors, JvQLookoutEditor, JvQChart;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvCustomReg.dcr}
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
  RegisterComponents(RsPaletteButton, [TJvLookOutButton, TJvExpressButton]);
  RegisterComponents(RsPaletteEdit, [TJvValidateEdit]);
  RegisterComponents(RsPaletteBarPanel, [TJvGammaPanel, TJvOutlookBar,
    TJvLookout, {TJvLookOutPage, } TJvExpress]);
  RegisterComponents(RsPaletteLabel, [TJvLinkLabel]);
//  RegisterComponents(RsPaletteImageAnimator, [TJvThumbView, TJvThumbnail,
//    TJvThumbImage]);
  RegisterComponents(RsPaletteVisual, [TJvInspector, TJvInspectorBorlandPainter,
    TJvInspectorDotNETPainter, TJvTMTimeLine, TJvChart]);
//  RegisterComponents(RsPaletteNonVisual, [TJvTrayIcon, TJvScheduledEvents,
//    TJvBalloonHint]);

  RegisterPropertyEditor(TypeInfo(Integer), TJvCustomOutlookBar,
    cActivePageIndex, TJvOutlookBarActivePageEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages), TJvCustomOutlookBar,
    '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarButtons), TJvOutlookBarPage,
    '', TJvOutlookBarPagesPropertyEditor);
//  RegisterPropertyEditor(TypeInfo(Integer), TJvOutlookBarButton,
//    cImageIndex, TJvOutlookBarButtonImageIndexProperty);
//  RegisterPropertyEditor(TypeInfo(TJvColors), TJvHLEditor,
//    cColors, TJvHLEditorColorProperty);
//  RegisterPropertyEditor(TypeInfo(TJvColors), TJvWideHLEditor,
//    cColors, TJvHLEditorColorProperty);
//  RegisterPropertyEditor(TypeInfo(IJclSchedule), TJvEventCollectionItem,
//    cSchedule, TJvSchedulePropertyEditor);
//  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvLookoutButton,
//    cImageIndex, TJvLookOutImageIndexProperty);
//  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvExpressButton,
//    cImageIndex, TJvLookOutImageIndexProperty);
//  RegisterPropertyEditor(TypeInfo(string), TJvThumbView,
//    cFilter, TFilterProperty);

//  RegisterComponentEditor(TJvHLEdPropDlg, TJvHLEdPropDlgEditor);
  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarComponentEditor);
//  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);
  RegisterComponentEditor(TJvLookOut, TJvLookOutEditor);
  RegisterComponentEditor(TJvLookOutPage, TJvLookOutPageEditor);
  RegisterComponentEditor(TJvExpress, TJvExpressEditor);
//  RegisterComponentEditor(TJvCustomScheduledEvents, TJvSchedEventComponentEditor);
  RegisterClass(TJvLookoutPage);
end;

end.
