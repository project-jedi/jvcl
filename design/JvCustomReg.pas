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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvCustomReg;

{$I jvcl.inc}

interface

procedure Register;

implementation

uses
  Classes,
  ImgList,
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  FiltEdit,
  {$IFNDEF COMPILER7_UP}
  ExptIntf,
  {$ENDIF COMPILER7_UP}
  ToolsAPI,
  JvDsgnConsts,
  JclSchedule,
  {$IFDEF VCL}
  JvTrayIcon, JvThumbImage, JvThumbnails, JvThumbViews, JvBalloonHint,
  JvEditor, JvHLEditor, JvHLEditorPropertyForm, JvHLParser, JvEditorCommon,
  JvUnicodeEditor, JvUnicodeHLEditor, JvImagesViewer, JvImageListViewer,
  JvOwnerDrawViewer,
  JvHLEditEditor, JvScheduleEditors,
  {$ENDIF VCL}
  JvGammaPanel, JvLinkLabel, JvLookOut, JvOutlookBar, JvScheduledEvents,
  JvTimeLine, JvTMTimeLine, JvValidateEdit, JvChart,
  JvTimeLineEditor, JvOutlookBarEditors, JvLookoutEditor;

{$IFDEF MSWINDOWS}
{$R ..\Resources\JvCustomReg.dcr}
{$ENDIF MSWINDOWS}
{$IFDEF UNIX}
{$R ../Resources/JvCustomReg.dcr}
{$ENDIF UNIX}

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
    TJvLookOut, {TJvLookOutPage, } TJvExpress]);
  RegisterComponents(RsPaletteLabel, [TJvLinkLabel]);
  RegisterComponents(RsPaletteVisual, [TJvTimeLine, TJvTMTimeLine, TJvChart]);
  RegisterComponents(RsPaletteNonVisual, [TJvScheduledEvents]);
  {$IFDEF VCL}
  RegisterComponents(RsPaletteEdit, [TJvEditor, TJvHLEditor,
    TJvWideEditor, TJvWideHLEditor, TJvHLEdPropDlg]);
  RegisterComponents(RsPaletteImageAnimator, [TJvThumbView, TJvThumbnail,
    TJvThumbImage]);
  RegisterComponents(RsPaletteVisual, [TJvImagesViewer, TJvImageListViewer,
    TJvOwnerDrawViewer]);
  RegisterComponents(RsPaletteNonVisual, [TJvTrayIcon, TJvBalloonHint]);
  {$ENDIF VCL}

  RegisterPropertyEditor(TypeInfo(Integer), TJvCustomOutlookBar,
    cActivePageIndex, TJvOutlookBarActivePageEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages), TJvCustomOutlookBar,
    '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarButtons), TJvOutlookBarPage,
    '', TJvOutlookBarPagesPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvOutlookBarButton,
    cImageIndex, TJvOutlookBarButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvOutlookBarPage,
    cImageIndex, TJvOutlookBarPageImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvLookOutButton,
    cImageIndex, TJvLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvExpressButton,
    cImageIndex, TJvLookOutImageIndexProperty);

  {$IFDEF VCL}
  RegisterPropertyEditor(TypeInfo(TJvColors), TJvHLEditor,
    cColors, TJvHLEditorColorProperty);
  RegisterPropertyEditor(TypeInfo(TJvColors), TJvWideHLEditor,
    cColors, TJvHLEditorColorProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvThumbView,
    cFilter, TFilterProperty);
  RegisterComponentEditor(TJvHLEdPropDlg, TJvHLEdPropDlgEditor);
  RegisterPropertyEditor(TypeInfo(IJclSchedule), TJvEventCollectionItem,
    cSchedule, TJvSchedulePropertyEditor); // depends on TDateTimePicker
  RegisterComponentEditor(TJvCustomScheduledEvents, TJvSchedEventComponentEditor);
  {$ENDIF VCL}

  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarComponentEditor);
  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);
  RegisterComponentEditor(TJvLookOut, TJvLookOutEditor);
  RegisterComponentEditor(TJvLookOutPage, TJvLookOutPageEditor);
  RegisterComponentEditor(TJvExpress, TJvExpressEditor);
  RegisterClass(TJvLookOutPage);
end;

end.
