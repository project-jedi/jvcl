{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvUCB.PAS, released on 2002-05-26.

The Initial Developer of the Original Code is Peter Thörnqvist [peter3@peter3.com]
Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
All Rights Reserved.

Contributor(s):            

Last Modified: 2002-05-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvP3Reg;
interface

uses
  Classes;

procedure Register;

implementation
{$R JvP3Reg.dcr}
uses
  JvArrowBtn, JvBmpAnim, JvCaptionButton,
  JvColorCombo,
  {$IFNDEF D6PersonalEdition}  JvProgBarEx,  {$ENDIF}
  JvDriveCtrls, JvFindReplace, JvInstallLabel, JvLineEdit, JvRollOut, JvScrollPanel,
  JvTransBtn2, JvTimeLine, JvTimeLineEdit, JvShFileOp, JvAppHotKey,

    JvCaptionPanel, JvColorBtn, JvColorBox,JvColorForm, JvOutEdit,
  JvImagewindow, JvListComb,JvLookout, JvProfiler32,JvRegTV,
  JvSearchFiles, JvTipWin,JvTransBtn, ImgList,JvLCProperty,JvTipProperty,

  JvChangeNotify, Controls,JvEnterTab, JvFindFiles2,
  JvFileInfo, JvItemsPanel, JvDsgnEditors, JvCntScr, JvShBrowse, JvCmdEdit,
  JvTMTL, JvCalendar, JvUCB, JvChNtfyProperty, JvShBrProperty, JvOLBar, JvOLBarEditor,JvSHFmt
{$IFDEF DELPHI5},DsgnIntf {$ENDIF}{$IFDEF DELPHI6_UP},DesignEditors, DesignIntf {$ENDIF}

;

const
  cPage = 'JVCL P3';

procedure Register;
begin
  RegisterComponentEditor(TJvCustomTimeLine, TTimeLineEditor);
  RegisterComponents(cPage, [TJvArrowButton, TJvBmpAnimator, TJvCaptionButton,
    TJvColorComboBox, TJvFontComboBox2,
{$IFNDEF D6PersonalEdition} TJvProgressBar2,{$ENDIF}
      TJvDriveCombo, TJvDriveList, TJvFindReplace, TJvInstallLabel, TJvLineCombo, TJvLineEdit,
      TJvRollOut, TJvScrollingWindow, TJvDivider,TJvSHFileOperation, TJvTransparentButton2,
      TJvTimeline, TJvDirectoryListBox, TJvFileListBox,TJvApplicationHotKey,TJvFindFilesDialog2,
      TJvFormatDrive2]);

{$IFNDEF D6PersonalEdition}
      RegisterComponents('JEDI-DB', [TJvDBProgressBar]);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvLookoutButton, 'ImageIndex', TLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvExpressButton, 'ImageIndex', TLookOutImageIndexProperty);
  RegisterComponentEditor(TJvTipWindow,TJvTipPropertyEditor);
  RegisterComponentEditor(TJvLookOut,TLookOutEditor);
  RegisterComponentEditor(TJvLookOutPage,TLookOutPageEditor);
  RegisterComponentEditor(TJvListBox3,TJvListCombProperty);
  RegisterComponentEditor(TJvComboBox3,TJvListCombProperty);
  RegisterComponentEditor(TJvExpress,TExpressEditor);

  RegisterComponents(cPage,[TJvLookOut,TJvExpress,TJvLookOutButton,TJvExpressButton]);
  RegisterClass(TJvLookOutPage);

  RegisterComponents(cPage,[TJvSearchFiles,TJvCaptionPanel,TJvColorButton,TJvColorBox,
    TJvColorSquare,TJvComboBox3,TJvImageBox2,TJvImageWindow,TJvListBox3,
    TJvProfiler,TJvRegistryTreeView,TJvTipWindow,TJvTransparentButton]);


 RegisterPropertyEditor(TypeInfo(string),TJvChangeItem,'Directory',TDirectoryPropertyEditor);
  RegisterPropertyEditor(TypeInfo(TDateTime),nil,'',TDateTimeExProperty);
  RegisterPropertyEditor(TypeInfo(TDate),nil,'',TDateExProperty);
  RegisterPropertyEditor(TypeInfo(TTime),nil,'',TTimeExProperty);
  RegisterComponentEditor(TJvChangeNotify,TJvChangeNotifyEditor);
  RegisterComponentEditor(TJvShellBrowser,TJvShellBrowserEditor);
  RegisterComponentEditor(TJvCustomOutlookBar, TOLBarComponentEditor);
  RegisterPropertyEditor(typeinfo(integer),TJvCustomOutlookBar,'ActivePageIndex',TOLBarActivePageEditor);

  RegisterComponents(cPage,[TJvChangeNotify,TJvMonthCalendar2,
    TJvFileInfo,TJvItemsPanel,TJvContentScroller,TJvEnterAsTab,
    TJvShellBrowser,TJvCommandEdit,TJvTMTimeline,TJvOutlookBar,TJvUninstallComboBox,TJvUninstallListBox]);
	  
end;
end.

