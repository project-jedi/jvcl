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
  Windows, SysUtils, Classes, ImgList, ActnList,
  {$IFDEF COMPILER5}
  Forms,
  {$ENDIF COMPILER5}
  {$IFDEF COMPILER6_UP}
  DesignEditors, DesignIntf,
  {$ELSE}
  DsgnIntf,
  {$ENDIF COMPILER6_UP}
  FiltEdit,
  {$IFNDEF COMPILER7_UP}
  ExptIntf,
  {$ENDIF !COMPILER7_UP}
  ToolsAPI,
  JclSchedule,
  JvDsgnConsts,
  JvTrayIcon, JvThumbImage, JvThumbnails, JvThumbViews, JvBalloonHint,
  JvEditor, JvHLEditor, JvHLEditorPropertyForm, JvHLParser, JvEditorCommon,
  JvUnicodeEditor, JvUnicodeHLEditor, JvImagesViewer, JvImageListViewer,
  JvOwnerDrawViewer, JvHLEditEditor, JvScheduleEditors,
  JvGammaPanel, JvLinkLabel, JvLookOut, JvOutlookBar, JvScheduledEvents,
  JvTimeLine, JvTMTimeLine, JvValidateEdit, JvChart, JvTimeLineEditor,
  JvOutlookBarEditors, JvLookoutEditor, 
  {$IFDEF JVCLThemesEnabled}
  JvTabBarXPPainter,
  {$ENDIF JVCLThemesEnabled}
  JvTabBar;

{$R JvCustomReg.dcr}

type
  TCustomActionClass = class of TCustomAction;

  { TJvStdEditorActionsRes is used to copy the VCL's standard edit actions
    properties to the JVCL standard edit actions }
  TJvStdEditorActionsRes = class(TComponent)
  private
    FStandardActions: TComponent;
    FActionList: TActionList;
  public
    constructor Create(AOwner: TComponent); override;

    function CreateAction(AActionClass: TCustomActionClass;
      const AStandardActionClassName: string): TCustomAction;
  end;

function FindComponentByClassName(AOwner: TComponent; const AClassName: string): TComponent;
var
  I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    Result := AOwner.Components[I];
    if AnsiSameText(Result.ClassName, AClassName) then
      Exit;
  end;
  Result := nil;
end;

function FindComponentByClass(AOwner: TComponent; AComponentClass: TComponentClass): TComponent;
var
  I: Integer;
begin
  for I := 0 to AOwner.ComponentCount - 1 do
  begin
    Result := AOwner.Components[I];
    if Result.ClassType = AComponentClass then
      Exit;
  end;
  Result := nil;
end;

{ Find the TStandardActions class (dclstdXX.bpl) }
function ModuleEnumProc(HInstance: Integer; Data: Pointer): Boolean;
var
  StandardActionsClass: TComponentClass;
begin
  StandardActionsClass := TComponentClass(GetProcAddress(HMODULE(HInstance), '@Actnres@TStandardActions@'));
  if StandardActionsClass <> nil then
  begin
    TJvStdEditorActionsRes(Data).FStandardActions := StandardActionsClass.Create(Data);
    Result := False;
  end
  else
    Result := True;
end;

constructor TJvStdEditorActionsRes.Create(AOwner: TComponent);
var
  StdActionList: TActionList;
begin
  inherited Create(AOwner);
  EnumModules(ModuleEnumProc, Self);
  if FStandardActions <> nil then
  begin
    StdActionList := TActionList(FindComponentByClass(FStandardActions, TActionList));
    if StdActionList <> nil then
    begin
      FActionList := TActionList.Create(Self);
      FActionList.Images := StdActionList.Images;

      { Create the JVCL standard edit actions }
      CreateAction(TJvEditCut, 'TEditCut');
      CreateAction(TJvEditCopy, 'TEditCopy');
      CreateAction(TJvEditPaste, 'TEditPaste');
      CreateAction(TJvEditSelectAll, 'TEditSelectAll');
      CreateAction(TJvEditUndo, 'TEditUndo');
      CreateAction(TJvEditDelete, 'TEditDelete');
    end;
  end;
end;

function TJvStdEditorActionsRes.CreateAction(AActionClass: TCustomActionClass;
  const AStandardActionClassName: string): TCustomAction;
var
  StdAction: TCustomAction;
begin
  Result := AActionClass.Create(Self);
  Result.ActionList := FActionList;

  { Copy the localized properties }
  StdAction := TCustomAction(FindComponentByClassName(FStandardActions, AStandardActionClassName));
  if TObject(StdAction) is TCustomAction then
  begin
    Result.Caption := StdAction.Caption;
    //Result.Category := StdAction.Category; is overwritten by the IDE
    Result.Hint := StdAction.Hint;
    Result.Visible := StdAction.Visible;
    Result.Enabled := StdAction.Enabled;
    Result.ShortCut := StdAction.ShortCut;
    Result.Checked := StdAction.Checked;
    Result.HelpContext := StdAction.HelpContext;
    Result.ImageIndex := StdAction.ImageIndex;
  end;
end;


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

  RegisterComponents(RsPaletteBarPanel, [TJvTabBar, TJvModernTabBarPainter]);
  {$IFDEF JVCLThemesEnabled}
  RegisterComponents(RsPaletteBarPanel, [TJvTabBarXPPainter]);
  {$ENDIF JVCLThemesEnabled}

  RegisterComponents(RsPaletteLabel, [TJvLinkLabel]);
  RegisterComponents(RsPaletteVisual, [TJvTimeLine, TJvTMTimeLine, TJvChart]);
  RegisterComponents(RsPaletteNonVisual, [TJvScheduledEvents]);
  RegisterComponents(RsPaletteEdit, [TJvEditor, TJvHLEditor,
    TJvWideEditor, TJvWideHLEditor, TJvHLEdPropDlg]);
  RegisterActions(RsJVCLEditActionsCategory, [TJvEditCut, TJvEditCopy, TJvEditPaste,
    TJvEditSelectAll, TJvEditUndo, TJvEditDelete], TJvStdEditorActionsRes);
  RegisterComponents(RsPaletteImageAnimator, [TJvThumbView, TJvThumbnail,
    TJvThumbImage]);
  RegisterComponents(RsPaletteVisual, [TJvImagesViewer, TJvImageListViewer,
    TJvOwnerDrawViewer]);
  RegisterComponents(RsPaletteNonVisual, [TJvTrayIcon, TJvBalloonHint]);

  RegisterPropertyEditor(TypeInfo(Integer), TJvCustomOutlookBar,
    cActivePageIndex, TJvOutlookBarActivePageProperty);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarPages), TJvCustomOutlookBar,
    '', TJvOutlookBarPagesProperty);
  RegisterPropertyEditor(TypeInfo(TJvOutlookBarButtons), TJvOutlookBarPage,
    '', TJvOutlookBarPagesProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvOutlookBarButton,
    cImageIndex, TJvOutlookBarButtonImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvOutlookBarPage,
    cImageIndex, TJvOutlookBarPageImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvLookOutButton,
    cImageIndex, TJvLookOutImageIndexProperty);
  RegisterPropertyEditor(TypeInfo(TImageIndex), TJvExpressButton,
    cImageIndex, TJvLookOutImageIndexProperty);

  RegisterPropertyEditor(TypeInfo(TJvColors), TJvHLEditor,
    cColors, TJvHLEditorColorProperty);
  RegisterPropertyEditor(TypeInfo(TJvColors), TJvWideHLEditor,
    cColors, TJvHLEditorColorProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvThumbView,
    cFilter, TFilterProperty);
  RegisterComponentEditor(TJvHLEdPropDlg, TJvHLEdPropDlgEditor);
  RegisterPropertyEditor(TypeInfo(IJclSchedule), TJvEventCollectionItem,
    cSchedule, TJvScheduleProperty); // depends on TDateTimePicker
  RegisterComponentEditor(TJvCustomScheduledEvents, TJvSchedEventEditor);

  RegisterComponentEditor(TJvCustomOutlookBar, TJvOutlookBarEditor);
  RegisterComponentEditor(TJvCustomTimeLine, TJvTimeLineEditor);
  RegisterComponentEditor(TJvLookOut, TJvLookOutEditor);
  RegisterComponentEditor(TJvLookOutPage, TJvLookOutPageEditor);
  RegisterComponentEditor(TJvExpress, TJvExpressEditor);
  RegisterClass(TJvLookOutPage);
end;

end.
