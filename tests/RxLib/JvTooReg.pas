{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvTooReg.PAS, released on 2002-07-04.

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


unit JvTooReg;

interface

procedure Register;

implementation

{$IFDEF WIN32}
{$R *.Res}
{$ELSE}
{$R *.D16}
{$ENDIF}

uses Classes, SysUtils, Controls, Graphics, TypInfo, RTLConsts, DesignIntf, DesignEditors, VCLEditors, Consts,
  ExtCtrls, JvPictEdit, JvHook, JvPicClip, JvPlacemnt, JvPresrDsn, JvMinMaxEd, JvDualList,
  JvClipView, JvSpeedbar, JvSbEdit, JvDataConv, JvCalc, JvPageMngr, JvPgMngrEd, JvMrgMngr,
  JvStrHlder, JvShell, JvAppEvent, JvVCLUtils, JvTimerLst, JvTimLstEd, JvIcoList, JvIcoLEdit,
  {$IFDEF USE_RX_GIF} JvGIF, JvGIFCtrl, {$ENDIF} JvLConst, JvCtrls,
  {$IFDEF Delphi3_Up} JvResExp, {$ENDIF} JvMenus, JvMRUList,
  {$IFDEF WIN32} JvNotify, JvGrdCpt, JvGradEdit, {$ENDIF} JvHintProp;

{ TJvStringsEditor }

type
  TJvStringsEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  end;

procedure TJvStringsEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'STRINGS') = 0) then begin
    Prop.Edit;
    Continue := False;
  end;
end;

{ TJvComponentFormProperty }

type
  TJvComponentFormProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure TJvComponentFormProperty.GetValues(Proc: TGetStrProc);
begin
  inherited GetValues(Proc);
  if (Designer.Root is GetTypeData(GetPropType)^.ClassType) and
    (Designer.Root.Name <> '') then Proc(Designer.Root.Name);
end;

procedure TJvComponentFormProperty.SetValue(const Value: string);
var
  Component: TComponent;
begin
{$IFDEF WIN32}
  Component := Designer.GetComponent(Value);
{$ELSE}
  Component := Designer.Root.FindComponent(Value);
{$ENDIF}
  if ((Component = nil) or not (Component is GetTypeData(GetPropType)^.ClassType))
    and (CompareText(Designer.Root.Name, Value) = 0) then
  begin
    if not (Designer.Root is GetTypeData(GetPropType)^.ClassType) then
      raise EPropertyError.Create(ResStr(SInvalidPropertyValue));
    SetOrdValue(Longint(Designer.Root));
  end
  else inherited SetValue(Value);
end;

{ Designer registration }

procedure Register;
begin
{ Components }
  RegisterComponents(LoadStr(srRXTools), [TJvPicClip, TJvFormStorage,
    TJvFormPlacement, TJvWindowHook, TJvAppEvents, TJvSpeedBar, TJvCalculator,
    TJvTimerList, TJvPageManager, TJvMergeManager, TJvMRUManager, TJvSecretPanel,
    TJvStrHolder, TJvTrayIcon, TJvMainMenu, TJvPopupMenu,
    {$IFDEF WIN32} TJvFolderMonitor, {$ENDIF} TJvClipboardViewer,
    {$IFDEF WIN32} TJvGradientCaption, {$ENDIF} TJvDualListDialog
    {$IFNDEF Delphi4_Up}, TJvConverter {$ENDIF}]);

{$IFDEF Delphi3_Up}
  RegisterNonActiveX([TJvPicClip, TJvFormPlacement, TJvFormStorage, TJvWindowHook,
    TJvDualListDialog, TJvSecretPanel, TJvSpeedBar, TJvClipboardViewer,
    TJvPageManager, TJvMergeManager, TJvMRUManager, TJvAppEvents, TJvTimerList,
    TJvTrayIcon, TJvFolderMonitor, TJvGradientCaption], axrComponentOnly);
{$ENDIF Delphi3_Up}

{ TJvPicClip }
  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);

{ TJvStrHolder }
  RegisterComponentEditor(TJvStrHolder, TJvStringsEditor);

{ TJvFormPlacement }
  RegisterPropertyEditor(TypeInfo(TJvWinMinMaxInfo), TJvFormPlacement,
    'MinMaxInfo', TMinMaxProperty);

{ TJvFormStorage }
  RegisterComponentEditor(TJvFormStorage, TJvFormStorageEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvFormStorage, 'StoredProps',
    TJvStoredPropsProperty);

{ TJvWindowHook }
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvWindowHook,
    'WinControl', TJvComponentFormProperty);

{ TJvSpeedBar }
  RegisterNoIcon([TJvSpeedItem, TJvSpeedbarSection]);
  RegisterComponentEditor(TJvSpeedBar, TJvSpeedbarCompEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedItem, 'BtnCaption', THintProperty);

{ TJvPageManager }
  RegisterNoIcon([TJvPageProxy]);
  RegisterComponentEditor(TJvPageManager, TJvPageManagerEditor);
  RegisterPropertyEditor(TypeInfo(TList), TJvPageManager, 'PageProxies',
    TJvProxyListProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPageProxy, 'PageName',
    TJvPageNameProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvPageManager, 'PriorBtn',
    TJvPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvPageManager, 'NextBtn',
    TJvPageBtnProperty);

{ TMergeManager }
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvMergeManager,
    'MergeFrame', TJvComponentFormProperty);

{ TJvTimerList }
  RegisterNoIcon([TJvTimerEvent]);
  RegisterComponentEditor(TJvTimerList, TJvTimersCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TList), TJvTimerList, 'Events',
    TJvTimersItemListProperty);

{ TJvTrayIcon }
  RegisterPropertyEditor(TypeInfo(TJvIconList), nil, '', TIconListProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvTrayIcon, 'Hint',
    TStringProperty);
{$IFDEF Delphi4_Up}

{ JvMenus }
  RegisterPropertyEditor(TypeInfo(Boolean), TJvMainMenu, 'OwnerDraw', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvPopupMenu, 'OwnerDraw', nil);
{$ENDIF}

{$IFDEF USE_RX_GIF}
{ TJvGIFAnimator }
  RegisterComponentEditor(TJvGIFAnimator, TJvGraphicsEditor);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TJvPictProperty);
  RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TJvGraphicPropertyEditor);
  RegisterComponentEditor(TImage, TJvGraphicsEditor);

{$IFDEF WIN32}
{ TJvGradientCaption }
  RegisterComponentEditor(TJvGradientCaption, TGradientCaptionEditor);
 {$IFNDEF Delphi3_Up}
  RegisterPropertyEditor(TypeInfo(TJvCaptionList), TJvGradientCaption, '',
    TGradientCaptionsProperty);
 {$ENDIF}
{$ENDIF}

{$IFDEF Delphi3_Up}
{ Project Resource Expert }
  RegisterResourceExpert;
{$ENDIF}
end;

end.
