{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvxTooReg.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software          
All Rights Reserved.

Last Modified: 2002-07-04

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://JVCL.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}


unit JvxTooReg;

interface

procedure Register;

implementation

{$IFDEF WIN32}
{$R *.Res}
{$ELSE}
{$R *.D16}
{$ENDIF}

uses Classes, SysUtils, Controls, Graphics, TypInfo, RTLConsts, DesignIntf, DesignEditors, VCLEditors, Consts,
  ExtCtrls, JvxPictEdit, JvxHook, JvxPicClip, JvxPlacemnt, JvxPresrDsn, JvxMinMaxEd, JvxDualList,
  JvxClipView, JvxSpeedbar, JvxSbEdit, JvxDataConv, JvxCalc, JvxPageMngr, JvxPgMngrEd, JvxMrgMngr,
  JvxStrHlder, JvxShell, JvxAppEvent, JvxVCLUtils, JvxTimerLst, JvxTimLstEd, JvxIcoList, JvxIcoLEdit,
  {$IFDEF USE_RX_GIF} JvxGIF, JvxGIFCtrl, {$ENDIF} JvxLConst, JvxCtrls,
  {$IFDEF Delphi3_Up} JvxResExp, {$ENDIF} JvxMenus, JvxMRUList,
  {$IFDEF WIN32} JvxNotify, JvxGrdCpt, JvxGradEdit, {$ENDIF} JvxHintProp;

{ TJvxStringsEditor }

type
  TJvxStringsEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
  end;

procedure TJvxStringsEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
var
  PropName: string;
begin
  PropName := Prop.GetName;
  if (CompareText(PropName, 'STRINGS') = 0) then begin
    Prop.Edit;
    Continue := False;
  end;
end;

{ TJvxComponentFormProperty }

type
  TJvxComponentFormProperty = class(TComponentProperty)
  public
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

procedure TJvxComponentFormProperty.GetValues(Proc: TGetStrProc);
begin
  inherited GetValues(Proc);
  if (Designer.Root is GetTypeData(GetPropType)^.ClassType) and
    (Designer.Root.Name <> '') then Proc(Designer.Root.Name);
end;

procedure TJvxComponentFormProperty.SetValue(const Value: string);
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
  RegisterComponents(LoadStr(srRXTools), [TJvxPicClip, TJvxFormStorage,
    TJvxFormPlacement, TJvxWindowHook, TJvxAppEvents, TJvxSpeedBar, TJvxCalculator,
    TJvxTimerList, TJvxPageManager, TJvxMergeManager, TJvxMRUManager, TJvxSecretPanel,
    TJvxStrHolder, TJvxTrayIcon, TJvxMainMenu, TJvxPopupMenu,
    {$IFDEF WIN32} TJvxFolderMonitor, {$ENDIF} TJvxClipboardViewer,
    {$IFDEF WIN32} TJvxGradientCaption, {$ENDIF} TJvxDualListDialog
    {$IFNDEF Delphi4_Up}, TJvxConverter {$ENDIF}]);

{$IFDEF Delphi3_Up}
  RegisterNonActiveX([TJvxPicClip, TJvxFormPlacement, TJvxFormStorage, TJvxWindowHook,
    TJvxDualListDialog, TJvxSecretPanel, TJvxSpeedBar, TJvxClipboardViewer,
    TJvxPageManager, TJvxMergeManager, TJvxMRUManager, TJvxAppEvents, TJvxTimerList,
    TJvxTrayIcon, TJvxFolderMonitor, TJvxGradientCaption], axrComponentOnly);
{$ENDIF Delphi3_Up}

{ TJvxPicClip }
  RegisterComponentEditor(TJvxPicClip, TJvxGraphicsEditor);

{ TJvxStrHolder }
  RegisterComponentEditor(TJvxStrHolder, TJvxStringsEditor);

{ TJvxFormPlacement }
  RegisterPropertyEditor(TypeInfo(TJvxWinMinMaxInfo), TJvxFormPlacement,
    'MinMaxInfo', TMinMaxProperty);

{ TJvxFormStorage }
  RegisterComponentEditor(TJvxFormStorage, TJvxFormStorageEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvxFormStorage, 'StoredProps',
    TJvxStoredPropsProperty);

{ TJvxWindowHook }
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvxWindowHook,
    'WinControl', TJvxComponentFormProperty);

{ TJvxSpeedBar }
  RegisterNoIcon([TJvxSpeedItem, TJvxSpeedbarSection]);
  RegisterComponentEditor(TJvxSpeedBar, TJvxSpeedbarCompEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvxSpeedItem, 'BtnCaption', THintProperty);

{ TJvxPageManager }
  RegisterNoIcon([TJvxPageProxy]);
  RegisterComponentEditor(TJvxPageManager, TJvxPageManagerEditor);
  RegisterPropertyEditor(TypeInfo(TList), TJvxPageManager, 'PageProxies',
    TJvxProxyListProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxPageProxy, 'PageName',
    TJvxPageNameProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvxPageManager, 'PriorBtn',
    TJvxPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvxPageManager, 'NextBtn',
    TJvxPageBtnProperty);

{ TMergeManager }
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvxMergeManager,
    'MergeFrame', TJvxComponentFormProperty);

{ TJvxTimerList }
  RegisterNoIcon([TJvxTimerEvent]);
  RegisterComponentEditor(TJvxTimerList, TJvxTimersCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TList), TJvxTimerList, 'Events',
    TJvxTimersItemListProperty);

{ TJvxTrayIcon }
  RegisterPropertyEditor(TypeInfo(TJvxIconList), nil, '', TIconListProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvxTrayIcon, 'Hint',
    TStringProperty);
{$IFDEF Delphi4_Up}

{ JvxMenus }
  RegisterPropertyEditor(TypeInfo(Boolean), TJvxMainMenu, 'OwnerDraw', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvxPopupMenu, 'OwnerDraw', nil);
{$ENDIF}

{$IFDEF USE_RX_GIF}
{ TJvxGIFAnimator }
  RegisterComponentEditor(TJvxGIFAnimator, TJvxGraphicsEditor);
{$ENDIF}

  RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TJvxPictProperty);
  RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TJvxGraphicPropertyEditor);
  RegisterComponentEditor(TImage, TJvxGraphicsEditor);

{$IFDEF WIN32}
{ TJvxGradientCaption }
  RegisterComponentEditor(TJvxGradientCaption, TGradientCaptionEditor);
 {$IFNDEF Delphi3_Up}
  RegisterPropertyEditor(TypeInfo(TJvxCaptionList), TJvxGradientCaption, '',
    TGradientCaptionsProperty);
 {$ENDIF}
{$ENDIF}

{$IFDEF Delphi3_Up}
{ Project Resource Expert }
  RegisterResourceExpert;
{$ENDIF}
end;

end.
