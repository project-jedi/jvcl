{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvToolReg.PAS, released on 2002-07-04.

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

unit JvToolReg;

interface

procedure Register;

implementation

{.$R *.Res}

uses
  Classes, SysUtils, Controls, TypInfo, Consts,
  {$IFDEF COMPILER6_UP}
  RTLConsts, DesignIntf, DesignEditors, VCLEditors,
  {$ELSE}
  LibIntf, DsgnIntf,
  {$ENDIF}
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  Graphics, ExtCtrls,
  {$ENDIF}
  {$IFDEF USE_JV_GIF}
  JvGIF, JvGIFCtrl,
  {$ENDIF}
  JvxCtrls,
  JvMenus, JvMRUList,
  JvGrdCpt, JvGradEdit,
  JvPictEdit, JvWndProcHook, JvPicClip, JvPlacemnt, JvPresrDsn, JvMinMaxEd, JvDualList,
  JvClipView, JvSpeedbar, JvSbEdit, JvDataConv, JvCalc, JvPageMngr, JvPgMngrEd, JvMrgMngr,
  JvStrHlder, JvAppEvent, JvVCLUtils, JvTimerLst, JvTimLstEd, JvIcoList, JvIcoLEdit,
  JvDsgnEditors, JvxDConst;


procedure Register;
begin
  RegisterComponents(srJvXToolsPalette, [TJvPicClip, TJvFormStorage,
    TJvFormPlacement, TJvWindowHook, TJvAppEvents, TJvSpeedBar, TJvCalculator,
      TJvTimerList, TJvPageManager, TJvMergeManager, TJvMRUManager, TJvSecretPanel,
      TJvStrHolder, TJvMainMenu, TJvPopupMenu, TJvClipboardViewer,
      TJvxGradientCaption, TJvDualListDialog]);

  RegisterNonActiveX([TJvPicClip, TJvFormPlacement, TJvFormStorage, TJvWindowHook,
    TJvDualListDialog, TJvSecretPanel, TJvSpeedBar, TJvClipboardViewer,
      TJvPageManager, TJvMergeManager, TJvMRUManager, TJvAppEvents, TJvTimerList,
      TJvxGradientCaption], axrComponentOnly);

  RegisterComponentEditor(TJvPicClip, TJvGraphicsEditor);
  RegisterComponentEditor(TJvStrHolder, TJvStringsEditor);
  RegisterPropertyEditor(TypeInfo(TJvWinMinMaxInfo), TJvFormPlacement,
    'MinMaxInfo', TMinMaxProperty);
  RegisterComponentEditor(TJvFormStorage, TJvFormStorageEditor);
  RegisterPropertyEditor(TypeInfo(TStrings), TJvFormStorage, 'StoredProps',
    TJvStoredPropsProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvWindowHook,
    'WinControl', TJvComponentFormProperty);
  RegisterNoIcon([TJvSpeedItem, TJvSpeedbarSection]);
  RegisterComponentEditor(TJvSpeedBar, TJvSpeedbarCompEditor);
  RegisterPropertyEditor(TypeInfo(TCaption), TJvSpeedItem, 'BtnCaption', TStringProperty);
  RegisterNoIcon([TJvPageProxy]);
  RegisterComponentEditor(TJvPageManager, TJvPageManagerEditor);
  RegisterPropertyEditor(TypeInfo(TList), TJvPageManager, 'PageProxies',
    TJvProxyListProperty);
  RegisterPropertyEditor(TypeInfo(string), TJvPageProxy, 'PageName', TJvPageNameProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvPageManager, 'PriorBtn', TJvPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TControl), TJvPageManager, 'NextBtn', TJvPageBtnProperty);
  RegisterPropertyEditor(TypeInfo(TWinControl), TJvMergeManager, 'MergeFrame', TJvComponentFormProperty);
  RegisterNoIcon([TJvTimerEvent]);
  RegisterComponentEditor(TJvTimerList, TJvTimersCollectionEditor);
  RegisterPropertyEditor(TypeInfo(TList), TJvTimerList, 'Events', TJvTimersItemListProperty);
  RegisterPropertyEditor(TypeInfo(TJvIconList), nil, '', TIconListProperty);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvMainMenu, 'OwnerDraw', nil);
  RegisterPropertyEditor(TypeInfo(Boolean), TJvPopupMenu, 'OwnerDraw', nil);

  {$IFDEF USE_JV_GIF}
  RegisterComponentEditor(TJvGIFAnimator, TJvGraphicsEditor);
  {$ENDIF}
  {$IFDEF JVCL_REGISTER_GLOBAL_DESIGNEDITORS}
  RegisterPropertyEditor(TypeInfo(TPicture), nil, '', TJvPictProperty);
  RegisterPropertyEditor(TypeInfo(TGraphic), nil, '', TJvGraphicPropertyEditor);
  RegisterComponentEditor(TImage, TJvGraphicsEditor);
  {$ENDIF}

  RegisterComponentEditor(TJvxGradientCaption, TGradientCaptionEditor);

  { Project Resource Expert }
  //mb RegisterResourceExpert;
end;

end.

