//Band objects wrapper classes. }

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvBandObject.PAS, released on 2001-07-10.

The Initial Developer of the Original Code is Chiang Seng Chang <cs@ctzen.com>
Portions created by Chiang Seng Chang are Copyright (C) 2001 Chiang Seng Chang.
All Rights Reserved.

Contributor(s): ______________________________________.

Last Modified: 2001-mm-dd

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit jvBandObject;

interface

uses
  Windows, ComObj, ShlObj, ActiveX, Classes, Menus,
  jvBandForms;

const
  {:Desk band category ID.
  Used to register the 'Implemented Categories' of the band object.
  @seeAlso <see const="CATID_INFOBAND">
  @seeAlso <see const="CATID_COMMBAND">
  }
  CATID_DESKBAND = '{00021492-0000-0000-C000-000000000046}';

  {:Info band category ID.
  Used to register the 'Implemented Categories' of the band object.
  @seeAlso <see const="CATID_DESKBAND">
  @seeAlso <see const="CATID_COMMBAND">
  }
  CATID_INFOBAND = '{00021493-0000-0000-C000-000000000046}';

  {:Comm band category ID.
  Used to register the 'Implemented Categories' of the band object.
  @seeAlso <see const="CATID_DESKBAND">
  @seeAlso <see const="CATID_INFOBAND">
  }
  CATID_COMMBAND = '{00021494-0000-0000-C000-000000000046}';

type
  // Band Object Factory Classes

  {:Utimate base class for band object factories.
  This is the 'mother' of all band object factory classes.
  @cat jvBandObjectFactoryComponents
  @seeAlso <see class="TzToolBandObjectFactory">
  @seeAlso <see class="TzCatBandObjectFactory">
  @seeAlso <see class="TzDeskBandObjectFactory">
  @seeAlso <see class="TzExplorerBarObjectFactory">
  @seeAlso <see class="TzInfoBandObjectFactory">
  @seeAlso <see class="TzCommBandObjectFactory">
  }
  TzCustomBandObjectFactory = class(TComObjectFactory)
  private
    function GetClassIDString: string;
  public
    //: The band object's class ID as a string.
    property ClassIDString: string read GetClassIDString;
  end;

  {:Base class for IE tool band object factories.
  Concrete tool band object factories inherit from this class.
  @cat jvBandObjectFactoryComponents
  @seeAlso <see class="TzCustomBandObjectFactory">
  @seeAlso <see class="TzCatBandObjectFactory">
  @seeAlso <see class="TzDeskBandObjectFactory">
  @seeAlso <see class="TzExplorerBarObjectFactory">
  @seeAlso <see class="TzInfoBandObjectFactory">
  @seeAlso <see class="TzCommBandObjectFactory">
  }
  TzToolBandObjectFactory = class(TzCustomBandObjectFactory)
  public
    //: Register or unregister the tool band object.
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  {:Base class for band object factories with 'implemented categories'.
  Band objects with 'implemented categories' (i.e. info, comm and desk bands) will
  have their factories inherited from this class.
  @cat jvBandObjectFactoryComponents
  @seeAlso <see class="TzCustomBandObjectFactory">
  @seeAlso <see class="TzToolBandObjectFactory">
  @seeAlso <see class="TzDeskBandObjectFactory">
  @seeAlso <see class="TzExplorerBarObjectFactory">
  @seeAlso <see class="TzInfoBandObjectFactory">
  @seeAlso <see class="TzCommBandObjectFactory">
  }
  TzCatBandObjectFactory = class(TzCustomBandObjectFactory)
  protected
    {:Get the 'implemented categories' ID.
    Subclassed factory classes has to supply the CatID by implementing
    this function.
    }
    function GetImplCatID: TGUID; virtual; abstract;
  public
    //: Register or unregister the band object.
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  {:Base class for desk band object factories.
  Concrete desk band object factories inherit from this class.
  @cat jvBandObjectFactoryComponents
  @seeAlso <see class="TzCustomBandObjectFactory">
  @seeAlso <see class="TzToolBandObjectFactory">
  @seeAlso <see class="TzCatBandObjectFactory">
  @seeAlso <see class="TzExplorerBarObjectFactory">
  @seeAlso <see class="TzInfoBandObjectFactory">
  @seeAlso <see class="TzCommBandObjectFactory">
  }
  TzDeskBandObjectFactory = class(TzCatBandObjectFactory)
  protected
    //: Returns <see const="CATID_DESKBAND">
    function GetImplCatID: TGUID; override;
  end;

  {:Base class for IE explorer bar object factories.
  IE Explorer bars (i.e. info and comm bands) will have
  their factories inherited from this class.
  @cat jvBandObjectFactoryComponents
  @seeAlso <see class="TzCustomBandObjectFactory">
  @seeAlso <see class="TzToolBandObjectFactory">
  @seeAlso <see class="TzCatBandObjectFactory">
  @seeAlso <see class="TzDeskBandObjectFactory">
  @seeAlso <see class="TzInfoBandObjectFactory">
  @seeAlso <see class="TzCommBandObjectFactory">
  }
  TzExplorerBarObjectFactory = class(TzCatBandObjectFactory)
  private
    function BarSize: string;
  protected
    {:Get the location of the HTML file for the explorer bar.
    If the explorer bar is to display HTML, subclassed factory
    classes will need to override this method and return a value.
    }
    function GetURL: string; virtual;
    {:Get the default width of the explorer bar.
    Not useful ?
    @seeAlso <see method="GetBarHeight">
    }
    function GetBarWidth: Word; virtual;
    {:Get the default height of the explorer bar.
    Not useful ?
    @seeAlso <see method="GetBarWidth">
    }
    function GetBarHeight: Word; virtual;
  public
    //: Register or unregister the explorer bar object.
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  {:Base class for info band object factories.
  Concrete info band object factories inherit from this class.
  @cat jvBandObjectFactoryComponents
  @seeAlso <see class="TzCustomBandObjectFactory">
  @seeAlso <see class="TzToolBandObjectFactory">
  @seeAlso <see class="TzCatBandObjectFactory">
  @seeAlso <see class="TzDeskBandObjectFactory">
  @seeAlso <see class="TzExplorerBarObjectFactory">
  @seeAlso <see class="TzCommBandObjectFactory">
  }
  TzInfoBandObjectFactory = class(TzExplorerBarObjectFactory)
  protected
    //: Returns <see const="CATID_INFOBAND">
    function GetImplCatID: TGUID; override;
  end;

  {:Base class for comm band object factories.
  Concrete comm band object factories inherit from this class.
  @cat jvBandObjectFactoryComponents
  @seeAlso <see class="TzCustomBandObjectFactory">
  @seeAlso <see class="TzToolBandObjectFactory">
  @seeAlso <see class="TzCatBandObjectFactory">
  @seeAlso <see class="TzDeskBandObjectFactory">
  @seeAlso <see class="TzExplorerBarObjectFactory">
  @seeAlso <see class="TzInfoBandObjectFactory">
  }
  TzCommBandObjectFactory = class(TzExplorerBarObjectFactory)
  protected
    //: Returns <see const="CATID_COMMBAND">
    function GetImplCatID: TGUID; override;
  end;

  // Band Object Classes

  {:Utimate base class for band objects.
  This is the 'mother' of all band object classes.
  @cat jvBandObjectComponents
  @seeAlso <see class="TzToolBandObject">
  @seeAlso <see class="TzContextMenuBandObject">
  @seeAlso <see class="TzDeskBandObject">
  @seeAlso <see class="TzInfoBandObject">
  @seeAlso <see class="TzCommBandObject">
  }
  TzCustomBandObject = class(TComObject, IDeskBand, IObjectWithSite, IPersistStream, IInputObject)
  private
    FBandForm: TjvBandForm;
    FBandID: DWORD;
    FViewMode: DWORD;
    FSite: IInputObjectSite;
    FOleCommandTarget: IOleCommandTarget;
  protected
    {:Creates the band form.
    Concrete band object classes must override this and
    return a band form.
    }
    function CreateBandForm(const ParentWnd: HWnd): TjvBandForm; virtual; abstract;
  public
    {$IFNDEF T2H}
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    {$ENDIF}
    {:Notify Windows that band info has changed.
    Call this function after band info field(s) has been updated.
    @seeAlso <see property="OleCommandTarget">
    }
    function BandInfoChanged: HRESULT;
    {:Maximize the band object.
    @seeAlso <see property="OleCommandTarget">
    }
    function Maximize: HRESULT;
    {:Show all band objects.
    @seeAlso <see method="ShowMeOnly">
    @seeAlso <see method="HideAllBands">
    @seeAlso <see property="OleCommandTarget">
    }
    function ShowAllBands: HRESULT;
    {:Hide all band objects.
    @seeAlso <see method="ShowMeOnly">
    @seeAlso <see method="ShowAllBands">
    @seeAlso <see property="OleCommandTarget">
    }
    function HideAllBands: HRESULT;
    {:Hide all band objects except self.
    @seeAlso <see method="ShowAllBands">
    @seeAlso <see method="HideAllBands">
    @seeAlso <see property="OleCommandTarget">
    }
    function ShowMeOnly: HRESULT;
    {: Contains the band ID supplied by IDeskBand::GetBandInfo.
    @seeAlso <see method="GetBandInfo">
    @seeAlso <see property="ViewMode">
    }
    property BandID: DWORD read FBandID;
    {: Contains the view mode supplied by IDeskBand::GetBandInfo.
    Possible values:
    <ul>
    <li>DBIF_VIEWMODE_NORMAL
    <li>DBIF_VIEWMODE_VERTICAL
    <li>DBIF_VIEWMODE_FLOATING
    <li>DBIF_VIEWMODE_TRANSPARENT
    </ul>
    @seeAlso <see method="GetBandInfo">
    @seeAlso <see property="BandID">
    }
    property ViewMode: DWORD read FViewMode;
    {: Contains the band object's site supplied by IObjectWithSite::SetSite.
    @seeAlso <see property="OleCommandTarget">
    @seeAlso <see method="SetSite">
    @seeAlso <see method="GetSite">
    }
    property Site: IInputObjectSite read FSite;
    {: Contains the band object's site as IOleCommandTarget.
    @seeAlso <see property="Site">
    @seeAlso <see method="BandInfoChanged">
    @seeAlso <see method="Maximize">
    @seeAlso <see method="ShowMeOnly">
    @seeAlso <see method="ShowAllBands">
    @seeAlso <see method="HideAllBands">
    }
    property OleCommandTarget: IOleCommandTarget read FOleCommandTarget;
    // IDeskBand
    {:Implements IDeskBand::GetBandInfo.
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IDeskBand/GetBandInfo.htm#IDeskBand__GetBandIn">MSDN<a> for details.
    @seeAlso <see property="BandID">
    @seeAlso <see property="ViewMode">
    @seeAlso <see class="TjvBandForm" property="Caption">
    @seeAlso <see class="TjvBandForm" property="BandMinSize">
    @seeAlso <see class="TjvBandForm" property="BandMaxSize">
    @seeAlso <see class="TjvBandForm" property="BandIntegral">
    @seeAlso <see class="TjvBandForm" property="BandActualSize">
    @seeAlso <see class="TjvBandForm" property="BandModeFlags">
    @seeAlso <see class="TjvBandForm" property="Color">
    }
    function GetBandInfo(dwBandID, dwViewMode: DWORD;
      var pdbi: TDeskBandInfo): HResult; virtual; stdcall;
    // IDockingWindow
    {:Implements IDockingWindow:ShowDW.
    Shows or hides the band form.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IDockingWindow/ShowDW.htm#IDockingWindow_ShowDW">MSDN<a> for details.
    @seeAlso <see method="CloseDW">
    @seeAlso <see method="ResizeBorderDW">
    }
    function ShowDW(fShow: BOOL): HResult; virtual; stdcall;
    {:Implements IDockingWindow:CloseDW.
    Closes the band form.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IDockingWindow/CloseDW.htm#IDockingWindow_CloseDW">MSDN<a> for details.
    @seeAlso <see method="ShowDW">
    @seeAlso <see method="ResizeBorderDW">
    }
    function CloseDW(dwReserved: DWORD): HResult; virtual; stdcall;
    {:Implements IDockingWindow:ResizeBorderDW.
    This method is never called for band objects.
    @seeAlso <see method="ShowDW">
    @seeAlso <see method="CloseDW">
    }
    function ResizeBorderDW(var prcBorder: TRect;
      punkToolbarSite: IUnknown; fReserved: BOOL): HResult; virtual; stdcall;
    // IOleWindow
    {:Implements IOleWindow:GetWindow.
    Returns the band form's handle.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/com/oin_ou2z_33cn.htm">MSDN<a> for details.
    @seeAlso <see method="ContextSensitiveHelp">
    }
    function GetWindow(out wnd: HWnd): HResult; virtual; stdcall;
    {:Implements IOleWindow:ContextSensitiveHelp.
    See <a href="http://msdn.microsoft.com/library/psdk/com/oin_ou2z_12i8.htm">MSDN<a> for details.
    @seeAlso <see method="GetWindow">
    }
    function ContextSensitiveHelp(fEnterMode: BOOL): HResult; virtual; stdcall;
    // IObjectWithSite
    {:Implements IObjectWidthSite:SetSite.
    See <a href="http://msdn.microsoft.com/library/psdk/com/oin_e2o_9ckl.htm">MSDN<a> for details.
    @seeAlso <see method="GetSite">
    @seeAlso <see property="Site">
    }
    function SetSite(const pUnkSite: IUnknown): HResult; virtual; stdcall;
    {:Implements IObjectWidthSite:GetSite.
    See <a href="http://msdn.microsoft.com/library/psdk/com/oin_e2o_6tb9.htm">MSDN<a> for details.
    @seeAlso <see method="SetSite">
    @seeAlso <see property="Site">
    }
    function GetSite(const riid: TIID; out site: IUnknown): HResult; virtual; stdcall;
    // IPersistStream
    {:Implements IPersistStream::IsDirty.
    Always returns False, unless subclass overrides.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/com/cmi_n2p_8lmh.htm">MSDN<a> for details.
    @seeAlso <see method="Load">
    @seeAlso <see method="Save">
    @seeAlso <see method="GetSizeMax">
    }
    function IsDirty: HResult; virtual; stdcall;
    {:Implements IPersistStream::Load.
    Does nothing, unless subclass overrides.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/com/cmi_n2p_54f8.htm">MSDN<a> for details.
    @seeAlso <see method="IsDirty">
    @seeAlso <see method="Save">
    @seeAlso <see method="GetSizeMax">
    }
    function Load(const stm: IStream): HResult; virtual; stdcall;
    {:Implements IPersistStream::Save.
    Does nothing, unless subclass overrides.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/com/cmi_n2p_3945.htm">MSDN<a> for details.
    @seeAlso <see method="IsDirty">
    @seeAlso <see method="Load">
    @seeAlso <see method="GetSizeMax">
    }
    function Save(const stm: IStream; fClearDirty: BOOL): HResult; virtual; stdcall;
    {:Implements IPersistStream::GetSizeMax.
    Always returns 0, unless subclass overrides.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/com/cmi_n2p_9xko.htm">MSDN<a> for details.
    @seeAlso <see method="IsDirty">
    @seeAlso <see method="Load">
    @seeAlso <see method="Save">
    }
    function GetSizeMax(out cbSize: Largeint): HResult; virtual; stdcall;
    // IPersist
    {:Implements IPersist::GetClassID.
    Returns the band object's class ID.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/com/cmi_n2p_1yn8.htm">MSDN<a> for details.
    }
    function GetClassID(out classID: TCLSID): HResult; virtual; stdcall;
    // IInputObject
    {:Implements IInputObject::UIActivateIO.
    Call band form's SetFocus if necessary.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IInputObject/UIActivateIO.htm#IInputObject__UIActi">MSDN<a> for details.
    @seeAlso <see method="HasFocusIO">
    @seeAlso <see method="TranslateAcceleratorIO">
    }
    function UIActivateIO(fActivate: BOOL; var lpMsg: TMsg): HResult; virtual; stdcall;
    {:Implements IInputObject::HasFocusIO.
    Returns band form's Focused.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IInputObject/HasFocusIO.htm#IInputObject__HasFoc">MSDN<a> for details.
    @seeAlso <see method="UIActivateIO">
    @seeAlso <see method="TranslateAcceleratorIO">
    }
    function HasFocusIO: HResult; virtual; stdcall;
    {:Implements IInputObject::TranslateAcceleratorIO.
    Does nothing, unless subclass overrides.<br><br>
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IInputObject/TranslateAcceleratorIO.htm#IInputObject__Transl">MSDN<a> for details.
    @seeAlso <see method="UIActivateIO">
    @seeAlso <see method="HasFocusIO">
    }
    function TranslateAcceleratorIO(var lpMsg: TMsg): HResult; virtual; stdcall;
  end;

  {:Base class for IE tool band objects.
  Concrete tool band objects inherit from this class.
  @cat jvBandObjectComponents
  @seeAlso <see class="TzCustomBandObject">
  @seeAlso <see class="TzContextMenuBandObject">
  @seeAlso <see class="TzDeskBandObject">
  @seeAlso <see class="TzInfoBandObject">
  @seeAlso <see class="TzCommBandObject">
  }
  TzToolBandObject = class(TzCustomBandObject)
  end;

  {:Base class for band objects which support context menu.
  Band objects which support context menu (i.e. info, comm and
  desk bands) will inherit from this class.
  @cat jvBandObjectComponents
  @seeAlso <see class="TzCustomBandObject">
  @seeAlso <see class="TzToolBandObject">
  @seeAlso <see class="TzDeskBandObject">
  @seeAlso <see class="TzInfoBandObject">
  @seeAlso <see class="TzCommBandObject">
  }
  { TzContextMenuBandObject
    For explorer bars (info bands & comm bands) and desk bands
    which supports the IContextMenu interface.
  }
  TzContextMenuBandObject = class(TzCustomBandObject, IContextMenu)
  public
    // IContextMenu
    {:Implements IContextMenu::QueryContextMenu.
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IContextMenu/QueryContextMenu.htm">MSDN<a> for details.
    @seeAlso <see method="InvokeCommand">
    @seeAlso <see method="GetCommandString">
    @seeAlso <see class="TjvBandForm" property="BandContextMenu">
    }
    function QueryContextMenu(hMenu: HMENU;
      indexMenu, idCmdFirst, idCmdLast, uFlags: UINT): HResult; virtual; stdcall;
    {:Implements IContextMenu::InvokeCommand.
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IContextMenu/InvokeCommand.htm">MSDN<a> for details.
    @seeAlso <see method="QueryContextMenu">
    @seeAlso <see method="GetCommandString">
    @seeAlso <see class="TjvBandForm" property="BandContextMenu">
    }
    function InvokeCommand(var lpici: TCMInvokeCommandInfo): HResult; virtual; stdcall;
    {:Implements IContextMenu::GetCommandString.
    See <a href="http://msdn.microsoft.com/library/psdk/shellcc/shell/IFaces/IContextMenu/GetCommandString.htm">MSDN<a> for details.
    @seeAlso <see method="QueryContextMenu">
    @seeAlso <see method="InvokeCommand">
    @seeAlso <see class="TjvBandForm" property="BandContextMenu">
    }
    function GetCommandString(idCmd, uType: UINT; pwReserved: PUINT;
      pszName: LPSTR; cchMax: UINT): HResult; virtual; stdcall;
  end;

  {:Base class for desk band objects.
  Concrete desk band objects inherit from this class.
  @cat jvBandObjectComponents
  @seeAlso <see class="TzCustomBandObject">
  @seeAlso <see class="TzToolBandObject">
  @seeAlso <see class="TzContextMenuBandObject">
  @seeAlso <see class="TzInfoBandObject">
  @seeAlso <see class="TzCommBandObject">
  }
  TzDeskBandObject = class(TzContextMenuBandObject)
  end;

  {:Base class for info band objects.
  Concrete info band objects inherit from this class.
  @cat jvBandObjectComponents
  @seeAlso <see class="TzCustomBandObject">
  @seeAlso <see class="TzToolBandObject">
  @seeAlso <see class="TzContextMenuBandObject">
  @seeAlso <see class="TzDeskBandObject">
  @seeAlso <see class="TzCommBandObject">
  }
  TzInfoBandObject = class(TzContextMenuBandObject)
  end;

  {:Base class for comm band objects.
  Concrete comm band objects inherit from this class.
  @cat jvBandObjectComponents
  @seeAlso <see class="TzCustomBandObject">
  @seeAlso <see class="TzToolBandObject">
  @seeAlso <see class="TzContextMenuBandObject">
  @seeAlso <see class="TzDeskBandObject">
  @seeAlso <see class="TzInfoBandObject">
  }
  TzCommBandObject = class(TzContextMenuBandObject)
  end;

implementation

uses
  {$IFDEF Debug}
  zTrace,
  {$ENDIF}
  Registry, SysUtils, Messages, Math,
  jvBandUtils;

function MakeHResult(sev, fac, code: LongWord): HRESULT;
begin
  Result := (sev shl 31) or (fac shl 16) or (code);
end;

// Band Object Factory Classes

{ TzCustomBandObjectFactory }

function TzCustomBandObjectFactory.GetClassIDString: string;
begin
  Result := GUIDToString(ClassID);
end;

{ TzToolBandObjectFactory }

procedure TzToolBandObjectFactory.UpdateRegistry(Register: Boolean);
begin
  if Register then
    inherited;
  with TRegistry.Create do
  try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKey('Software\Microsoft\Internet Explorer\Toolbar', True) then
    try
      if Register then
        WriteString(ClassIDString, Description)
      else
        DeleteValue(ClassIDString);
    finally
      CloseKey;
    end;
  finally
    Free;
  end;
  if not Register then
    inherited;
end;

{ TzCatBandObjectFactory }

procedure TzCatBandObjectFactory.UpdateRegistry(Register: Boolean);
var
  CatRegister: ICatRegister;
  ImplCatID: TGUID;
begin
  if Register then
    inherited;
  ImplCatID := GetImplCatID;
  CoInitialize(nil);
  CatRegister := ComObj.CreateComObject(CLSID_StdComponentCategoryMgr) as ICatRegister;
  if Register then
  begin
    CatRegister.RegisterClassImplCategories(ClassID, 1, @ImplCatID);
  end
  else
  begin
    CatRegister.UnregisterClassImplCategories(ClassID, 1, @ImplCatID);
    DeleteRegKey('CLSID\' + ClassIDString + '\Implemented Categories');
  end;
  CatRegister := nil;
  CoUninitialize();
  if not Register then
    inherited;
end;

{ TzDeskBandObjectFactory }

function TzDeskBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_DESKBAND);
end;

{ TzExplorerBarObjectFactory }

function TzExplorerBarObjectFactory.BarSize: string;
var
  S: string;
begin
  S := Format('%.4x', [GetBarWidth]);
  Result := Copy(S, 3, 2) + ',' + Copy(S, 1, 2) + ',';
  S := Format('%.4x', [GetBarHeight]);
  Result := Result + Copy(S, 3, 2) + ',' + Copy(S, 1, 2) + ',00,00,00,00';
end;

function TzExplorerBarObjectFactory.GetBarHeight: Word;
begin
  Result := 0;
end;

function TzExplorerBarObjectFactory.GetBarWidth: Word;
begin
  Result := 0;
end;

function TzExplorerBarObjectFactory.GetURL: string;
begin
  Result := '';
end;

procedure TzExplorerBarObjectFactory.UpdateRegistry(Register: Boolean);
begin
  if Register then
  begin
    inherited;
    if GetURL <> '' then
    begin
      CreateRegKey('CLSID\' + ClassIDString + '\Instance\CLSID', '', '{4D5C8C2A-D075-11D0-B416-00C04FB90376}');
      CreateRegKey('CLSID\' + ClassIDString + '\Instance\InitPropertyBag\Url', '', GetURL);
    end;
    if (GetBarWidth <> 0) or (GetBarHeight <> 0) then
    begin
      with TRegistry.Create do
      try
        RootKey := HKEY_CURRENT_USER;
        if OpenKey('Software\Microsoft\Internet Explorer\Explorer Bars\' + ClassIDString, True) then
        try
          WriteString('BarSize', BarSize)
        finally
          CloseKey;
        end;
      finally
        Free;
      end;
    end;
  end
  else
  begin
    with TRegistry.Create do
    try
      RootKey := HKEY_CURRENT_USER;
      if OpenKey('Software\Microsoft\Internet Explorer\Explorer Bars\' + ClassIDString, True) then
      try
        DeleteValue('BarSize');
      finally
        CloseKey;
      end;
      DeleteKey('Software\Microsoft\Internet Explorer\Explorer Bars\' + ClassIDString);
    finally
      Free;
    end;
    DeleteRegKey('CLSID\' + ClassIDString + '\Instance\InitPropertyBag\Url');
    DeleteRegKey('CLSID\' + ClassIDString + '\Instance\InitPropertyBag');
    DeleteRegKey('CLSID\' + ClassIDString + '\Instance\CLSID');
    DeleteRegKey('CLSID\' + ClassIDString + '\Instance');
    inherited;
  end;
end;

{ TzInfoBandObjectFactory }

function TzInfoBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_INFOBAND);
end;

{ TzCommBandObjectFactory }

function TzCommBandObjectFactory.GetImplCatID: TGUID;
begin
  Result := StringToGUID(CATID_COMMBAND);
end;

// Band Object Classes

{ TzCustomBandObject }

procedure TzCustomBandObject.AfterConstruction;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.AfterConstruction()');
  {$ENDIF}
  inherited;
  FBandForm := nil;
  FSite := nil;
  FOleCommandTarget := nil;
end;

procedure TzCustomBandObject.BeforeDestruction;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.BeforeDestruction()');
  {$ENDIF}
  if Assigned(FSite) then
    FSite := nil; // implicit Release
  if Assigned(FOleCommandTarget) then
    FOleCommandTarget := nil; // implicit Release
  if Assigned(FBandForm) then
    FreeAndNil(FBandForm);
  inherited;
end;

function TzCustomBandObject.BandInfoChanged: HRESULT;
var
  CGID_DeskBand: TGUID;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  vaIn := OleVariant(BandID);
  Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_BANDINFOCHANGED,
    OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

function TzCustomBandObject.Maximize: HRESULT;
var
  CGID_DeskBand: TGUID;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  vaIn := OleVariant(BandID);
  Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_MAXIMIZEBAND,
    OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

function TzCustomBandObject.HideAllBands: HRESULT;
var
  CGID_DeskBand: TGUID;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  vaIn := 0;
  Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_SHOWONLY,
    OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

function TzCustomBandObject.ShowAllBands: HRESULT;
var
  CGID_DeskBand: TGUID;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  vaIn := 1;
  Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_SHOWONLY,
    OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
end;

function TzCustomBandObject.ShowMeOnly: HRESULT;
var
  CGID_DeskBand: TGUID;
  Unknown: IUnknown;
  vaIn, vaOut: OleVariant;
begin
  if not Assigned(OleCommandTarget) then
  begin
    Result := E_FAIL;
    Exit;
  end;
  CGID_DeskBand := IDeskBand;
  if Self.QueryInterface(IUnknown, Unknown) <> S_OK then
  begin
    Result := E_FAIL;
    Exit;
  end;
  try
    vaIn := Unknown;
    Result := OleCommandTarget.Exec(@CGID_DeskBand, DBID_SHOWONLY,
      OLECMDEXECOPT_DODEFAULT, vaIn, vaOut);
  finally
    Unknown := nil;
  end;
end;

// IDeskBand

function TzCustomBandObject.GetBandInfo(dwBandID, dwViewMode: DWORD;
  var pdbi: TDeskBandInfo): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetBandInfo()');
  zTraceLog('  dwBandID=' + Format('0x%x', [dwBandID]));
  zTraceLog('  dwViewMode=' + Format('0x%x', [dwViewMode]));
  zTraceLog('  pdbi=' + Format('0x%p', [@pdbi]));
  zTraceLog('    dwMask=' + Format('0x%x', [pdbi.dwMask]));
  {$ENDIF}
  FBandID := dwBandID;
  FViewMode := dwViewMode;
  if not Assigned(FBandForm) then
  begin
    Result := E_UNEXPECTED;
    Exit;
  end;
  with pdbi, FBandForm do
  begin
    if (dwMask and DBIM_MINSIZE) <> 0 then
    begin
      ptMinSize := BandMinSize;
      {$IFDEF Debug}
      zTraceLog('  pdbi.ptMinSize=' + Format('(%d,%d)', [ptMinSize.x, ptMinSize.y]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_MAXSIZE) <> 0 then
    begin
      ptMaxSize := BandMaxSize;
      {$IFDEF Debug}
      zTraceLog('  pdbi.ptMaxSize=' + Format('(%d,%d)', [ptMaxSize.x, ptMaxSize.y]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_INTEGRAL) <> 0 then
    begin
      ptIntegral := BandIntegral;
      {$IFDEF Debug}
      zTraceLog('  pdbi.ptIntegral=' + Format('(%d,%d)', [ptIntegral.x, ptIntegral.y]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_ACTUAL) <> 0 then
    begin
      ptActual := BandActualSize;
      {$IFDEF Debug}
      zTraceLog('  pdbi.ptActual=' + Format('(%d,%d)', [ptActual.x, ptActual.y]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_TITLE) <> 0 then
    begin
      StringToWideChar(Caption, @wszTitle, Length(wszTitle));
      {$IFDEF Debug}
      zTraceLog('  pdbi.wszTitle=' + Format('%s', [Caption]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_MODEFLAGS) <> 0 then
    begin
      dwModeFlags := DBIMF_NORMAL;
      if bmfVariableHeight in BandModeFlags then
        dwModeFlags := dwModeFlags or DBIMF_VARIABLEHEIGHT;
      if bmfDebossed in BandModeFlags then
        dwModeFlags := dwModeFlags or DBIMF_DEBOSSED;
      if bmfBkColor in BandModeFlags then
        dwModeFlags := dwModeFlags or DBIMF_BKCOLOR;
      {$IFDEF Debug}
      zTraceLog('  pdbi.dwModeFlags=' + Format('0x%x', [dwModeFlags]));
      {$ENDIF}
    end;
    if (dwMask and DBIM_BKCOLOR) <> 0 then
    begin
      crBkgnd := Color;
      {$IFDEF Debug}
      zTraceLog('  pdbi.crBkgnd=' + Format('0x%x', [crBkgnd]));
      {$ENDIF}
    end;
  end;
  Result := NOERROR;
end;

// IDockingWindow

function TzCustomBandObject.ShowDW(fShow: BOOL): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.ShowDW()');
  zTraceLog('  fShow=' + BooleanAsString(fShow));
  {$ENDIF}
  Result := NOERROR;
  if not Assigned(FBandForm) then
    Exit;
  with FBandForm do
    if fShow then
      Show
    else
      Hide;
end;

function TzCustomBandObject.CloseDW(dwReserved: DWORD): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.CloseDW()');
  {$ENDIF}
  Result := NOERROR;
  if not Assigned(FBandForm) then
    Exit;
  ShowDW(False);
  FBandForm.Close;
end;

function TzCustomBandObject.ResizeBorderDW(var prcBorder: TRect;
  punkToolbarSite: IUnknown; fReserved: BOOL): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.ResizeBorderDW()');
  {$ENDIF}
  // Never called for band objects.
  Result := E_NOTIMPL;
end;

// IOleWindow

function TzCustomBandObject.GetWindow(out wnd: HWnd): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetWindow()');
  {$ENDIF}
  if Assigned(FBandForm) then
    wnd := FBandForm.Handle
  else
    wnd := 0;
  {$IFDEF Debug}
  zTraceLog('  wnd=' + Format('0x%x', [wnd]));
  {$ENDIF}
  Result := S_OK;
end;

function TzCustomBandObject.ContextSensitiveHelp(
  fEnterMode: BOOL): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.ContextSensitiveHelp()');
  {$ENDIF}
  Result := E_NOTIMPL;
end;

// IObjectWithSite

function TzCustomBandObject.SetSite(const pUnkSite: IUnknown): HResult;
var
  OleWindow: IOleWindow;
  ParentWnd: HWnd;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.SetSite()');
  zTraceLog('  pUnkSite=' + iif(Assigned(pUnkSite), 'not nil', 'nil'));
  {$ENDIF}
  if Assigned(FSite) then
    FSite := nil; // implicit Release
  if Assigned(FOleCommandTarget) then
    FOleCommandTarget := nil; // implicit Release
  if Assigned(pUnkSite) then
  begin
    if not Assigned(FBandForm) then
    begin
      if pUnkSite.QueryInterface(IOleWindow, OleWindow) <> S_OK then
      begin
        Result := E_FAIL;
        Exit;
      end;
      try
        OleWindow.GetWindow(ParentWnd);
      finally
        OleWindow := nil;
      end;
      {$IFDEF Debug}
      zTraceLog('  ParentWnd=' + Format('0x%x', [ParentWnd]));
      {$ENDIF}
      if ParentWnd = 0 then
      begin
        Result := E_FAIL;
        Exit;
      end;
      FBandForm := CreateBandForm(ParentWnd);
    end;
    if pUnkSite.QueryInterface(IInputObjectSite, FSite) <> S_OK then // implicit FSite.AddRef;
    begin
      Result := E_FAIL;
      Exit;
    end;
    {$IFDEF Debug}
    zTraceLog('  FSite assigned.');
    {$ENDIF}
    if FSite.QueryInterface(IOleCommandTarget, FOleCommandTarget) <> S_OK then
      FOleCommandTarget := nil;
  end;
  Result := S_OK;
end;

function TzCustomBandObject.GetSite(const riid: TIID;
  out site: IUnknown): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetSite()');
  zTraceLog('  riid=' + GUIDToString(riid));
  {$ENDIF}
  if not Assigned(FSite) then
  begin
    site := nil;
    Result := E_FAIL;
    Exit;
  end;
  Result := FSite.QueryInterface(riid, site);
  {$IFDEF Debug}
  zTraceLog('  Result=' + IntToStr(Result));
  {$ENDIF}
end;

// IPersistStream

function TzCustomBandObject.IsDirty: HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.IsDirty()');
  {$ENDIF}
  Result := S_FALSE;
end;

function TzCustomBandObject.Load(const stm: IStream): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.Load()');
  {$ENDIF}
  Result := S_OK;
end;

function TzCustomBandObject.Save(const stm: IStream;
  fClearDirty: BOOL): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.Save()');
  {$ENDIF}
  Result := S_OK;
end;

function TzCustomBandObject.GetSizeMax(out cbSize: Largeint): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetSizeMax()');
  {$ENDIF}
  cbSize := 0;
  Result := S_OK;
end;

// IPersist

function TzCustomBandObject.GetClassID(out classID: TCLSID): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetClassID()');
  {$ENDIF}
  ClassID := Factory.ClassID;
  {$IFDEF Debug}
  zTraceLog('  ClassID=' + GUIDToString(ClassID));
  {$ENDIF}
  Result := S_OK;
end;

// IInputObject

function TzCustomBandObject.UIActivateIO(fActivate: BOOL;
  var lpMsg: TMsg): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.UIActivateIO()');
  zTraceLog('  fActivate=' + BooleanAsString(fActivate));
  {$ENDIF}
  Result := S_OK;
  if not Assigned(FBandForm) then
    Exit;
  if fActivate then
    FBandForm.SetFocus;
end;

function TzCustomBandObject.HasFocusIO: HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.HasFocusIO()');
  {$ENDIF}
  Result := iif(Assigned(FBandForm) and FBandForm.Focused,
    S_OK, S_FALSE);
  {$IFDEF Debug}
  zTraceLog('  Result=' + IntToStr(Result));
  {$ENDIF}
end;

function TzCustomBandObject.TranslateAcceleratorIO(
  var lpMsg: TMsg): HResult;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.TranslateAcceleratorIO()');
  {$ENDIF}
  Result := S_FALSE;
end;

{ TzContextMenuBandObject }

// IContextMenu

function GetContextMenuCaption(const MenuItem: TMenuItem): string;
begin
  Result := MenuItem.Caption;
  if MenuItem.Count > 0 then
    Exit;
  if (MenuItem.ShortCut <> scNone) and
    ((MenuItem.Parent = nil) or (MenuItem.Parent.Parent <> nil) or not (MenuItem.Parent.Owner is TMainMenu)) then
    Result := Result + #9 + ShortCutToText(MenuItem.ShortCut);
end;

function AddContextMenuItem(const MenuItem: TMenuItem; const hMenu: HMENU;
  const idCmdFirst: UINT; ARightToLeft: Boolean): Boolean;
const
  RightToLeftMenuFlag = MFT_RIGHTORDER or MFT_RIGHTJUSTIFY;
  IBreaks: array[TMenuBreak] of DWORD = (MFT_STRING, MFT_MENUBREAK, MFT_MENUBARBREAK);
  IChecks: array[Boolean] of DWORD = (MFS_UNCHECKED, MFS_CHECKED);
  IDefaults: array[Boolean] of DWORD = (0, MFS_DEFAULT);
  IEnables: array[Boolean] of DWORD = (MFS_DISABLED or MFS_GRAYED, MFS_ENABLED);
  IRadios: array[Boolean] of DWORD = (MFT_STRING, MFT_RADIOCHECK);
  ISeparators: array[Boolean] of DWORD = (MFT_STRING, MFT_SEPARATOR);
  IRTL: array[Boolean] of DWORD = (0, RightToLeftMenuFlag);
  IOwnerDraw: array[Boolean] of DWORD = (MFT_STRING, MFT_OWNERDRAW);
var
  MenuItemInfo: TMenuItemInfo;
  IsOwnerDraw: Boolean;
  ParentMenu: TMenu;
begin
  Result := False;
  if not MenuItem.Visible then
    Exit;
  MenuItemInfo.cbSize := SizeOf(TMenuItemInfo);
  MenuItemInfo.fMask := MIIM_CHECKMARKS or MIIM_DATA or MIIM_ID or
    MIIM_STATE or MIIM_SUBMENU or MIIM_TYPE;
  ParentMenu := MenuItem.GetParentMenu;
  IsOwnerDraw := Assigned(ParentMenu) and
    (ParentMenu.OwnerDraw or (MenuItem.GetImageList <> nil)) or
    Assigned(MenuItem.Bitmap) and not MenuItem.Bitmap.Empty;
  MenuItemInfo.fType := IRadios[MenuItem.RadioItem] or
    IBreaks[MenuItem.Break] or
    ISeparators[MenuItem.Caption = cLineCaption] or
    IRTL[ARightToLeft] or
    IOwnerDraw[IsOwnerDraw];
  MenuItemInfo.fState := IChecks[MenuItem.Checked] or
    IEnables[MenuItem.Enabled] or
    IDefaults[MenuItem.Default];
  MenuItemInfo.wID := MenuItem.Command + idCmdFirst;
  MenuItemInfo.hbmpChecked := 0;
  MenuItemInfo.hbmpUnchecked := 0;
  MenuItemInfo.dwTypeData := PChar(GetContextMenuCaption(MenuItem));
  if MenuItem.Count > 0 then
    MenuItemInfo.hSubMenu := MenuItem.Handle
  else
    MenuItemInfo.hSubMenu := 0;
  Result := InsertMenuItem(hMenu, DWORD(-1), True, MenuItemInfo);
  {$IFDEF Debug}
  if not Result then
    Exit;
  zTraceLog('  Menu item added, MenuItem.Command=' + IntToStr(MenuItem.Command));
  zTraceLog('    Count=' + IntToStr(MenuItem.Count));
  zTraceLog('    Handle=' + Format('0x%x', [MenuItemInfo.hSubMenu]));
  {$ENDIF}
end;

function TzContextMenuBandObject.QueryContextMenu(hMenu: HMENU; indexMenu,
  idCmdFirst, idCmdLast, uFlags: UINT): HResult;
var
  i: Integer;
  idCmd: UINT;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.QueryContextMenu()');
  zTraceLog('  indexMenu: ' + IntToStr(indexMenu));
  zTraceLog('  idCmdFirst: ' + IntToStr(idCmdFirst));
  zTraceLog('  idCmdLast: ' + IntToStr(idCmdLast));
  zTraceLog('  uFlags: ' + Format('0x%x', [uFlags]));
  {$ENDIF}
  if (CMF_DEFAULTONLY and uFlags) <> 0 then
  begin
    Result := MakeHResult(SEVERITY_SUCCESS, 0, 0);
    Exit;
  end;
  Result := MakeHResult(SEVERITY_SUCCESS, 0, 1);
  if not Assigned(FBandForm) then
    Exit;
  with FBandForm do
  begin
    if not Assigned(BandContextMenu) then
      Exit;
    idCmd := idCmdFirst;
    with BandContextMenu do
    begin
      for i := 0 to Items.Count - 1 do
      begin
        if AddContextMenuItem(Items[i], hMenu, idCmdFirst,
          SysLocale.MiddleEast and (BiDiMode <> bdLeftToRight)) then
          idCmd := Max(idCmd, Items[i].Command);
      end;
    end;
  end;
  Result := MakeHResult(SEVERITY_SUCCESS, 0, idCmd + 1);
end;

function TzContextMenuBandObject.InvokeCommand(
  var lpici: TCMInvokeCommandInfo): HResult;
var
  idCmd: UINT;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.InvokeCommand()');
  {$ENDIF}
  idCmd := LoWord(lpici.lpVerb);
  {$IFDEF Debug}
  zTraceLog('  idCmd=' + IntToStr(idCmd));
  {$ENDIF}
  Result := E_INVALIDARG;
  if not Assigned(FBandForm) then
    Exit;
  with FBandForm do
  begin
    if not Assigned(BandContextMenu) then
      Exit;
    if BandContextMenu.DispatchCommand(idCmd) then
      Result := NOERROR;
  end;
end;

function TzContextMenuBandObject.GetCommandString(idCmd, uType: UINT;
  pwReserved: PUINT; pszName: LPSTR; cchMax: UINT): HResult;
var
  MenuItem: TMenuItem;
begin
  {$IFDEF Debug}
  zTraceLog(ClassName + '.GetCommandString()');
  zTraceLog('  idCmd=' + IntToStr(idCmd));
  zTraceLog('  uType=' + Format('0x%x', [uType]));
  {$ENDIF}
  Result := E_INVALIDARG;
  if not Assigned(FBandForm) then
    Exit;
  with FBandForm do
  begin
    if not Assigned(BandContextMenu) then
      Exit;
    case uType of
      GCS_HELPTEXT:
        begin
          MenuItem := BandContextMenu.FindItem(idCmd, fkCommand);
          if MenuItem = nil then
            Exit;
          StrCopy(pszName, PChar(MenuItem.Hint));
        end;
      GCS_VERB:
        begin
          MenuItem := BandContextMenu.FindItem(idCmd, fkCommand);
          if MenuItem = nil then
            Exit;
          StrCopy(pszName, PChar(GetContextMenuCaption(MenuItem)));
        end;
      GCS_VALIDATE:
        Result := NOERROR;
    end;
  end;
end;

end.

