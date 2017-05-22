{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: kvBandForms.PAS, released on 2001-07-10.

The Initial Developer of the Original Code is Chiang Seng Chang <cs att ctzen dott com>
Portions created by Chiang Seng Chang are Copyright (C) 2001 Chiang Seng Chang.
All Rights Reserved.

Contributor(s): ______________________________________.

You may retrieve the latest version of this file at the Project JEDI home page,
located at http://www.delphi-jedi.org

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvBandForms;

{$I jvcl.inc}
{$I windowsonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, ComObj, Classes, Forms, Controls, Menus, JvComponent;

type
  {:Band object mode flag.
  @enum bmfVariableHeight  Height of the band object can be changed.
  @enum bmfDebossed        Band object displayed with a sunken appearance.
  @enum bmfBkColor         Band object displayed with the background color specified in the band form's Color property.
  @seeAlso <see type="TJvBandModeFlags">
  @seeAlso <see class="TJvBandForm" property="BandModeFlags">
  }
  TJvBandModeFlag = (bmfVariableHeight, bmfDebossed, bmfBkColor);

  {:Set of band object mode flags.
  @seeAlso <see class="TJvBandForm" property="BandModeFlags">
  }
  TJvBandModeFlags = set of TJvBandModeFlag;

  {:Event type for band form's OnBandGetXXXX events.
  @seeAlso <see class="TJvBandForm" event="OnBandGetMinSize">
  @seeAlso <see class="TJvBandForm" event="OnBandGetMaxSize">
  @seeAlso <see class="TJvBandForm" event="OnBandGetIntegral">
  @seeAlso <see class="TJvBandForm" event="OnBandGetActualSize">
  }
  TzGetPointLEvent = function(Sender: TObject): TPointL of object;

  {:Base class for band forms.
  @cat JvBandFormComponents
  }
  TJvBandForm = class(TJvForm)
  private
    FBandObject: TComObject;
    FBandModeFlags: TJvBandModeFlags;
    FBandContextMenu: TPopupMenu;
    FBandIntegralX: Word;
    FBandIntegralY: Word;
    FOnGetMinSize: TzGetPointLEvent;
    FOnGetMaxSize: TzGetPointLEvent;
    FOnGetIntegral: TzGetPointLEvent;
    FOnGetActualSize: TzGetPointLEvent;
    function GetMinSize: TPointL;
    function GetMaxSize: TPointL;
    function GetIntegral: TPointL;
    function GetActualSize: TPointL;
    procedure SetContextMenu(const Value: TPopupMenu);
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property _BandObject: TComObject read FBandObject;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {:Band form constructor.
    Use this constructor to create a band form.<br>
    The band object wizard generates code which calls this constructor
    automatically.<br>
    When the band form is created, its ClientWidth and ClientHeight is
    set to the size of ParentWindow.<br><br>
    Note.  This is a constructor, Time2Help (the help file generator)
    mislabelled this as a procedure !
    <br>
    @param ParentWindow   The parent window of the band form.
    @param BandObject     The band object associated with the band form.
    }
    constructor CreateBandForm(const ParentWindow: THandle; const BandObject: TComObject);
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    {:Returns the minimum size of the band form in a TPointL structure.
    Minimum size is obtained from in the properties Constraints.MinWidth and
    Constraints.MinHeight.<br>
    Use OnBandGetMinSize event to override this.
    @seeAlso <see property="Constraints">
    @seeAlso <see event="OnBandGetMinSize">
    @seeAlso <see class="TzCustomBandObject" method="GetBandInfo">
    }
    property BandMinSize: TPointL read GetMinSize;
    {:Returns the maximum size of the band form in a TPointL structure.
    Maximum size is obtained from the properties Constraints.MaxWidth and
    Constraints.MaxHeight.<br>
    Use OnBandGetMaxSize event to override this.
    @seeAlso <see property="Constraints">
    @seeAlso <see event="OnBandGetMaxSize">
    @seeAlso <see class="TzCustomBandObject" method="GetBandInfo">
    }
    property BandMaxSize: TPointL read GetMaxSize;
    {:Returns the sizing step of the band form in a TPointL structure.
    Sizing step is obtained from the properties BandIntegralX and BandIntegralY.<br>
    Use OnBandGetIntegral event to override this.
    @seeAlso <see property="BandIntegralX">
    @seeAlso <see property="BandIntegralY">
    @seeAlso <see event="OnBandGetIntegral">
    @seeAlso <see class="TzCustomBandObject" method="GetBandInfo">
    }
    property BandIntegral: TPointL read GetIntegral;
    {:Returns the actual size of the band form in a TPointL structure.
    Actual size is obtained from the properties ClientWidth and ClientHeight.<br>
    Use OnBandGetActualSize event to override this.
    @seeAlso <see property="ClientWidth">
    @seeAlso <see property="ClientHeight">
    @seeAlso <see event="OnBandGetActualSize">
    @seeAlso <see class="TzCustomBandObject" method="GetBandInfo">
    }
    property BandActualSize: TPointL read GetActualSize;
    // Below are from TForm (Probably more properties should be hidden)
    //  procedure ArrangeIcons;
    //  procedure Cascade;
    //  procedure Next;
    //  procedure Previous;
    //  procedure Tile;
    //  property ActiveMDIChild;
    //  property ClientHandle;
    property DockManager;
    //  property MDIChildCount;
    //  property MDIChildren;
    //  property TileMode;
  published
    {:Specifies the band object's mode flags.
    Used by IDeskBand::GetBandInfo.
    @seeAlso <see class="TzCustomBandObject" method="GetBandInfo">
    }
    property BandModeFlags: TJvBandModeFlags read FBandModeFlags write FBandModeFlags default [bmfVariableHeight];
    {:Specifies the band object's X sizing step.
    @seeAlso <see property="BandIntegralY">
    @seeAlso <see property="BandIntegral">
    }
    property BandIntegralX: Word read FBandIntegralX write FBandIntegralX default 1;
    {:Specifies the band object's Y sizing step.
    @seeAlso <see property="BandIntegralX">
    @seeAlso <see property="BandIntegral">
    }
    property BandIntegralY: Word read FBandIntegralY write FBandIntegralY default 1;
    {:Specifies the band object's context menu.
    To integrate menuitems into the band window's context menu,
    drop a popup menu onto the band form and set this
    property to the popup menu.
    Note. Tool bands do not support context menu.
    @seeAlso <see class="TzContextMenuBandObject" method="QueryContextMenu">
    @seeAlso <see class="TzContextMenuBandObject" method="GetCommandString">
    @seeAlso <see class="TzContextMenuBandObject" method="InvokeCommand">
    }
    property BandContextMenu: TPopupMenu read FBandContextMenu write SetContextMenu;
    {:Occurs when the band form is queried for it's minimum size.
    @seeAlso <see property="BandMinSize">
    }
    property OnBandGetMinSize: TzGetPointLEvent read FOnGetMinSize write FOnGetMinSize;
    {:Occurs when the band form is queried for it's maximum size.
    @seeAlso <see property="BandMaxSize">
    }
    property OnBandGetMaxSize: TzGetPointLEvent read FOnGetMaxSize write FOnGetMaxSize;
    {:Occurs when the band form is queried for it's sizing steps.
    @seeAlso <see property="BandIntegral">
    }
    property OnBandGetIntegral: TzGetPointLEvent read FOnGetIntegral write FOnGetIntegral;
    {:Occurs when the band form is queried for it's actual size.
    @seeAlso <see property="BandActualSize">
    }
    property OnBandGetActualSize: TzGetPointLEvent read FOnGetActualSize write FOnGetActualSize;
    // Below are from TForm (Probably more properties should be hidden)
    {: The band object's title.
    @seeAlso <see class="TzCustomBandObject" method="GetBandInfo">
    }
    property Caption;
    {: The band object's actual height.
    @seeAlso <see property="ClientWidth">
    @seeAlso <see property="BandActualSize">
    }
    property ClientHeight;
    {: The band object's actual width.
    @seeAlso <see property="ClientHeight">
    @seeAlso <see property="BandActualSize">
    }
    property ClientWidth;
    {: The band object's background color.
    @seeAlso <see class="TzCustomBandObject" method="GetBandInfo">
    }
    property Color;
    {: The band object's minimum and maximum sizes.
    @seeAlso <see property="BandMinSize">
    @seeAlso <see property="BandMaxSize">
    }
    property Constraints;
    property Action;
    property ActiveControl;
    property Align;
    property Anchors;
    property AutoScroll;
    property AutoSize;
    property BiDiMode;
    //  property BorderIcons;
    //  property BorderStyle;
    //  property BorderWidth;
    property UseDockManager;
    property DefaultMonitor;
    property DockSite;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentFont default False;
    property Font;
    //  property FormStyle;
    property Height;
    property HelpFile;
    property HorzScrollBar;
    property Icon;
    property KeyPreview;
    property Menu;
    property OldCreateOrder;
    property ObjectMenuItem;
    property ParentBiDiMode;
    property PixelsPerInch;
    property PopupMenu;
    //  property Position;
    property PrintScale;
    property Scaled;
    property ShowHint;
    property VertScrollBar;
    //  property Visible;
    property Width;
    property WindowState;
    property WindowMenu;
    property OnActivate;
    property OnCanResize;
    property OnClick;
    property OnClose;
    //  property OnCloseQuery;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnCreate;
    property OnDblClick;
    property OnDestroy;
    property OnDeactivate;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnHide;
    property OnHelp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnPaint;
    property OnResize;
    property OnShortCut;
    property OnShow;
    property OnStartDock;
    property OnUnDock;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  SysUtils, JclIDEUtils, JvJCLUtils, JvJVCLUtils;

var
  GlobalBandFormMessageHook: HHook;
  GlobalBandForms: TList;

procedure InstallHook; forward;
procedure UninstallHook; forward;

//=== { TJvBandForm } ========================================================

procedure TJvBandForm.AfterConstruction;
begin
  inherited AfterConstruction;
  GlobalBandForms.Add(Self);
  if (GlobalBandForms.Count = 1) and not (csDesigning in ComponentState) then
    InstallHook;
end;

procedure TJvBandForm.BeforeDestruction;
begin
  GlobalBandForms.Remove(Self);
  if (GlobalBandForms.Count = 0) and not (csDesigning in ComponentState) then
    UninstallHook;
  inherited BeforeDestruction;
end;

constructor TJvBandForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBandModeFlags := [bmfVariableHeight];
  FBandIntegralX := 1;
  FBandIntegralY := 1;
  BorderIcons := [];
  BorderStyle := bsNone;
  BorderWidth := 0;
  FormStyle := fsNormal;
  Position := poDesigned;
  Visible := False;
  ParentFont := False;
end;

constructor TJvBandForm.CreateBandForm(const ParentWindow: THandle; const BandObject: TComObject);
var
  Rect: TRect;
begin
  CreateParented(ParentWindow); // Band form should be a child window.
  FBandObject := BandObject;
  Windows.GetClientRect(ParentWindow, Rect);
  ClientWidth := Rect.Right - Rect.Left + 1;
  ClientHeight := Rect.Bottom - Rect.Top + 1;
end;

constructor TJvBandForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited CreateNew(AOwner);
  FBandModeFlags := [bmfVariableHeight];
  FBandIntegralX := 1;
  FBandIntegralY := 1;
  BorderStyle := bsNone;
  Visible := False;
end;

destructor TJvBandForm.Destroy;
begin
  ControlStyle := ControlStyle + [csNoStdEvents];
  inherited Destroy;
end;

function TJvBandForm.GetMinSize: TPointL;
begin
  if Assigned(FOnGetMinSize) then
    Result := FOnGetMinSize(Self)
  else
    Result := PointL(Constraints.MinWidth, Constraints.MinHeight);
end;

function TJvBandForm.GetMaxSize: TPointL;
begin
  if Assigned(FOnGetMaxSize) then
    Result := FOnGetMaxSize(Self)
  else
    Result := PointL(iif(Constraints.MaxWidth = 0, -1, Constraints.MaxWidth),
      iif(Constraints.MaxHeight = 0, -1, Constraints.MaxHeight));
end;

function TJvBandForm.GetIntegral: TPointL;
begin
  if Assigned(FOnGetIntegral) then
    Result := FOnGetIntegral(Self)
  else
    Result := PointL(FBandIntegralX, FBandIntegralY);
end;

function TJvBandForm.GetActualSize: TPointL;
begin
  if Assigned(FOnGetActualSize) then
    Result := FOnGetActualSize(Self)
  else
    Result := PointL(ClientWidth, ClientHeight);
end;

procedure TJvBandForm.SetContextMenu(const Value: TPopupMenu);
begin
  ReplaceComponentReference(Self, Value, TComponent(FBandContextMenu));
end;

procedure TJvBandForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  case Operation of
    opRemove:
      if AComponent = FBandContextMenu then
        FBandContextMenu := nil;
  end;
end;

{$IFDEF RTL230_UP}
function MsgHookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
{$ELSE}
function MsgHookProc(nCode, wParam, lParam: Integer): Integer; stdcall;
{$ENDIF RTL230_UP}
var
  lOk: Boolean;
  I: Integer;
  Msg: PMsg;
begin
  if nCode >= 0 then
    try
      lOk := False;
      Msg := PMsg(Pointer(lParam));
      if (((Msg^.message = WM_KEYDOWN) or (Msg^.message = WM_KEYUP)) and
        ((Msg^.wParam = VK_BACK))) then
        lOk := True
      else
      if Msg^.message = WM_MOUSEMOVE then //Enable Flat effects!
        Application.HandleMessage;
      if lOk then
      begin
        for I := 0 to GlobalBandForms.Count - 1 do
          if IsDialogMessage(TJvBandForm(GlobalBandForms.Items[I]).Handle, Msg^) then
          begin
            Msg^.message := WM_NULL;
            Break;
          end;
      end;
    except
    end;
  Result := CallNextHookEx(GlobalBandFormMessageHook, nCode, wParam, lParam);
end;

procedure InstallHook;
var
  Installations: TJclBorRADToolInstallations;
  DelphiVersion: Integer;
  RunningInIDE: Boolean;
begin
  Installations := TJclBorRADToolInstallations.Create;
  try
    if CompilerVersion >= 21 then
      DelphiVersion := Trunc(CompilerVersion - 7)
    else if CompilerVersion = 18.5 then
      DelphiVersion := 11
    else
      DelphiVersion := Trunc(CompilerVersion - 8);

    RunningInIDE := SameText(ParamStr(0), Installations.DelphiInstallationFromVersion[DelphiVersion].IdeExeFileName);
  finally
    Installations.Free;
  end;

  // Only install hook if not in IDE so as not to introduce glitches in the IDE
  if not RunningInIDE then
    GlobalBandFormMessageHook := SetWindowsHookEx(WH_GETMESSAGE, MsgHookProc, HInstance, GetCurrentThreadID);
end;

procedure UninstallHook;
begin
  if GlobalBandFormMessageHook <> 0 then
  begin
    UnhookWindowsHookEx(GlobalBandFormMessageHook);
    GlobalBandFormMessageHook := 0;
  end;
end;

initialization
  {$IFDEF UNITVERSIONING}
  RegisterUnitVersion(HInstance, UnitVersioning);
  {$ENDIF UNITVERSIONING}
  GlobalBandForms := TList.Create;

finalization
  GlobalBandForms.Free;
  {$IFDEF UNITVERSIONING}
  UnregisterUnitVersion(HInstance);
  {$ENDIF UNITVERSIONING}

end.

