{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: jvBandForms.PAS, released on 2001-07-10.

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

unit jvBandForms;

interface

uses
  Classes, Forms, Controls, Windows, Messages, Menus,
  ComObj;

type
  {:Band object mode flag.
  @enum bmfVariableHeight  Height of the band object can be changed.
  @enum bmfDebossed        Band object displayed with a sunken appearance.
  @enum bmfBkColor         Band object displayed with the background color specified in the band form's Color property.
  @seeAlso <see type="TjvBandModeFlags">
  @seeAlso <see class="TjvBandForm" property="BandModeFlags">
  }
  TjvBandModeFlag = (bmfVariableHeight, bmfDebossed, bmfBkColor);

  {:Set of band object mode flags.
  @seeAlso <see class="TjvBandForm" property="BandModeFlags">
  }
  TjvBandModeFlags = set of TjvBandModeFlag;

  {:Event type for band form's OnBandGetXXXX events.
  @seeAlso <see class="TjvBandForm" event="OnBandGetMinSize">
  @seeAlso <see class="TjvBandForm" event="OnBandGetMaxSize">
  @seeAlso <see class="TjvBandForm" event="OnBandGetIntegral">
  @seeAlso <see class="TjvBandForm" event="OnBandGetActualSize">
  }
  TzGetPointLEvent = function(Sender: TObject): TPointL of object;

  {:Base class for band forms.
  @cat jvBandFormComponents
  }
  TjvBandForm = class(TCustomForm)
  private
    FBandObject: TComObject;
    FBandModeFlags: TjvBandModeFlags;
    FBandContextMenu: TPopupMenu;
    FBandIntegralX, FBandIntegralY: Word;
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
    {$IFNDEF T2H}
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    property _BandObject: TComObject read FBandObject;
    {$ENDIF}
  public
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
    constructor CreateBandForm(const ParentWindow: HWnd; const BandObject: TComObject);
    {$IFNDEF T2H}
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    {$ENDIF}
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
    {$IFNDEF T2H}
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
    {$ENDIF}
  published
    {:Specifies the band object's mode flags.
    Used by IDeskBand::GetBandInfo.
    @seeAlso <see class="TzCustomBandObject" method="GetBandInfo">
    }
    property BandModeFlags: TjvBandModeFlags read FBandModeFlags write FBandModeFlags default [bmfVariableHeight];
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
    {$IFNDEF T2H}
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
    property Ctl3D;
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
    {$ENDIF}
  end;

implementation

uses
  jvBandUtils, jvBandWindows;

{ TjvBandForm }

constructor TjvBandForm.CreateBandForm(const ParentWindow: HWnd; const BandObject: TComObject);
var
  Rect: TRect;
begin
  CreateParented(ParentWindow); // Band form should be a child window.
  FBandObject := BandObject;
  Windows.GetClientRect(ParentWindow, Rect);
  ClientWidth := Rect.Right - Rect.Left + 1;
  ClientHeight := Rect.Bottom - Rect.Top + 1;
end;

constructor TjvBandForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FBandModeFlags := [bmfVariableHeight];
  FBandIntegralX := 1;
  FBandIntegralY := 1;
  BorderStyle := bsNone;
  Visible := False;
end;

function TjvBandForm.GetMinSize: TPointL;
begin
  if Assigned(FOnGetMinSize) then
    Result := FOnGetMinSize(Self)
  else
    with Constraints do
      Result := PointL(MinWidth, MinHeight);
end;

function TjvBandForm.GetMaxSize: TPointL;
begin
  if Assigned(FOnGetMaxSize) then
    Result := FOnGetMaxSize(Self)
  else
    with Constraints do
      Result := PointL(iif(MaxWidth = 0, -1, MaxWidth),
        iif(MaxHeight = 0, -1, MaxHeight));
end;

function TjvBandForm.GetIntegral: TPointL;
begin
  if Assigned(FOnGetIntegral) then
    Result := FOnGetIntegral(Self)
  else
    Result := PointL(FBandIntegralX, FBandIntegralY);
end;

function TjvBandForm.GetActualSize: TPointL;
begin
  if Assigned(FOnGetActualSize) then
    Result := FOnGetActualSize(Self)
  else
    Result := PointL(ClientWidth, ClientHeight);
end;

procedure TjvBandForm.SetContextMenu(const Value: TPopupMenu);
begin
  FBandContextMenu := Value;
  if Value = nil then
    Exit;
  Value.FreeNotification(Self);
end;

procedure TjvBandForm.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  case Operation of
    opRemove:
      begin
        if AComponent = FBandContextMenu then
          FBandContextMenu := nil;
      end;
  end;
end;

// Below are from TForm
// Since FormStyle is always fsNormal, all codes are commented.

{
procedure TjvBandForm.ArrangeIcons;
begin
  if (FormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDIICONARRANGE, 0, 0);
end;

procedure TjvBandForm.Cascade;
begin
  if (FormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDICASCADE, 0, 0);
end;

procedure TjvBandForm.Next;
begin
  if (FormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDINEXT, 0, 0);
end;

procedure TjvBandForm.Previous;
begin
  if (FormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDINEXT, 0, 1);
end;

procedure TjvBandForm.Tile;
const
  TileParams: array[TTileMode] of Word = (MDITILE_HORIZONTAL, MDITILE_VERTICAL);
begin
  if (FormStyle = fsMDIForm) and (ClientHandle <> 0) then
    SendMessage(ClientHandle, WM_MDITILE, TileParams[TileMode], 0);
end;
}

end.

