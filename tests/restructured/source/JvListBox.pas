{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvListbox2.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This unit is a merging of the original TJvListBox, JvListBox2, TJvExListBox.
Merging done 2002-06-15 by Peter Thornqvist [peter3@peter3.com]

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Petr Vones (petr.v@mujmail.cz)
Peter Below <100113.1101@compuserve.com>

MERGE NOTES:
  * The Alignment property might mess things up depending on other property settings
  * not very extensively tested
  * TJvListBox in JvCtrls inherits from TJvCustomListbox in this unit.
    Maybe TJvListBox should be moved here instead (or this code into JvCtrls)?
  * TJvPlaylist now inherits from JvListBox

Last Modified: 2002-06-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvListBox;
{$I JVCL.INC}
{$IFDEF COMPILER6_UP}
{$WARN UNIT_PLATFORM OFF}
{$ENDIF}
{$IFDEF LINUX}
This unit is only supported on Windows!
{$ENDIF}



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, StdCtrls, Controls, Forms,
  JvItemsSearchs, JVCLVer;

type
  TJvListBoxDataEvent = procedure(Sender: TWinControl; Index: Integer; var Text: string) of object;
  TJvListboxChange = procedure(Sender: TObject; Item: string) of object;
  TJvScrollEvent = procedure(sender: TObject; const Msg: TWMScroll; var dontScroll: Boolean) of object;

  TJvCustomListBox = class(TListbox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    fAlignment: TAlignment;
    FHotTrack: Boolean;
    FColor: TColor;
    FSaved: TColor;
    FOver: Boolean;
    FMaxWidth: Integer;
    FItemSearchs: TJvItemsSearchs;
    FCount: Integer;
    FScrollBars: TScrollStyle;
    FSorted: Boolean;
    FHorzExtent: Integer;
    FOwnerData: Boolean;
    FOnGetText: TJvListBoxDataEvent;
    FOldStyle: TListBoxStyle;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnSelectCancel: TNotifyEvent;
    FOnDelete: TJvListboxChange;
    FOnAdd: TJvListboxChange;
    FOnChange: TNotifyEvent;
    FOnHScroll: TJvScrollEvent;
    FOnVScroll: TJvScrollEvent;
    procedure SetScrollBars(const Value: TScrollStyle);
    procedure SetHorzExtent(const Value: Integer);
    procedure SetOwnerData(const Value: Boolean);
    procedure SetCount(const Value: Integer);
    procedure SetItems(const Value: TStrings);
    procedure SetSorted(const Value: Boolean);
    procedure SetHotTrack(const Value: Boolean);
    function GetItems: TStrings;
    procedure SetAlignment(const Value: TAlignment);
  protected
    
    procedure LBAddString(var Msg: TMessage); message LB_ADDSTRING;
    procedure LBDeleteString(var Msg: TMessage); message LB_DELETEstring;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SelectCancel(var Msg: TMessage); message LBN_SELCANCEL;
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure Changed; virtual;
    { Protected declarations }
    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;

    property Count: Integer read FCount write SetCount;
    property HorzExtent: Integer read FHorzExtent write SetHorzExtent default 0;
    property Items: TStrings read GetItems write SetItems;
    property OwnerData: Boolean read FOwnerData write SetOwnerData default False;
    property ScrollBars: TScrollStyle read FScrollBars write SetScrollBars default ssBoth;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property OnGetText: TJvListBoxDataEvent read FOnGetText write FOnGetText;
    property Alignment: TAlignment read fAlignment write SetAlignment
      default taLeftJustify;
    property HotTrack: Boolean read FHotTrack write SetHotTrack default False;
    property HintColor: TColor read FColor write FColor default clInfoBk;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnSelectCancel: TNotifyEvent read FOnSelectCancel write FOnSelectCancel;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDeleteString: TJvListboxChange read FOnDelete write FOnDelete;
    property OnAddString: TJvListboxChange read FOnAdd write FOnAdd;
    property OnVerticalScroll: TJvScrollEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TJvScrollEvent read FOnHScroll write FOnHScroll;
  public
    procedure WndProc(var Msg: TMessage); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure UpdateCount;
    procedure UpdateHorzExtent;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
    procedure SelectAll; {$IFDEF COMPILER6_UP}override;{$ENDIF}
    procedure UnselectAll;
    procedure InvertSelection;
    procedure MoveSelectedUp; virtual;
    procedure MoveSelectedDown; virtual;
    procedure DeleteSelected; {$IFDEF COMPILER6_UP}override;{$ENDIF}
    procedure DeleteAllButSelected;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

resourcestring
  RsLBVirtualCantBeSorted = 'ListBox doesn''t allow sorting in virtual mode';

implementation
uses
  JclBase;

{**************************************************}

constructor TJvCustomListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxWidth := 0;
  FHotTrack := False;
  FColor := clInfoBk;
  FOver := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FItemSearchs := TJvItemsSearchs.Create;
end;

{**************************************************}

destructor TJvCustomListBox.Destroy;
begin
  FItemSearchs.Free;
  inherited;
end;

{**************************************************}

procedure TJvCustomListBox.CreateParams(var Params: TCreateParams);
const
  ScrollBar: array[TScrollStyle] of DWORD = (0, WS_HSCROLL, WS_VSCROLL,
    WS_HSCROLL or WS_VSCROLL);
  Sorted: array[Boolean] of DWORD = (0, LBS_SORT);
begin
  inherited;
  with Params do
  begin
    Style := Style and not (WS_HSCROLL or WS_VSCROLL) or ScrollBar[FScrollBars] or
      Sorted[FSorted];
    if FOwnerData then
    begin
      Style := Style and not (LBS_SORT or LBS_HASSTRINGS) or LBS_NODATA;
      if Self.Style = lbStandard then
        Style := Style or LBS_OWNERDRAWFIXED;
    end;
  end;
end;

procedure TJvCustomListBox.SetHotTrack(const Value: Boolean);
begin
  if FhotTrack <> Value then
  begin
    FHotTrack := Value;
    Ctl3d := not FHotTrack;
  end;
end;

procedure TJvCustomListBox.LBAddString(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnAdd) then
    FOnAdd(Self, StrPas(PChar(Msg.lParam)));
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomListBox.LBDeleteString(var Msg: TMessage);
begin
  if Assigned(FOnDelete) then
    FOnDelete(Self, Items.Strings[LongInt(Msg.wParam)]);
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvCustomListBox.WndProc(var Msg: TMessage);
var
  Text: string;
  ItemWidth: Word;
begin
  if FOwnerData then
  begin
    with Msg do
      case Msg of
        LB_GETTEXT:
          if Assigned(FOnGetText) and (wParam > -1) and (wParam < Count) then
          begin
            Text := '';
            OnGetText(Self, WParam, Text);
            Result := Length(Text);
            StrPCopy(PChar(LParam), Text);
          end
          else
            Result := LB_ERR;
        LB_GETTEXTLEN:
          if Assigned(FOnGetText) and (wParam > -1) and (wParam < Count) then
          begin
            Text := '';
            OnGetText(Self, WParam, Text);
            Result := Length(Text);
          end
          else
            Result := LB_ERR;
        LB_ADDSTRING, LB_INSERTSTRING, LB_SETITEMDATA:
          Result := LB_ERR;
      else
        inherited;
      end;
    Exit;
  end
  else
    case Msg.Msg of
      LB_ADDSTRING, LB_INSERTSTRING:
        begin
          ItemWidth := Canvas.TextWidth(StrPas(PChar(Msg.lParam)) + ' ');
          if FMaxWidth < ItemWidth then
            FMaxWidth := ItemWidth;
          SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
        end;
      LB_DELETESTRING:
        begin
          ItemWidth := Canvas.TextWidth(Items[Msg.wParam] + ' ');
          if ItemWidth = FMaxWidth then
          begin
            inherited;
            UpdateHorzExtent;
            Exit;
          end;
        end;
      LB_RESETCONTENT:
        SendMessage(Handle, LB_SETHORIZONTALEXTENT, 0, 0);
      WM_SETFONT:
        begin
          inherited;
          Canvas.Font.Assign(Font);
          UpdateHorzExtent;
          Exit;
        end;
    end;
  inherited;
end;

procedure TJvCustomListBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvCustomListBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvCustomListBox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FHotTrack then
      Ctl3d := True;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvCustomListBox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FHotTrack then
      Ctl3d := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

function TJvCustomListBox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCustomListBox.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCustomListBox.WMHScroll(var Msg: TWMHScroll);
var
  dontScroll: Boolean;
begin
  if Assigned(FOnHScroll) then
  begin
    dontScroll := False;
    FOnHScroll(self, msg, dontScroll);
    if dontScroll then
      Exit;
  end;
  inherited;
end;

{**************************************************}

procedure TJvCustomListBox.WMVScroll(var Msg: TWMVScroll);
var
  dontScroll: Boolean;
begin
  if Assigned(FOnVScroll) then
  begin
    dontScroll := False;
    FOnVScroll(self, msg, dontScroll);
    if dontScroll then
      Exit;
  end;
  inherited;
end;

{**************************************************}

procedure TJvCustomListBox.SelectCancel(var Msg: TMessage);
begin
  if Assigned(FOnSelectCancel) then
    FOnSelectCancel(Self);
end;

{**************************************************}

function TJvCustomListBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCustomListBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
  Changed;
end;

{**************************************************}

procedure TJvCustomListBox.SelectAll;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for i := 0 to Items.Count - 1 do
      Selected[i] := True;
    Items.EndUpdate;
  end;
end;
{**************************************************}

procedure TJvCustomListBox.UnselectAll;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for i := 0 to Items.Count - 1 do
      Selected[i] := False;
    Items.EndUpdate;
  end;
end;
{**************************************************}

procedure TJvCustomListBox.InvertSelection;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    Items.BeginUpdate;
    for i := 0 to Items.Count - 1 do
      Selected[i] := not (Selected[i]);
    Items.EndUpdate;
  end;
end;
{**************************************************}

procedure TJvCustomListBox.DeleteSelected;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then
        Items.Delete(i);
  end
  else if ItemIndex <> -1 then
  begin
    i := ItemIndex;
    Items.Delete(i);
    if i > 0 then
      Dec(i);
    if Items.Count > 0 then
      ItemIndex := i;
  end;
  Changed;
end;

{**************************************************}

procedure TJvCustomListBox.MoveSelectedDown;
var
  i: Integer;
begin
  if not MultiSelect then
  begin
    if (ItemIndex <> -1) and (ItemIndex < Items.Count - 1) then
    begin
      Items.Exchange(ItemIndex, ItemIndex + 1);
      ItemIndex := ItemIndex + 1;
    end;
    Exit;
  end;
  if (Items.Count > 0) and (SelCount > 0) and (not (Selected[Items.Count - 1])) then
  begin
    i := Items.Count - 2;
    while i >= 0 do
    begin
      if Selected[i] then
      begin
        Items.Exchange(i, i + 1);
        Selected[i + 1] := True;
      end;
      Dec(i);
    end;
  end;
end;

{**************************************************}

procedure TJvCustomListBox.MoveSelectedUp;
var
  i: Integer;
begin
  if not MultiSelect then
  begin
    if ItemIndex > 1 then
    begin
      Items.Exchange(ItemIndex, ItemIndex - 1);
      ItemIndex := ItemIndex - 1;
    end;
    Exit;
  end;
  if (Items.Count > 0) and (SelCount > 0) and not Selected[0] then
  begin
    i := 1;
    while i < Items.Count do
    begin
      if Selected[i] then
      begin
        Items.Exchange(i, i - 1);
        Selected[i - 1] := True;
      end;
      Inc(i);
    end;
  end;
end;

{**************************************************}

procedure TJvCustomListBox.DeleteAllButSelected;
var
  i: Integer;
begin
  if not MultiSelect then
    Exit;
  i := 0;
  while i < Items.Count do
    if not (Selected[i]) then
      Items.Delete(i)
    else
      Inc(i);
  Changed;
end;

procedure TJvCustomListBox.Changed;
begin
  // (rom) TODO?
end;

procedure TJvCustomListBox.CreateWnd;
begin
  inherited;
  UpdateHorzExtent;
  UpdateCount;
end;

procedure TJvCustomListBox.SetCount(const Value: Integer);
begin
  if FCount <> Value then
  begin
    FCount := Value;
    UpdateCount;
  end;
end;

procedure TJvCustomListBox.SetHorzExtent(const Value: Integer);
begin
  if FHorzExtent <> Value then
  begin
    FHorzExtent := Value;
    UpdateHorzExtent;
  end;
end;

procedure TJvCustomListBox.SetItems(const Value: TStrings);
begin
  if FOwnerData then
    inherited Items.Clear
  else
    inherited Items := Value;
end;

procedure TJvCustomListBox.SetOwnerData(const Value: Boolean);
begin
  if FOwnerData <> Value then
  begin
    FOwnerData := Value;
    Items.Clear;
    FSorted := False;
    RecreateWnd;
  end;
end;

procedure TJvCustomListBox.SetScrollBars(const Value: TScrollStyle);
begin
  if FScrollBars <> Value then
  begin
    FScrollBars := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomListBox.SetSorted(const Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if FOwnerData and Value then
      raise EJclError.CreateResRec(@RsLBVirtualCantBeSorted);
    FSorted := Value;
    RecreateWnd;
  end;
end;

procedure TJvCustomListBox.UpdateCount;
begin
  if FOwnerData and HandleAllocated then
    Perform(LB_SETCOUNT, FCount, 0);
end;

procedure TJvCustomListBox.UpdateHorzExtent;
begin
  if HandleAllocated and (FScrollBars in [ssHorizontal, ssBoth]) then
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, FHorzExtent, 0);
end;

function TJvCustomListBox.GetItems: TStrings;
begin
  Result := inherited Items;
end;

procedure TJvCustomListBox.SetAlignment(const Value: TAlignment);
begin
  if fAlignment <> Value then
  begin
    FAlignment := Value;
    if FAlignment <> taLeftJustify then
    begin
      FOldStyle := Style;
      Style := lbOwnerDrawFixed;
    end
    else
      Style := FoldStyle;
    Invalidate;
  end;
end;

procedure TJvCustomListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
const
  alignflags: array[TAlignment] of DWORD =
  (DT_LEFT, DT_RIGHT, DT_CENTER);
var
  //  flags: DWORD;
  S: string;
begin
  if Assigned(OnDrawItem) or (Alignment = taLeftJustify) then
    inherited
  else
  begin
    Canvas.FillRect(Rect);
    if index >= 0 then
    begin
      S := Items[index];
      if Length(S) > 0 then
        DrawText(Canvas.handle, PChar(S), Length(S), Rect,
          DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX or
          alignflags[FAlignment]);
    end;
  end;
end;

procedure TJvCustomListBox.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  if Alignment <> taLeftJustify then
    Repaint;
end;

end.

