{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCombobox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCombobox;



interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvMaxPixel, JvPropAutoSave, JvItemsSearchs, JVCLVer;

type
  TJvCustomCombobox = class(TCustomComboBox)
  private
    FKey: Word;
    FAutoComplete: Boolean;
    {$IFNDEF COMPILER6_UP}
    FLastTime: Cardinal;      // SPM - Ported backward from Delphi 7
    FFilter: String;          // SPM - ditto
    {$ENDIF}
    FSearching: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnRestored: TNotifyEvent;
    FOver: Boolean;
    FMaxPixel: TJvMaxPixel;
    FAutoSave: TJvAutoSave;
    FItemSearchs: TJvItemsSearchs;
    FAboutJVCL: TJVCLAboutInfo;
    procedure MaxPixelChanged(Sender: TObject);
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$IFNDEF COMPILER6_UP}
    procedure KeyPress(var Key: Char); override;  // SPM - Ported backward from D7
    {$ENDIF}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    {$IFNDEF COMPILER6_UP}
    function SelectItem(const AnItem: String): Boolean;  // SPM - Ported from D7
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
  protected
//    property SelStart;
//    property SelText;
//    property SelLength;
//    property ItemIndex;

    property AutoComplete: Boolean read FAutoComplete write FAutoComplete default True;
    property AutoSave: TJvAutoSave read FAutoSave write FAutoSave;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;

    property OnRestored: TNotifyEvent read FOnRestored write FOnRestored;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
  end;

  TJvComboBox = class(TJvCustomComboBox)
  published
//    property SelStart;
//    property SelText;
//    property SelLength;
    property HintColor;
    property MaxPixel;

    property AutoComplete default True;
    {$IFDEF COMPILER6_UP}
    property AutoDropDown default False;
    {$ENDIF}
    property AutoSave;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property Style; {Must be published before Items}
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    {$IFDEF COMPILER6_UP}
    property OnCloseUp;
    {$ENDIF}
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    {$IFDEF COMPILER6_UP}
    property OnSelect;
    {$ENDIF}
    property OnStartDock;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }

    property OnRestored;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnCtl3DChanged;
    property OnParentColorChange;
  end;


implementation

{**************************************************}

constructor TJvCustomCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColor := clInfoBk;
  {$IFNDEF COMPILER6_UP}
  FLastTime := 0;           // SPM - Ported backward from Delphi 7
  {$ENDIF}
  FAutoComplete := True;
  FSearching := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FOver := False;
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FAutoSave := TJvAutoSave.Create(Self);
  FItemSearchs := TJvItemsSearchs.Create;
end;

{**************************************************}

destructor TJvCustomCombobox.Destroy;
begin
  FMaxPixel.Free;
  FAutoSave.Free;
  FItemSearchs.Free;
  inherited;
end;

{**************************************************}

procedure TJvCustomCombobox.Loaded;
var
  st: string;
  i: Integer;
begin
  inherited;
  if Style = csDropDownList then
  begin
    if FAutoSave.LoadValue(i) then
    begin
      ItemIndex := i;
      if Assigned(FOnRestored) then
        FOnRestored(Self);
    end;
  end
  else
  begin
    if FAutoSave.LoadValue(st) then
    begin
      Text := st;
      if Assigned(FOnRestored) then
        FOnRestored(Self);
    end;
  end;
end;

{**************************************************}

procedure TJvCustomCombobox.Change;
var
  res: Integer;
  st: string;
  start, finish: Integer;
begin
  inherited;
  if not FSearching and FAutoComplete then
  begin
    st := Text;
    FMaxPixel.Test(st, Font);
    if Text <> st then
    begin
      Text := st;
      Exit;
    end;
    if (FKey <> 8) and (FKey <> 46) and (FKey <> 13) then
    begin
      FSearching := True;
      st := Text;
      res := SendMessage(Handle, CB_FINDSTRING, -1, Longint(PChar(st)));
      if res <> CB_ERR then
      try
        ItemIndex := res;
        start := Length(st);
        finish := Length(Items[res]) - start;
        Text := Items[res];
        SelStart := start;
        SelLength := finish;
      except
      end;
      FSearching := False;
    end;
  end;
  if Style = csDropDownList then
    FAutoSave.SaveValue(ItemIndex)
  else
    FAutoSave.SaveValue(Text);
end;

{**************************************************}

function TJvCustomCombobox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCustomCombobox.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCustomCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FKey := Key;
end;

{$IFNDEF COMPILER6_UP}
{**************************************************}
// SPM - Ported backward from Delphi 7 and modified:
procedure TJvCustomComboBox.KeyPress(var Key: Char);

  function HasSelectedText(var StartPos, EndPos: DWORD): Boolean;
  begin
    SendMessage(Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
    Result := EndPos > StartPos;
  end;

  procedure DeleteSelectedText;
  var
    StartPos, EndPos: DWORD;
    OldText: String;
  begin
    OldText := Text;
    SendMessage(Handle, CB_GETEDITSEL, Integer(@StartPos), Integer(@EndPos));
    Delete(OldText, StartPos + 1, EndPos - StartPos);
    SendMessage(Handle, CB_SETCURSEL, -1, 0);
    Text := OldText;
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(StartPos, StartPos));
  end;

var
  StartPos: DWORD;
  EndPos: DWORD;
  OldText: String;
  SaveText: String;
  Msg : TMSG;
  LastByte: Integer;
begin
  inherited KeyPress(Key);
  if not AutoComplete then exit;
  if Style in [csDropDown, csSimple] then
    FFilter := Text
  else
  begin
   if GetTickCount - FLastTime >= 500 then
      FFilter := '';
    FLastTime := GetTickCount;
  end;
  case Ord(Key) of
    VK_ESCAPE: exit;
    VK_BACK:
      begin
        if HasSelectedText(StartPos, EndPos) then
          DeleteSelectedText
        else
          if (Style in [csDropDown, csSimple]) and (Length(Text) > 0) then
          begin
            SaveText := Text;
            LastByte := StartPos;
            while ByteType(SaveText, LastByte) = mbTrailByte do Dec(LastByte);
            OldText := Copy(SaveText, 1, LastByte - 1);
            SendMessage(Handle, CB_SETCURSEL, -1, 0);
            Text := OldText + Copy(SaveText, EndPos + 1, MaxInt);
            SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(LastByte - 1, LastByte - 1));
            FFilter := Text;
          end
          else
          begin
            while ByteType(FFilter, Length(FFilter)) = mbTrailByte do
              Delete(FFilter, Length(FFilter), 1);
            Delete(FFilter, Length(FFilter), 1);
          end;
        Key := #0;
        Change;
      end;
  else // case
    if HasSelectedText(StartPos, EndPos) then
      SaveText := Copy(FFilter, 1, StartPos) + Key
    else
      SaveText := FFilter + Key;

    if Key in LeadBytes then
    begin
      if PeekMessage(Msg, Handle, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_CHAR) then
      begin
        if SelectItem(SaveText + Char(Msg.wParam)) then
        begin
          PeekMessage(Msg, Handle, 0, 0, PM_REMOVE);
          Key := #0
        end;
      end;
    end
    else
      if SelectItem(SaveText) then
        Key := #0
  end; // case
end;

{**************************************************}
// SPM - Ported backward from Delphi 7 and modified:
function TJvCustomComboBox.SelectItem(const AnItem: String): Boolean;
var
  Idx: Integer;
  ValueChange: Boolean;
begin
  if Length(AnItem) = 0 then
  begin
    Result := False;
    ItemIndex := -1;
    Change;
    exit;
  end;
  Idx := SendMessage(Handle, CB_FINDSTRING, -1, LongInt(PChar(AnItem)));
  Result := (Idx <> CB_ERR);
  if not Result then exit;
  ValueChange := Idx <> ItemIndex;
  SendMessage(Handle, CB_SETCURSEL, Idx, 0);
  if (Style in [csDropDown, csSimple]) then
  begin
    Text := AnItem + Copy(Items[Idx], Length(AnItem) + 1, MaxInt);
    SendMessage(Handle, CB_SETEDITSEL, 0, MakeLParam(Length(AnItem), Length(Text)));
  end
  else
  begin
    ItemIndex := Idx;
    FFilter := AnItem;
  end;
  if ValueChange then
  begin
    Click;
    Change;
  end;
end;
{$ENDIF}

{**************************************************}

procedure TJvCustomCombobox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvCustomCombobox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvCustomCombobox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then Exit;
    Application.HintColor := FColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvCustomCombobox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

function TJvCustomCombobox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCustomCombobox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCustomCombobox.MaxPixelChanged(Sender: TObject);
var
  st: string;
begin
  if Style <> csDropDownList then
  begin
    st := Text;
    FMaxPixel.Test(st, Font);
    if Text <> st then
      Text := st;
    SelStart := Length(Text);
  end;
end;

end.
