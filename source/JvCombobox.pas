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
  Windows, Dialogs, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  JvMaxPixel, JvPropAutoSave, JvItemsSearchs, JVCLVer;

type
  TJvCustomComboBox = class(TCustomComboBox)
  private
    FKey: Word;
    FAutoComplete: Boolean;
    {$IFNDEF COMPILER6_UP}
    FLastTime: Cardinal;      // SPM - Ported backward from Delphi 7
    FFilter: string;          // SPM - ditto
    {$ENDIF}
    FSearching: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FHintColor: TColor;
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
    FReadOnly: Boolean; // ain
    procedure MaxPixelChanged(Sender: TObject);
    procedure SetReadOnly(const Value: Boolean); // ain
  protected
    procedure Change; override;
    procedure CreateWnd; override; // ain
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    {$IFNDEF COMPILER6_UP}
    procedure KeyPress(var Key: Char); override;  // SPM - Ported backward from D7
    {$ENDIF}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure WMLButtonDown(var Msg: TWMLButtonDown); message WM_LBUTTONDOWN; // ain
    procedure WMLButtonDblClk(var Msg: TWMLButtonDown); message WM_LBUTTONDBLCLK; // ain
    {$IFNDEF COMPILER6_UP}
    function SelectItem(const AnItem: string): Boolean;  // SPM - Ported from D7
    {$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure WndProc(var Msg: TMessage); override; // ain
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
    property HintColor: TColor read FHintColor write FHintColor default clInfoBk;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False; // ain

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
    property ReadOnly; // ain
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

constructor TJvCustomComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FHintColor := clInfoBk;
  FAutoComplete := True;
  {$IFNDEF COMPILER6_UP}
  FLastTime := 0;           // SPM - Ported backward from Delphi 7
  {$ENDIF}
  FSearching := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FOver := False;
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FAutoSave := TJvAutoSave.Create(Self);
  FItemSearchs := TJvItemsSearchs.Create;
  FReadOnly := False; // ain
end;

destructor TJvCustomComboBox.Destroy;
begin
  FMaxPixel.Free;
  FAutoSave.Free;
  FItemSearchs.Free;
  inherited;
end;

procedure TJvCustomComboBox.Loaded;
var
  St: string;
  I: Integer;
begin
  inherited;
  if Style = csDropDownList then
  begin
    if FAutoSave.LoadValue(I) then
    begin
      ItemIndex := I;
      if Assigned(FOnRestored) then
        FOnRestored(Self);
    end;
  end
  else
  begin
    if FAutoSave.LoadValue(St) then
    begin
      Text := St;
      if Assigned(FOnRestored) then
        FOnRestored(Self);
    end;
  end;
end;

procedure TJvCustomComboBox.Change;
var
  Res: Integer;
  St: string;
  Start, Finish: Integer;
begin
  inherited;
  if not FSearching and FAutoComplete then
  begin
    St := Text;
    FMaxPixel.Test(St, Font);
    if Text <> St then
    begin
      Text := St;
      Exit;
    end;
    if (FKey <> VK_BACK) and (FKey <> VK_DELETE) and (FKey <> VK_RETURN) then
    begin
      FSearching := True;
      St := Text;
      Res := SendMessage(Handle, CB_FINDSTRING, -1, Longint(PChar(St)));
      if Res <> CB_ERR then
      try
        ItemIndex := Res;
        Start := Length(St);
        Finish := Length(Items[Res]) - Start;
        Text := Items[Res];
        SelStart := Start;
        SelLength := Finish;
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

function TJvCustomComboBox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

function TJvCustomComboBox.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

procedure TJvCustomComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FKey := Key;
end;

{$IFNDEF COMPILER6_UP}

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
  OldText: string;
  SaveText: string;
  Msg: TMsg;
  LastByte: Integer;
begin
  inherited KeyPress(Key);
  if not AutoComplete then
    Exit;
  if Style in [csDropDown, csSimple] then
    FFilter := Text
  else
  begin
   if GetTickCount - FLastTime >= 500 then
      FFilter := '';
    FLastTime := GetTickCount;
  end;
  case Ord(Key) of
    VK_ESCAPE:
      Exit;
    VK_BACK:
      begin
        if HasSelectedText(StartPos, EndPos) then
          DeleteSelectedText
        else
        if (Style in [csDropDown, csSimple]) and (Length(Text) > 0) then
        begin
          SaveText := Text;
          LastByte := StartPos;
          while ByteType(SaveText, LastByte) = mbTrailByte do
            Dec(LastByte);
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
  else
    if HasSelectedText(StartPos, EndPos) then
      SaveText := Copy(FFilter, 1, StartPos) + Key
    else
      SaveText := FFilter + Key;

    if Key in LeadBytes then
    begin
      if PeekMessage(Msg, Handle, 0, 0, PM_NOREMOVE) and (Msg.Message = WM_CHAR) then
      begin
        if SelectItem(SaveText + Char(Msg.WParam)) then
        begin
          PeekMessage(Msg, Handle, 0, 0, PM_REMOVE);
          Key := #0;
        end;
      end;
    end
    else
      if SelectItem(SaveText) then
        Key := #0;
  end;
end;

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
    Exit;
  end;
  Idx := SendMessage(Handle, CB_FINDSTRING, -1, LongInt(PChar(AnItem)));
  Result := (Idx <> CB_ERR);
  if not Result then
    Exit;
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

{$ENDIF COMPILER6_UP}

procedure TJvCustomComboBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCustomComboBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCustomComboBox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FHintColor;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCustomComboBox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TJvCustomComboBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

function TJvCustomComboBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

procedure TJvCustomComboBox.MaxPixelChanged(Sender: TObject);
var
  St: string;
begin
  if Style <> csDropDownList then
  begin
    St := Text;
    FMaxPixel.Test(St, Font);
    if Text <> St then
      Text := St;
    SelStart := Length(Text);
  end;
end;

procedure TJvCustomComboBox.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    SendMessage(EditHandle, EM_SETREADONLY, Ord(Value), 0);
  end;
end;

procedure TJvCustomComboBox.WndProc(var Msg: TMessage);
begin
  if ReadOnly and not (csDesigning in ComponentState) then
  begin
    case Msg.Msg of
      WM_KEYDOWN:
        begin
          if Msg.WParam in [VK_DOWN, VK_UP, VK_RIGHT, VK_LEFT, VK_F4] then
          begin
            // see keelab aktiivse itemi vahetamise nooleklahvidega DDL kui CB on aktiivne
            Msg.Result := 0;
            Exit;
          end;
        end;
      WM_CHAR:
        begin
          // DDL trykkides ei aktiveeriks selle tahega algavat itemit
          Msg.Result := 0;
          Exit;
        end;
      WM_SYSKEYDOWN:
        begin
          if (Msg.WParam = VK_DOWN) or (Msg.WParam = VK_UP) then
          begin
            // see keelab Ald+Down listi avamise fookuses DDL CB-l
            Msg.Result := 0;
            Exit;
          end;
        end;
      WM_COMMAND:
        begin
          // DD editis nooleklahviga vahetamise valtimiseks kui fookuses
          if HiWord(Msg.WParam) = CBN_SELCHANGE then
          begin
            Msg.Result := 0;
            Exit;
          end;
        end;
      WM_USER + $B900:
        begin
          if Msg.WParam = VK_F4 then
          begin
            // DD F4 ei avaks
            Msg.Result := 1;
            Exit;
          end;
        end;
      WM_USER + $B904:
        begin
          if (Msg.WParam = VK_DOWN) or (Msg.WParam = VK_UP) then
          begin
            // DD Alt+ down ei avaks
            Msg.Result := 1;
            Exit;
          end;
        end;
    end;
  end;
  inherited WndProc(Msg);
end;

procedure TJvCustomComboBox.WMLButtonDown(var Msg: TWMLButtonDown);
begin
  if ReadOnly then
    SetFocus
  else
    inherited;
end;

procedure TJvCustomComboBox.WMLButtonDblClk(var Msg: TWMLButtonDown);
begin
  if not ReadOnly then
    inherited;
end;

procedure TJvCustomComboBox.CreateWnd;
begin
  inherited CreateWnd;
  SendMessage(EditHandle, EM_SETREADONLY, Ord(ReadOnly), 0);
end;

end.

