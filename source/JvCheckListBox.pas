{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckListBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This is a merging of the code in the original JvCheckListBox.pas and JvFixedCheckListBox.pas
Merging done 2002-06-05 by Peter Thornqvist [peter3@peter3.com]

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Below <100113.1101@compuserve.com>

Last Modified: 2002-06-05

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCheckListBox;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, CheckLst, Controls, Forms,
  JvItemsSearchs, JVCLVer;

type
  TJvCheckListBox = class(TCheckListBox)
  private
    FAboutJVCL: TJVCLAboutInfo;
    FEffect: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnSelectCancel: TNotifyEvent;
    FOver: Boolean;
    FMaxWidth: Integer;
    FScroll: Boolean;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FItemSearchs: TJvItemsSearchs;
    procedure SetHScroll(const Value: Boolean);
    procedure RefreshH;
    procedure SetEffect(const Value: Boolean);
  protected
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SelectCancel(var Msg: TMessage); message LBN_SELCANCEL;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Msg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;

    procedure SelectAll; {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure UnselectAll;
    procedure InvertSelection;
    procedure CheckAll;
    procedure UnCheckall;
    procedure InvertCheck;
    function GetChecked: TStringList;
    function GetUnChecked: TStringList;
    procedure DeleteSelected; {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure SaveToFile(FileName: TFileName);
    procedure LoadFromFile(FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property MultiSelect;
    property HotTrack: Boolean read FEffect write SetEffect default False;
    property HorScrollbar: Boolean read FScroll write SetHScroll default True;
    property HintColor: TColor read FColor write FColor default clInfoBk;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnSelectCancel: TNotifyEvent read FOnSelectCancel write FOnSelectCancel;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
  end;

implementation

type
  // Used for the load/save methods
  TCheckListRecord = record
    Checked: Boolean;
    StringSize: Integer;
  end;

constructor TJvCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxWidth := 0;
  FEffect := False;
  FColor := clInfoBk;
  FOver := False;
  FScroll := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FItemSearchs := TJvItemsSearchs.Create;
end;

destructor TJvCheckListBox.Destroy;
begin
  FItemSearchs.Free;
  inherited Destroy;
end;

procedure TJvCheckListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
    if FScroll then
      Style := Style or WS_HSCROLL
    else
      Style := Style xor WS_HSCROLL;
end;

procedure TJvCheckListBox.RefreshH;
var
  I: Integer;
  ItemWidth: Word;
begin
  FMaxWidth := 0;
  for I := 0 to Items.Count - 1 do
  begin
    ItemWidth := Canvas.TextWidth(Items[I] + ' ');
    if FMaxWidth < ItemWidth then
      FMaxWidth := ItemWidth;
  end;
  SetHScroll(FScroll);
end;

procedure TJvCheckListBox.WndProc(var Msg: TMessage);
var
  ItemWidth: Word;
begin
  case Msg.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        ItemWidth := Canvas.TextWidth(StrPas(PChar(Msg.lParam)) + ' ');
        if FMaxWidth < ItemWidth then
          FMaxWidth := ItemWidth;
        SetHScroll(FScroll);
      end;
    LB_DELETESTRING:
      begin
        ItemWidth := Canvas.TextWidth(Items[Msg.wParam] + ' ');
        if ItemWidth = FMaxWidth then
        begin
          inherited WndProc(Msg);
          RefreshH;
          Exit;
        end;
      end;
    LB_RESETCONTENT:
      begin
        FMaxWidth := 0;
        SetHScroll(FScroll);
      end;
    WM_SETFONT:
      begin
        inherited WndProc(Msg);
        Canvas.Font.Assign(Font);
        RefreshH;
        Exit;
      end;
  end;
  inherited WndProc(Msg);
end;

procedure TJvCheckListBox.SetHScroll(const Value: Boolean);
begin
  FScroll := Value;
  if FScroll then
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
end;

procedure TJvCheckListBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

procedure TJvCheckListBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

procedure TJvCheckListBox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    // for D7...
    if csDesigning in ComponentState then
      Exit;
    Application.HintColor := FColor;
    if FEffect then
      Ctl3D := True;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TJvCheckListBox.WMHScroll(var Msg: TWMHScroll);
var
  ScrollPos: Integer;
  R: TRect;
begin
  inherited;
  // (p3) what does this code do, really?
  if Msg.ScrollCode <> SB_ENDSCROLL then
  begin
    ScrollPos := GetScrollPos(Handle, SB_HORZ);
    if ScrollPos < 20 then
    begin
      R := ClientRect;
      R.Right := R.Left + 20;
      InvalidateRect(Handle, @R, False);
    end;
  end;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;

procedure TJvCheckListBox.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

procedure TJvCheckListBox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FEffect then
      Ctl3D := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TJvCheckListBox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

procedure TJvCheckListBox.SetEffect(const Value: Boolean);
begin
  FEffect := Value;
  if FEffect then
    Ctl3D := False;
end;

function TJvCheckListBox.SearchPrefix(Value: string; CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

procedure TJvCheckListBox.SelectCancel(var Msg: TMessage);
begin
  if Assigned(FOnSelectCancel) then
    FOnSelectCancel(Self);
end;

function TJvCheckListBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

function TJvCheckListBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

procedure TJvCheckListBox.SelectAll;
var
  I: Integer;
begin
  // (rom) simplified
  if MultiSelect then
    for I := 0 to Items.Count - 1 do
      Selected[I] := True;
end;

procedure TJvCheckListBox.UnselectAll;
var
  I: Integer;
begin
  if MultiSelect then
    for I := 0 to Items.Count - 1 do
      Selected[I] := False;
end;

procedure TJvCheckListBox.InvertSelection;
var
  I: Integer;
begin
  if MultiSelect then
    for I := 0 to Items.Count - 1 do
      Selected[I] := not Selected[I];
end;

procedure TJvCheckListBox.CheckAll;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Checked[I] := True;
end;

procedure TJvCheckListBox.UnCheckall;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Checked[I] := False;
end;

procedure TJvCheckListBox.InvertCheck;
var
  I: Integer;
begin
  for I := 0 to Items.Count - 1 do
    Checked[I] := not Checked[I];
end;

function TJvCheckListBox.GetChecked: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Items.Count - 1 do
    if Checked[I] then
      Result.AddObject(Items[I], Items.Objects[I]);
end;

function TJvCheckListBox.GetUnChecked: TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to Items.Count - 1 do
    if not Checked[I] then
      Result.AddObject(Items[I], Items.Objects[I]);
end;

procedure TJvCheckListBox.LoadFromFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Stream);
  Stream.Free;
end;

procedure TJvCheckListBox.LoadFromStream(Stream: TStream);
var
  CheckLst: TCheckListRecord;
  Buf: array [0..1023] of Char;
begin
  Items.Clear;
  while Stream.Position + SizeOf(TCheckListRecord) <= Stream.Size do
  begin
    Stream.Read(CheckLst, SizeOf(TCheckListRecord));
    if Stream.Position + CheckLst.StringSize <= Stream.Size then
    begin
      Stream.Read(Buf, CheckLst.StringSize);
      Buf[CheckLst.StringSize] := #0;
      Checked[Items.Add(Buf)] := CheckLst.Checked;
    end;
  end;
end;

procedure TJvCheckListBox.SaveToFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  SaveToStream(Stream);
  Stream.Free;
end;

procedure TJvCheckListBox.SaveToStream(Stream: TStream);
var
  I, J: Integer;
  CheckLst: TCheckListRecord;
  Buf: array [1..1023] of Char;
begin
  for I := 0 to Items.Count - 1 do
  begin
    CheckLst.Checked := Checked[I];
    CheckLst.StringSize := Length(Items[I]);
    Stream.Write(CheckLst, SizeOf(TCheckListRecord));
    for J := 1 to Length(Items[I]) do
      Buf[J] := Items[I][J];
    Stream.Write(Buf, CheckLst.StringSize);
  end;
end;

procedure TJvCheckListBox.DeleteSelected;
var
  I: Integer;
begin
  if MultiSelect then
  begin
    for I := Items.Count - 1 downto 0 do
      if Selected[I] then
        Items.Delete(I);
  end
  else
  if ItemIndex <> -1 then
  begin
    I := ItemIndex;
    Items.Delete(I);
    if I > 0 then
      Dec(I);
    if Items.Count > 0 then
      ItemIndex := I;
  end;
end;

end.
