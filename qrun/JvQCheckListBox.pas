{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckListBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This is a merging of the code in the original JvCheckListBox.pas and JvFixedCheckListBox.pas
Merging done 2002-06-05 by Peter Thornqvist [peter3 at sourceforge dot net]

Contributor(s):
Michael Beck [mbeck att bigfoot dott com].
Peter Below <100113 dott 1101 att compuserve dott com>

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQCheckListBox;

{$I jvcl.inc}

interface

uses
  Types, QWindows, QMessages, SysUtils, Classes, QControls,
  JvQExCheckLst;

type
  TJvCheckListBox = class(TJvExCheckListBox) 
  public
    constructor Create(AOwner: TComponent); override;
    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
    procedure SelectAll; 
    procedure UnselectAll;
    procedure InvertSelection;
    procedure CheckAll;
    procedure UnCheckAll;
    procedure InvertCheck;
    function GetChecked: TStringList;
    function GetUnChecked: TStringList;
    procedure DeleteSelected; 
    procedure SaveToFile(FileName: TFileName);
    procedure LoadFromFile(FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property MultiSelect;
    property HintColor;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange; 
  end;

implementation

uses
  JvQItemsSearchs;

type
  // Used for the load/save methods
  TCheckListRecord = record
    Checked: Boolean;
    StringSize: Integer;
  end;

constructor TJvCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner); 
  // ControlStyle := ControlStyle + [csAcceptsControls];
end;



function TJvCheckListBox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

function TJvCheckListBox.SearchPrefix(Value: string; CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;



function TJvCheckListBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

function TJvCheckListBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := TJvItemsSearchs.DeleteExactString(Items, Value, CaseSensitive);
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

procedure TJvCheckListBox.UnCheckAll;
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
