{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvIcoList.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 1997, 1998 Fedor Koshevnikov, Igor Pavluk and Serge Korolev
Copyright (c) 2001,2002 SGB Software
All Rights Reserved.

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}

unit JvIconList;

interface

uses
  Windows, SysUtils,
  Graphics,
  Classes;

type
  TJvIconList = class(TPersistent)
  private
    FList: TList;
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
    procedure SetUpdateState(Updating: Boolean);
    procedure IconChanged(Sender: TObject);
    function AddIcon(Icon: TIcon): Integer;
  protected
    procedure Changed; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function Get(Index: Integer): TIcon; virtual;
    function GetCount: Integer; virtual;
    procedure Put(Index: Integer; Icon: TIcon); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(Icon: TIcon): Integer; virtual;
    function AddResource(Instance: THandle; ResId: PChar): Integer; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Clear; virtual;
    procedure Delete(Index: Integer); virtual;
    procedure Exchange(Index1, Index2: Integer); virtual;
    function IndexOf(Icon: TIcon): Integer; virtual;
    procedure Insert(Index: Integer; Icon: TIcon); virtual;
    procedure InsertResource(Index: Integer; Instance: THandle;
      ResId: PChar); virtual;
    procedure LoadResource(Instance: THandle; const ResIds: array of PChar);
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure Move(CurIndex, NewIndex: Integer); virtual;
    procedure SaveToStream(Stream: TStream); virtual;
    property Count: Integer read GetCount;
    property Icons[Index: Integer]: TIcon read Get write Put; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

implementation

constructor TJvIconList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TJvIconList.Destroy;
begin
  FOnChange := nil;
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TJvIconList.BeginUpdate;
begin
  if FUpdateCount = 0 then
    SetUpdateState(True);
  Inc(FUpdateCount);
end;

procedure TJvIconList.Changed;
begin
  if (FUpdateCount = 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TJvIconList.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    SetUpdateState(False);
end;

procedure TJvIconList.ReadData(Stream: TStream);
var
  Len, Cnt: Longint;
  I: Integer;
  Icon: TIcon;
  Mem: TMemoryStream;
begin
  BeginUpdate;
  try
    Clear;
    Mem := TMemoryStream.Create;
    try
      Stream.Read(Cnt, SizeOf(Longint));
      for I := 0 to Cnt - 1 do
      begin
        Stream.Read(Len, SizeOf(Longint));
        if Len > 0 then
        begin
          Icon := TIcon.Create;
          try
            Mem.SetSize(Len);
            Stream.Read(Mem.Memory^, Len);
            Mem.Position := 0;
            Icon.LoadFromStream(Mem);
            AddIcon(Icon);
          except
            Icon.Free;
            raise;
          end;
        end
        else
          AddIcon(nil);
      end;
    finally
      Mem.Free;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TJvIconList.WriteData(Stream: TStream);
var
  I: Integer;
  Len: Longint;
  Mem: TMemoryStream;
begin
  Mem := TMemoryStream.Create;
  try
    Len := FList.Count;
    Stream.Write(Len, SizeOf(Longint));
    for I := 0 to FList.Count - 1 do
    begin
      Mem.Clear;
      if (Icons[I] <> nil) and not Icons[I].Empty then
      begin
        Icons[I].SaveToStream(Mem);
        Len := Mem.Size;
      end
      else
        Len := 0;
      Stream.Write(Len, SizeOf(Longint));
      if Len > 0 then
        Stream.Write(Mem.Memory^, Mem.Size);
    end;
  finally
    Mem.Free;
  end;
end;

procedure TJvIconList.DefineProperties(Filer: TFiler);

  function DoWrite: Boolean;
  var
    I: Integer;
    Ancestor: TJvIconList;
  begin
    Ancestor := TJvIconList(Filer.Ancestor);
    if (Ancestor <> nil) and (Ancestor.Count = Count) and (Count > 0) then
    begin
      Result := False;
      for I := 0 to Count - 1 do
      begin
        Result := Icons[I] <> Ancestor.Icons[I];
        if Result then
          Break;
      end
    end
    else
      Result := Count > 0;
  end;

begin
  Filer.DefineBinaryProperty('Icons', ReadData, WriteData, DoWrite);
end;

function TJvIconList.Get(Index: Integer): TIcon;
begin
  Result := TObject(FList[Index]) as TIcon;
end;

function TJvIconList.GetCount: Integer;
begin
  Result := FList.Count;
end;

procedure TJvIconList.IconChanged(Sender: TObject);
begin
  Changed;
end;

procedure TJvIconList.Put(Index: Integer; Icon: TIcon);
begin
  BeginUpdate;
  try
    if Index = Count then
      Add(nil);
    if Icons[Index] = nil then
      FList[Index] := TIcon.Create;
    Icons[Index].OnChange := IconChanged;
    Icons[Index].Assign(Icon);
  finally
    EndUpdate;
  end;
end;

function TJvIconList.AddIcon(Icon: TIcon): Integer;
begin
  Result := FList.Add(Icon);
  if Icon <> nil then
    Icon.OnChange := IconChanged;
  Changed;
end;

function TJvIconList.Add(Icon: TIcon): Integer;
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  try
    Ico.Assign(Icon);
    Result := AddIcon(Ico);
  except
    Ico.Free;
    raise;
  end;
end;

function TJvIconList.AddResource(Instance: THandle; ResId: PChar): Integer;
var
  Ico: TIcon;
  {$IFDEF VisualCLX}
  ResStream: TResourceStream;
  {$ENDIF VisualCLX}
begin
  Ico := TIcon.Create;
  try
    {$IFDEF VCL}
    Ico.Handle := LoadIcon(Instance, ResId);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    ResStream := TResourceStream.CreateFromID(Instance, Integer(ResId), RT_RCDATA);
    try
      Ico.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
    {$ENDIF VisualCLX}
    Result := AddIcon(Ico);
  except
    Ico.Free;
    raise;
  end;
end;

procedure TJvIconList.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source = nil then
    Clear
  else
  if Source is TJvIconList then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TJvIconList(Source).Count - 1 do
        Add(TJvIconList(Source)[I]);
    finally
      EndUpdate;
    end;
  end
  else
  if Source is TIcon then
  begin
    BeginUpdate;
    try
      Clear;
      Add(TIcon(Source));
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TJvIconList.Clear;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := FList.Count - 1 downto 0 do
      Delete(I);
  finally
    EndUpdate;
  end;
end;

procedure TJvIconList.Delete(Index: Integer);
var
  Icon: TIcon;
begin
  Icon := Icons[Index];
  if Icon <> nil then
  begin
    Icon.OnChange := nil;
    Icon.Free;
  end;
  FList.Delete(Index);
  Changed;
end;

procedure TJvIconList.Exchange(Index1, Index2: Integer);
begin
  FList.Exchange(Index1, Index2);
  Changed;
end;

function TJvIconList.IndexOf(Icon: TIcon): Integer;
begin
  Result := FList.IndexOf(Icon);
end;

procedure TJvIconList.InsertResource(Index: Integer; Instance: THandle;
  ResId: PChar);
var
  Ico: TIcon;
  {$IFDEF VisualCLX}
  ResStream: TResourceStream;
  {$ENDIF VisualCLX}
begin
  Ico := TIcon.Create;
  try
    {$IFDEF VCL}
    Ico.Handle := LoadIcon(Instance, ResId);
    {$ENDIF VCL}
    {$IFDEF VisualCLX}
    ResStream := TResourceStream.CreateFromID(Instance, Integer(ResId), RT_RCDATA);
    try
      Ico.LoadFromStream(ResStream);
    finally
      ResStream.Free;
    end;
    {$ENDIF VisualCLX}
    FList.Insert(Index, Ico);
    Ico.OnChange := IconChanged;
  except
    Ico.Free;
    raise;
  end;
  Changed;
end;

procedure TJvIconList.Insert(Index: Integer; Icon: TIcon);
var
  Ico: TIcon;
begin
  Ico := TIcon.Create;
  try
    Ico.Assign(Icon);
    FList.Insert(Index, Ico);
    Ico.OnChange := IconChanged;
  except
    Ico.Free;
    raise;
  end;
  Changed;
end;

procedure TJvIconList.LoadResource(Instance: THandle; const ResIds: array of PChar);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := Low(ResIds) to High(ResIds) do
      AddResource(Instance, ResIds[I]);
  finally
    EndUpdate;
  end;
end;

procedure TJvIconList.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
  Changed;
end;

procedure TJvIconList.SetUpdateState(Updating: Boolean);
begin
  if not Updating then
    Changed;
end;

procedure TJvIconList.LoadFromStream(Stream: TStream);
begin
  ReadData(Stream);
end;

procedure TJvIconList.SaveToStream(Stream: TStream);
begin
  WriteData(Stream);
end;

end.

