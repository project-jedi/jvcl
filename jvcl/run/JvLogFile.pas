{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLogFile.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvLogFile;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, Controls, Forms, Contnrs,
  JvComponent;

type
  TJvLogRecord = class(TObject)
  public
    Time: string;
    Title: string;
    Description: string;

    function GetOutputString: string;
  end;

  TJvLogRecordList = class(TObjectList)
  private
    function GetItem(Index: Integer): TJvLogRecord;
    procedure SetItem(Index: Integer; const ALogRecord: TJvLogRecord);
  public
    property Items[Index: Integer]: TJvLogRecord read GetItem write SetItem; default;
  end;

  TJvLogFile = class(TJvComponent)
  private
    FList: TJvLogRecordList;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    FFileName: TFilename;
    FActive: Boolean;
    FAutoSave: Boolean;
    FSizeLimit: Cardinal;
    function GetElement(Index: Integer): TJvLogRecord;
    procedure SetAutoSave(const Value: Boolean);
    procedure DoAutoSave;
    procedure EnsureSize;
    procedure SetFileName(const Value: TFilename);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    procedure Add(const Time, Title: string; const Description: string); overload;
    procedure Add(const Title: string; const Description: string = ''); overload;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Count: Integer;
    property Elements[Index: Integer]: TJvLogRecord read GetElement; default;

    procedure ShowLog(Title: string);
  published
    // (obones) some extra properties to make transparent use a bit easier
    property FileName: TFilename read FFileName write SetFileName;
    property Active: Boolean read FActive write FActive default True;
    property AutoSave: Boolean read FAutoSave write SetAutoSave default False;
    property SizeLimit: Cardinal read FSizeLimit write FSizeLimit default 0;  // 0 for infinity

    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvLogForm, JvConsts;

// === { TJvLogRecord } =======================================

function TJvLogRecord.GetOutputString: string;
begin
  Result := '[' + Time + ']' + StringReplace(Title, '>', '>>', [rfReplaceAll]) +
            '>' + Description + sLineBreak;
end;

// === { TJvLogRecordList } ===================================

function TJvLogRecordList.GetItem(Index: Integer): TJvLogRecord;
begin
  Result := TJvLogRecord(inherited Items[Index]);
end;

procedure TJvLogRecordList.SetItem(Index: Integer;
  const ALogRecord: TJvLogRecord);
begin
  inherited Items[Index] := ALogRecord;
end;

// === { TJvLogFile } =========================================

constructor TJvLogFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TJvLogRecordList.Create(True);

  // Set default values
  FActive := True;
  FAutoSave := False;
  FSizeLimit := 0;
end;

destructor TJvLogFile.Destroy;
begin
  DoAutoSave;
  FList.Free;
  inherited Destroy;
end;

procedure TJvLogFile.Add(const Time, Title, Description: string);
var
  LogRecord: TJvLogRecord;
begin
  if not Active then // Do not log if not active (obones)
    Exit;
    
  LogRecord := TJvLogRecord.Create;
  LogRecord.Time := Time;
  LogRecord.Title := Title;
  LogRecord.Description := Description;
  FList.Add(LogRecord);
  EnsureSize;
  DoAutoSave;
end;

procedure TJvLogFile.Add(const Title, Description: string);
begin
  Add(DateTimeToStr(Now), Title, Description);
end;

procedure TJvLogFile.Clear;
begin
  FList.Clear;
  DoAutoSave;
end;

function TJvLogFile.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TJvLogFile.Delete(Index: Integer);
begin
  FList.Delete(Index);
  DoAutoSave;
end;

procedure TJvLogFile.DoAutoSave;
begin
  if AutoSave then
    SaveToFile(Filename);
end;

procedure TJvLogFile.EnsureSize;
var
  SavedAutoSave: Boolean;
  I, J : Integer;
  Size : Cardinal;
begin
  if SizeLimit > 0 then
  begin
    // prevent file from being updated while we modify it
    SavedAutoSave := FAutoSave;
    AutoSave := False;

    // Calculate size, starting from the last item, so that
    // we will only delete the oldest items if required.
    I := FList.Count-1;
    Size := 0;
    while (I >= 0) and (Size < SizeLimit) do
    begin
      Inc(Size, Length(FList[I].GetOutputString));
      Dec(I);
    end;

    // Delete any left over items
    if (I >= 0) and (Size >= SizeLimit) then
      for J := 0 to I do
        Delete(0);

    // Restore saved value and force save if required
    FAutoSave := SavedAutoSave;
    DoAutoSave;
  end;
end;

function TJvLogFile.GetElement(Index: Integer): TJvLogRecord;
begin
  Result := TJvLogRecord(FList[Index]);
end;

procedure TJvLogFile.LoadFromFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvLogFile.LoadFromStream(Stream: TStream);
var
  I, J, L: Integer;
  LogRecord: TJvLogRecord;
  Found: Boolean;
  SavedAutoSave : Boolean;
begin
  SavedAutoSave := AutoSave;
  AutoSave := False;
  Clear;
  AutoSave := SavedAutoSave;
  with TStringList.Create do
  try
    LoadFromStream(Stream);
    for I := 0 to Count - 1 do
    begin
      LogRecord := TJvLogRecord.Create;

      //Extract time
      J := Pos('[', Strings[I]);
      if J = 0 then
      begin
        LogRecord.Free;
        Continue;
      end;
      LogRecord.Time := Copy(Strings[I], J + 1, MaxInt);
      J := Pos(']', LogRecord.Time);
      if J = 0 then
      begin
        LogRecord.Free;
        Continue;
      end;
      LogRecord.Title := Copy(LogRecord.Time, J + 1, MaxInt);
      System.Delete(LogRecord.Time, J, MaxInt);

      //Extract title and description
      J := 1;
      L := Length(LogRecord.Title);
      Found := False;
      while (J <= L) and not Found do
      begin
        if LogRecord.Title[J] = '>' then
        begin
          if (J < L) and (LogRecord.Title[J + 1] = '>') then
            Inc(J, 2)
          else
            Found := True;
        end
        else
          Inc(J);
      end;
      // if there's '>', get description field, otherwise assume there's no description
      if Found then
        LogRecord.Description := Copy(LogRecord.Title, J + 1, MaxInt);
      // if J = L (nothing was found), then nothing is deleted,
      // otherwise everything is deleted starting with '>' found
      System.Delete(LogRecord.Title, J, L);
      LogRecord.Title := StringReplace(LogRecord.Title, '>>', '>', [rfReplaceAll]);
      FList.Add(LogRecord);
    end;
  finally
    Free;
  end;
end;

procedure TJvLogFile.SaveToFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TJvLogFile.SaveToStream(Stream: TStream);
var
  I: Integer;
  St: string;
begin
  with TStringList.Create do
  try
    for I := 0 to FList.Count - 1 do
      with FList[I] do
      begin
        St := GetOutputString;
        Stream.WriteBuffer(Pointer(St)^, Length(St));
      end;
  finally
    Free;
  end;
end;

procedure TJvLogFile.SetAutoSave(const Value: Boolean);
begin
  FAutoSave := Value and (Filename <> '');  // can't autosave if no filename (obones)
end;

procedure TJvLogFile.SetFilename(const Value: TFilename);
begin
  FFilename := Value;
  LoadFromFile(Filename);
end;

procedure TJvLogFile.ShowLog(Title: string);
var
  I: Integer;
begin
  with TFoLog.Create(nil) do
  try
    Caption := Title;
    with ListView1 do
    begin
      Items.BeginUpdate;
      for I := 0 to FList.Count - 1 do
        with FList[I] do
          with Items.Add do
          begin
            Caption := Time;
            SubItems.Add(Title);
            SubItems.Add(Description);
          end;
      Items.EndUpdate;
    end;

    if Assigned(FOnShow) then
      FOnShow(Self);
    ShowModal;
    if Assigned(FOnClose) then
      FOnClose(Self);
  finally
    Free;
  end;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

