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

unit JvQLogFile;

{$I jvcl.inc}

interface

uses
  SysUtils, Classes, QControls, QForms,
  JvQComponent;

type
  TJvLogRecord = class(TObject)
  public
    Time: string;
    Title: string;
    Description: string;
  end;

  TJvLogFile = class(TJvComponent)
  private
    FList: TList;
    FOnClose: TNotifyEvent;
    FOnShow: TNotifyEvent;
    function GetElement(Index: Integer): TJvLogRecord;
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
    property OnShow: TNotifyEvent read FOnShow write FOnShow;
    property OnClose: TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  JvQLogForm, JvQConsts;

constructor TJvLogFile.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FList := TList.Create;
end;

destructor TJvLogFile.Destroy;
begin
  Clear;
  FList.Free;
  inherited Destroy;
end;

procedure TJvLogFile.Add(const Time, Title, Description: string);
var
  LogRecord: TJvLogRecord;
begin
  LogRecord := TJvLogRecord.Create;
  LogRecord.Time := Time;
  LogRecord.Title := Title;
  LogRecord.Description := Description;
  FList.Add(LogRecord);
end;

procedure TJvLogFile.Add(const Title, Description: string);
begin
  Add(DateTimeToStr(Now), Title, Description);
end;

procedure TJvLogFile.Clear;
var
  I: Integer;
begin
  // (rom) improved
  for I := 0 to FList.Count - 1 do
  begin
    TJvLogRecord(FList.Items[I]).Free;
    FList.Items[I] := nil;
  end;
  FList.Clear;
end;

function TJvLogFile.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TJvLogFile.Delete(Index: Integer);
begin
  TJvLogRecord(FList.Items[Index]).Free;
  FList.Delete(Index);
end;

function TJvLogFile.GetElement(Index: Integer): TJvLogRecord;
begin
  Result := TJvLogRecord(FList.Items[Index]);
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
begin
  Clear;
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
      with TJvLogRecord(FList.Items[I]) do
      begin
        St := '[' + Time + ']' + StringReplace(Title, '>', '>>', [rfReplaceAll]) +
          '>' + Description + sLineBreak;
        Stream.WriteBuffer(Pointer(St)^, Length(St));
      end;
  finally
    Free;
  end;
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
        with TJvLogRecord(FList[I]) do
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

