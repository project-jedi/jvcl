{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvLogFile.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].

Last Modified: 2000-02-28

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
Known Issues:
-----------------------------------------------------------------------------}
{$A+,B-,C+,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O+,P+,Q-,R-,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$I JEDI.INC}

unit JvLogFile;

interface

{$ObjExportAll On}

uses
  Windows, Messages, SysUtils, Classes, Graphics, ExtCtrls, Controls,   Forms ,JvComponent;

type
  TLogRecord = class
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
    function GetElement(Index: Integer): TLogRecord;
  protected
  public
    constructor Create(AOwner: TComponent);override;
    destructor Destroy;override;

    procedure LoadFromFile(FileName: TFileName);
    procedure SaveToFile(FileName: TFileName);
    procedure SaveToStream(Stream: TStream);
    procedure LoadFromStream(Stream: TStream);

    procedure Add(Time: string;Title: string; Description: string='');overload;
    procedure Add(Title: string);overload;
    procedure Delete(Index: Integer);
    procedure Clear;
    function Count: Integer;
    property Elements[Index: Integer]: TLogRecord read GetElement;default;

    procedure ShowLog(Title: string);
  published
    property OnShow:TNotifyEvent read FOnShow write FOnShow;
    property OnClose:TNotifyEvent read FOnClose write FOnClose;
  end;

implementation

uses
  JvFormLog;

{*******************************************************}
procedure TJvLogFile.Add(Time, Title, Description: string);
var
 LogRecord: TLogRecord;
begin
  LogRecord := TLogRecord.Create;
  LogRecord.Time := Time;
  LogRecord.Title := Title;
  LogRecord.Description := Description;
  FList.Add(LogRecord);
end;
{*******************************************************}
procedure TJvLogFile.Add(Title: string);
begin
  Add(DateTimeToStr(Now),Title);
end;
{*******************************************************}
procedure TJvLogFile.Clear;
var
 i: Integer;
begin
  for i:=0 to FList.Count-1 do
    Delete(0);
end;
{*******************************************************}
function TJvLogFile.Count: Integer;
begin
  result := FList.Count;
end;
{*******************************************************}
constructor TJvLogFile.Create(AOwner: TComponent);
begin
  inherited;
  FList := TList.Create;
end;
{*******************************************************}
procedure TJvLogFile.Delete(Index: Integer);
begin
  TLogRecord(FList.Items[Index]).Free;
  FList.Delete(Index);
end;
{*******************************************************}
destructor TJvLogFile.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;
{*******************************************************}
function TJvLogFile.GetElement(Index: Integer): TLogRecord;
begin
  result := TLogRecord(FList.Items[Index]);
end;
{*******************************************************}
procedure TJvLogFile.LoadFromFile(FileName: TFileName);
var
 Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Stream);
  Stream.Free;
end;
{*******************************************************}
procedure TJvLogFile.LoadFromStream(Stream: TStream);
var
 i,j: Integer;
 LogRecord: TLogRecord;
 Found: boolean;
begin
  Clear;
  with TStringList.Create do
  begin
    LoadFromStream(Stream);
    for i:=0 to Count-1 do
    begin
      LogRecord := TLogRecord.Create;

      //Extract time
      j := pos('[',Strings[i]);
      if j=0 then
      begin
        LogRecord.Free;
        Continue;
      end;
      LogRecord.Time := Copy(Strings[i],j+1,Length(Strings[i]));
      j := pos(']',LogRecord.Time);
      if j=0 then
      begin
        LogRecord.Free;
        Continue;
      end;
      LogRecord.Title := Copy(LogRecord.Time,j+1,Length(LogRecord.Time));
      LogRecord.Time := Copy(LogRecord.Time,1,j-1);

      //Extract title and description
      j := 1;
      Found := false;
      while (j<Length(LogRecord.Title)) and not(Found) do
      begin
        if LogRecord.Title[j]='>' then
        begin
          if (j+1<Length(LogRecord.Title)) and (LogRecord.Title[j+1]='>') then
            inc(j,2)
          else
            Found := true;
        end
        else
          inc(j);
      end;
      if not(Found) then
      begin
        LogRecord.Free;
        Continue;
      end;
      LogRecord.Description := Copy(LogRecord.Title,j+1,Length(LogRecord.Title));
      LogRecord.Title := Copy(LogRecord.Title,1,j-1);
      LogRecord.Title := StringReplace(LogRecord.Title,'>>','>',[rfReplaceAll]);
      FList.Add(LogRecord);
    end;

    Free;
  end;
end;
{*******************************************************}
procedure TJvLogFile.SaveToFile(FileName: TFileName);
var
 Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName,fmCreate or fmShareExclusive);
  SaveToStream(Stream);
  Stream.Free;
end;
{*******************************************************}
procedure TJvLogFile.SaveToStream(Stream: TStream);
var
 i: Integer;
begin
  with TStringList.Create do
  begin
    for i:=0 to FList.Count-1 do
      with TLogRecord(FList.Items[i]) do
        Add('['+Time+']'+StringReplace(Title,'>','>>',[rfReplaceAll])+'>'+Description);
    SaveToStream(Stream);
    Free;
  end;
end;
{*******************************************************}
procedure TJvLogFile.ShowLog(Title: string);
var
 i: Integer;
begin
  with TfoLog.Create(nil) do
  begin
    Caption := Title;
    with buListView1 do
    begin
      Items.BeginUpdate;
      for i:=0 to FList.Count-1 do
        with TLogRecord(FList[i]) do
          with Items.Add do
          begin
            Caption := Time;
            SubItems.Add(Title);
            SubItems.Add(Description);
          end;
      Items.EndUpdate;
    end;

    if Assigned(FOnShow) then
      FOnShow(self);
    ShowModal;
    if Assigned(FOnClose) then
      FOnClose(self);
    Free;
  end;
end;
{*******************************************************}
end.

